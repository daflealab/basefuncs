#' Import SDR data files
#'
#' \code{import_sdr_data} imports a directory of
#' SDR output files. Files must be in the xlsx format.
#' SDR IDs will be extracted along with the Run ID and
#' Oxygen consumption data.
#'
#' @param dataset_dir A string indicating the location
#' of the directory of xlsx files to import.
#' @param sheet_number An integer. The sheet number of the
#'  excel workbook that should be imported. Defaults to 1.
#' @param skip_rows An integer. The number of rows of the
#' excel sheet to skip on import. Defaults to 12.
#' @param Trim_time An integer. Initial sampling times
#' to skip. Defaults to 5. No Sensor errors are set to missing (NA).
#' @param ...
#'
#' @return A nested tibble by SDR, with Date, Time, Cell, and Oxygen values
#' @export
#'
#' @examples
#' test_data <- import_metab(dataset_dir = "./friA_18NOV/test/",
#'                            sheet_number = 1, skip_rows = 12,
#'                            Trim_time = 0)

import_sdr_data <- function(dataset_dir, sheet_number = 1, skip_rows = 12, Trim_time = 5, ...){

  stopifnot(length(dataset_dir) > 0)

  list_of_files <- dir(dataset_dir, pattern = "xlsx", full.names = TRUE)

  read_and_assign <- function(dataset){
    # Function to import data set, assign SDR and Run IDs
    # Returns a tidy tibble with Cell and Oxygen values as columns

    sdr_id <-  readxl::read_excel(dataset, range = "B5", col_names = FALSE, sheet = sheet_number)[[1]]
    run_id <-  readxl::read_excel(dataset, range = "B1", col_names = FALSE, sheet = sheet_number)[[1]]

    dataset_named <- readxl::read_excel(dataset, skip = skip_rows, sheet = sheet_number, na = c("No Sensor", "NA")) %>%
      mutate(Run_id = run_id, SDR = stringr::str_sub(sdr_id, -3)) %>%
      select(SDR, Run_id, Date = `Date/Time`, Time_min = `Time/Min.`, A1:D6) %>%
      mutate(Date = lubridate::dmy_hms(Date)) %>%
      mutate(Date = lubridate::as_date(Date)) %>%
      mutate(Day = lubridate::wday(Date, label = TRUE, abbr = FALSE)) %>%
      filter(Time_min > Trim_time) %>%
      gather(key = Cell, value = V02, A1:D6) %>%
      unite(col = location_ID, SDR, Cell, remove = FALSE)

  }

  #Function to generate tibbles nested by SDR and Run
  output <- map_df(list_of_files, read_and_assign) %>%
    group_by(location_ID, SDR, Date, Day, Run_id, Cell) %>% nest()

  return(output)
}
