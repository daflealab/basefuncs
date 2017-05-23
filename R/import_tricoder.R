#' Import tricoder data
#'
#' \code{import_tricoder} imports a directory of
#' csv Tricoder files. Choose whether bodysize or
#' offspring + death data is to be imported and the
#' column headings will be set correctly.
#'
#' @param dataset_dir A string indicating the location
#' of the directory of csv files to import.
#' @param data_type A string indicating the type of data to import.
#' Can be either "size" or "offspring" at the moment.
#' @param ext A string indicating the extension of files to be imported.
#' Defaults to txt.
#'
#' @return A single tibble.
#' @export
#'
#' @examples
#' test_data <- import_tricoder(dataset_dir = "./Bodysize/",
#'                   data_type = "size",
#'                   ext = "txt")

import_tricoder <- function(dataset_dir, data_type, ext = "txt", ...){

  assertthat::assert_that(assertthat::is.dir(dataset_dir),
                          assertthat::is.string(data_type))

  list_of_files <- dir(dataset_dir, pattern = ext, full.names = TRUE)

  if (data_type %in% c("Size", "size", "Bodysize", "bodysize"))
  {
    output <- map_df(list_of_files, ~ read_csv(.x,
                                               na = c("", "NA"),
                                               col_names = c("ID", "Date", "Mag", "EPU", "Looks_infected", "Eggs"),
                                               col_types = list(ID = col_character(),
                                                                Date = col_date("%d%m%y%H%M"),
                                                                Mag = col_integer(),
                                                                EPU = col_integer(),
                                                                Looks_infected = col_factor(c("YES", "NO")),
                                                                Eggs = col_factor(c("EGGS", "NONE"))),
                                               comment = "#")) %>%
      filter(!is.na(ID))

  } else if (data_type %in% c("Offspring", "offspring"))
  {
    output <- map_df(list_of_files, ~ read_csv(.x,
                                               na = c("", "NA"),
                                               col_names = c("ID", "Date", "Sex", "Clutch", "Offspring", "Alive_Dead"),
                                               col_types = list(ID = col_character(),
                                                                Date = col_date("%d%m%y%H%M"),
                                                                Sex = col_factor(c("MALE", "FEMALE")),
                                                                Clutch = col_integer(),
                                                                Offspring = col_integer(),
                                                                Alive_Dead = col_factor(c("ALIVE", "DEAD"))),
                                               comment = "#")) %>%
      filter(!is.na(ID))

  } else
  {
    stop("Please select data type as either \"Size\" or \"Offspring\"")
  }
}
