#' Imports IDs associated with a SDR run
#'
#' \code{import_sdr_id} imports an excel file linking the
#' location on an SDR to a ID. SDR ID must be a number.
#' Unused SDRs must be removed from the excel file (No
#' blank SDRs). Blank or Empty cells must be named Explicitly.
#'
#' @param file_name A string. The excel file to import. Must contain the
#' DATE in cell B2, BLOCK in B3 and RUN in B4. First 5 rows will be
#'  removed after import.
#' @param sheet_number An integer. The sheet to import from
#' the excel workbook. Indexed by number. Default is the first sheet.
#'
#' @return A tibble with Date, Run, SDR, Cell Details, and Animal IDs.
#' Will convert b or B or Blank or blank to BLANK. Returns an error
#' if duplicate SDR IDs are detected or unnames cells incuded.
#' @export
#'
#' @examples
#' first_run <- import_sdr_ids("./Metab positions 1_17-18NOV.xlsx")

import_sdr_id <- function(file_name, sheet_number = 1){

  test_L <- readxl::read_excel(path = file_name, sheet = sheet_number, range = cell_cols("A:G"),
                               col_names = c("Row_ID", "R1", "R2", "R3", "R4", "R5", "R6"),
                               col_types = "text") %>% slice(-(1:5))

  test_R <- readxl::read_excel(path = file_name, sheet = sheet_number, range = cell_cols("I:O"),
                               col_names = c("Row_ID", "R1", "R2", "R3", "R4", "R5", "R6"),
                               col_types = "text")

  date <- readxl::read_excel(path = file_name, range = "B2",
                             col_names = FALSE, sheet = sheet_number)[[1]]
  block <- readxl::read_excel(path = file_name, range = "B3",
                              col_names = FALSE, sheet = sheet_number)[[1]]
  run <- readxl::read_excel(path = file_name, range = "B4",
                            col_names = FALSE, sheet = sheet_number)[[1]]

  test_LR <- test_L %>% bind_rows(test_R) %>%
    mutate(Date = date, Block = block, Run = run) %>%
    na.omit()

  #Requires that SDR cells contain the text SDR followed by a space or underscore
  SDR_id <- test_LR %>% select(Row_ID) %>%
    filter(!(Row_ID %in% c("A", "B", "C", "D"))) %>%
    unlist(use.names = FALSE)

  if (length(unique(SDR_id)) != length(SDR_id))
  {

    stop("Duplicate SDR IDs in file")

  } else if (nrow(test_LR)/length(SDR_id) != 5)
  {

    stop("Missing values in SDR cells")

  } else
  {

    test_LR_trim <- test_LR %>% filter(Row_ID %in% c("A", "B", "C", "D")) %>%
      mutate(SDR = rep(SDR_id, each = 4)) %>%
      select(Date, Run, SDR, Row_ID, everything()) %>%
      gather(Column_ID, Indiv_ID, R1:R6) %>%
      mutate(Column_ID = str_sub(Column_ID, -1)) %>%
      unite(col = Cell, Row_ID, Column_ID, remove = FALSE, sep = "") %>%
      unite(location_ID, SDR, Cell, sep = "_", remove = FALSE) %>%
      mutate(Indiv_ID = ifelse(Indiv_ID %in% c("B", "b", "Blank", "blank"), "BLANK", Indiv_ID)) %>%
      mutate(Date = lubridate::as_date(Date)) %>%
      select(Date, Block, Run, location_ID, SDR, Run, Cell, everything())

    return(test_LR_trim)

  }
}
