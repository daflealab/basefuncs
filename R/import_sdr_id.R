#' Imports IDs associated with a SDR run
#'
#' \code{import_sdr_id} imports an excel file linking the
#' location on an SDR to a ID:
#' The last letter of the Run_ID must be a letter
#' The last three digits of the SDR_ID must be the SDR number
#'
#' @param file_name A string. The excel file to import.
#' @param sheet_number An integer. The sheet to import from
#' the excel workbook. Indexed by number. Default is the first sheet.
#'
#' @return A tibble with Date, Run, SDR, Cell Details, and Animal IDs.
#' Will convert b or B or Blank or blank to BLANK
#' @export
#'
#' @examples
#' first_run <- import_sdr_ids("./Metab positions 1_17-18NOV.xlsx")

import_sdr_id <- function(file_name, sheet_number = 1){

  test_L <- readxl::read_excel(file_name, sheet = sheet_number, range = cell_cols("A:G"),
                       col_names = c("Row_ID", "R1", "R2", "R3", "R4", "R5", "R6")) %>%
    mutate(Date = first(Row_ID)) %>% mutate(Run = first(R1)) %>%
    slice(-1)

  test_R <- readxl::read_excel(file_name, sheet = sheet_number, range = cell_cols("I:O"),
                       col_names = c("Row_ID", "R1", "R2", "R3", "R4", "R5", "R6")) %>%
    mutate(Date = first(Row_ID)) %>% mutate(Run = first(R1)) %>%
    slice(-1)

  test_LR <- test_L %>% bind_rows(test_R)

  #Requires that SDR cells contain the text SDR followed by a space or underscore
  SDR_id <- test_LR %>% select(R1) %>% filter(stringr::str_detect(R1, "SDR")) %>% unlist(use.names = FALSE)

  test_LR_trim <- test_LR %>% filter(!is.na(Row_ID)) %>% filter(!is.na(R1)) %>%
    mutate(SDR = rep(SDR_id, each = 4)) %>%
    select(Date, Run, SDR, Row_ID, everything()) %>%
    gather(Column_ID, Indiv_ID, R1:R6) %>%
    mutate(Column_ID = stringr::str_sub(Column_ID, -1)) %>%
    mutate(SDR = stringr::str_sub(SDR, -3)) %>%
    mutate(Run = stringr::str_sub(Run, -1)) %>%
    unite(col = Cell, Row_ID, Column_ID, remove = FALSE, sep = "") %>%
    unite(location_ID, SDR, Cell, sep = "_", remove = FALSE) %>%
    mutate(Indiv_ID = ifelse(Indiv_ID %in% c("B", "b", "Blank", "blank"), "BLANK", Indiv_ID)) %>%
    select(location_ID, SDR, Run, Cell, everything())

  return(test_LR_trim)
}
