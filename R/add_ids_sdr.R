#' Merges SDR ID and SDR data
#'
#' \code{add_ids_sdr} takes the nested dataframe of raw
#' SDR data and adds to this the IDs associated with each
#' cell
#'
#' @param sdr_df A tibble of metab data in nested format.
#' @param id_df A tibble of ids with location_ID as unique.
#'
#' @return A tibble with Date, Run, SDR, Cell Details and Animal IDs.
#' Blanks are now indicated by BLANK.
#' @export
#'
#' @examples
#' test_data_merged <- merge_sdr_id(sdr_df = test_data, id_df = first_run)

add_ids_sdr <- function(sdr_df, id_df){

  unique_date_id <- unique(sdr_df$Date)
  unique_date_data <- unique(id_df$Date)

  if (unique_date_id == unique_date_data)
  {

    id_df <- id_df %>% select(location_ID, Indiv_ID, Row_ID, Column_ID)

    sdr_df <- sdr_df %>% left_join(id_df, by = "location_ID") %>%
      select(Indiv_ID, SDR, location_ID, Cell, Row_ID, Column_ID, everything())

    return(sdr_df)

  } else
  {
    stop("Date mismatch between SDR IDs and SDR Data detected")
  }
}
