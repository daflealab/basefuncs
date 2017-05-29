#' Runs LoLinR over whole SDR arrays and runs
#'
#' \code{re_calc_metab} takes the LoLinR method of
#' estimating the best fitting change in oxygen consumption
#' but applies this to a nested dataframe of many SDR plate runs.
#' Time is converted from min to hours automatically.
#'
#' @param nested_df The nested df imported using import_metab().
#' Time must still be in minutes (e.g. Time_min)
#' @param metab_method A string. Either "pc" or "eq".
#' "pc"" is the suggested method by Colin for oxygen consumption data.
#' It is the default.
#' @param alpha_value Default at 0.2.
#' @param ID A string. The ID of the indvidual to edit
#' @param min_time An integer. Use to trim data and recalc slopes
#' @param max_time An integer. Use to trim data and recalc slopes
#' @param set_slope_zero Use to set slope for ID to missing
#'
#' @return A nested tibble with the model, summary and slopes (per hour change) as list columns.
#' @export
#'
#' @examples
#' test_data_merged <- calc_metab(test_data_merged)

re_calc_metab <- function(nested_df, SDR, cell, min_time, max_time,
                          metab_method = "pc", alpha_value = 0.8,
                          set_slope_zero = FALSE) {

  filter_data <- function(data) {
    #Function to filter original dataset
    data <- data %>% filter(Time_min > min_time, Time_min < max_time)
    return(data)
  }

  metab_model <- function(df) {
    #Function to run the oxygen comsumption models
    LoLinR::rankLocReg(xall = (df$Time_min/60), yall = df$V02, alpha = alpha_value, method = metab_method, verbose = FALSE)
  }

  get_beta <- function(summary_table) {
    #Function to extract the slope of the best fitting mode
    return(summary_table$summaryTable$b1[1])
  }

  #Code to iterate over all SDRs
  id_df <- nested_df %>% filter(SDR == SDR, Cell == cell)

  assertthat::assert_that(nrow(id_df) == 1,
                          msg = "More than one unique individual returned per ID. Check IDs")

  if (set_slope_zero == FALSE){

    id_df <- id_df %>%
      select(-model, -summary, -slope) %>%
      mutate(data = map(data, filter_data)) %>%
      mutate(model = map(data, metab_model)) %>%
      mutate(summary = map(model, summary)) %>%
      mutate(slope = map_dbl(summary, get_beta))

    nested_df <- nested_df %>% filter(SDR != SDR, Cell != cell) %>%
      bind_rows(id_df) %>% arrange(SDR, Column_ID, Row_ID)

    return(nested_df)

  } else {

    id_df$slope <- NA
    nested_df <- nested_df %>% filter(!(Indiv_ID %in% ID)) %>%
      bind_rows(id_df) %>% arrange(SDR, Column_ID, Row_ID)
  }
}
