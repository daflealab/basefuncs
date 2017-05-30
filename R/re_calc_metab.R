#' Runs LoLinR over whole SDR arrays and runs
#'
#' \code{re_calc_metab} takes the LoLinR method of
#' estimating the best fitting change in oxygen consumption
#' but applies this to a nested dataframe of many SDR plate runs.
#' Time is converted from min to hours automatically.
#'
#' @param nested_df The nested df imported using import_metab().
#' Time must still be in minutes (e.g. Time_min)
#' @param sdr_id A string. Identify of SDR plate.
#' @param cell_id A string. Identify of Cell to change.
#' @param min_time An integer. Use to trim data and recalc slopes
#' @param max_time An integer. Use to trim data and recalc slopes
#' @param set_slope_na Use to set slope for ID to missing
#' @param metab_method A string. Either "pc" or "eq".
#' "pc"" is the suggested method by Colin for oxygen consumption data.
#' It is the default.
#' @param alpha_value Now defaults at 0.8.
#'
#' @return A nested tibble with the model, summary and slopes (per hour change) as list columns.
#' @export
#'
#' @examples
#' test_data_merged <- calc_metab(test_data_merged)

re_calc_metab <- function(nested_df, sdr_id, cell_id, min_time, max_time, set_slope_na = FALSE,
                          metab_method = "pc", alpha_value = 0.8) {

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
  id_df <- nested_df %>% filter(SDR == sdr_id, Cell == cell_id)

  if (set_slope_na == FALSE){

    id_df <- id_df %>%
      select(-model, -summary, -slope) %>%
      mutate(data = map(data, filter_data)) %>%
      mutate(model = map(data, metab_model)) %>%
      mutate(summary = map(model, summary)) %>%
      mutate(slope = map_dbl(summary, get_beta))

    nested_df <- nested_df %>% filter(!(SDR == sdr_id & Cell == cell_id)) %>%
      bind_rows(id_df) %>% arrange(SDR, Column_ID, Row_ID)

    return(nested_df)

  } else {

    id_df$slope <- NA
    nested_df <- nested_df %>% filter(!(SDR == sdr_id & Cell == cell_id)) %>%
      bind_rows(id_df) %>% arrange(SDR, Column_ID, Row_ID)
  }
}
