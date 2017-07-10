#' Runs LoLinR over whole SDR arrays and runs
#'
#' \code{calc_metab} takes the LoLinR method of
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
#'
#' @return A nested tibble with the model, summary and slopes (per hour change) as list columns.
#' @export
#'
#' @examples
#' test_data_merged <- calc_metab(test_data_merged)

calc_metab <- function(nested_df, metab_method = "pc", alpha_value = 0.25) {

  metab_model <- function(df) {
    #Function to run the oxygen comsumption models
    LoLinR::rankLocReg(xall = (df$Time_min/60), yall = df$V02, alpha = alpha_value, method = metab_method, verbose = FALSE)
  }

  get_beta <- function(summary_table) {
    #Function to extract the slope of the best fitting mode
    return(summary_table$summaryTable$b1[1])
  }

  assertthat::assert_that(!("slope" %in% names(nested_df)),
                          msg = paste("Slope variable already exits, please reimport nested_df before running calc_metab() again"))

  #Code to iterate over all SDRs
  nested_df <- nested_df %>%
    mutate(model = map(data, metab_model)) %>%
    mutate(summary = map(model, summary)) %>%
    mutate(slope = map_dbl(summary, get_beta))

  return(nested_df)
}
