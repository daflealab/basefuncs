#' Plot LoLinR slopes for every SDR in a run
#'
#' \code{plot_metab_blanks} takes the nested dataframe of LoLinR
#' results and plots the slope of each cell of an entire run.
#' PLots are split by SDR and have Blank cells highlighted.
#'
#' @param metab_df A Tibble of metab data in nested format.
#' LoLinR must have  already been applied using \code{calc_metab}.
#'
#' @return A plot for each of SDR showing the slope estimates
#' for each SDR in a run. Blanks cells are highlighted.
#' @export
#'
#' @examples
#' plot_metab_blanks(test_data_merged)

plot_sdr_blanks <- function(metab_df){

  p <- metab_df %>% mutate(Blank = ifelse(Indiv_ID == "BLANK", "Y", "N")) %>%
    ggplot(aes(x = 1, y = slope, fill = Blank)) + facet_grid(. ~ SDR) +
    geom_dotplot(binaxis = "y", stackdir = "center", dotsize = 0.8, binwidth = 0.01) +
    theme_bw() + ggtitle("Variation in slopes for BLANK cells for each SDR") +
    theme(plot.title = element_text(hjust = 0.5)) +
    theme(axis.ticks = element_blank(), axis.text.x = element_blank(), axis.title = element_blank())

  return(p)
}
