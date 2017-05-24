#' Plot LoLinR slopes for every SDR in a run
#'
#' \code{plot_metab_blanks} takes the nested dataframe of LoLinR
#' results and plots the slope of each cell of an entire run.
#' PLots are split by SDR and have Blank cells highlighted.
#'
#' @param metab_df A Tibble of metab data in nested format.
#' LoLinR must have  already been applied using \code{calc_metab}.
#' @param dotcex An integer. The diameter of the dots relative to binwidth, default 1.
#' @param bin An integer. The maximum binwidth. Default at 0.1.
#'
#' @return A plot for each of SDR showing the slope estimates
#' for each SDR in a run. Blanks cells are highlighted.
#' @export
#'
#' @examples
#' plot_metab_blanks(test_data_merged)

plot_sdr_blanks <- function(metab_df, dotcex = 1, bin = 0.1){

  p <- metab_df %>% mutate(Blank = ifelse(Indiv_ID == "BLANK", "Y", "N")) %>%
    ggplot(aes(x = 1, y = slope, fill = Blank)) + facet_grid(. ~ SDR) +
    geom_dotplot(binaxis = "y", stackdir = "center", dotsize = dotcex, binwidth = 0.1) +
    theme_bw() + ggtitle("Variation in slopes for BLANK cells for each SDR") +
    theme(plot.title = element_text(hjust = 0.5)) +
    theme(axis.ticks = element_blank(), axis.text.x = element_blank(), axis.title = element_blank())

  return(p)
}
