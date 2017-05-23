#' Plot metabolic data and LoLinR results for a single SDR
#'
#' \code{plot_metab_cells} takes the nested dataframe of LoLinR
#' results and plots the best fitting model for each cell of a
#' single SDR. Includes IDs and location information.
#'
#' @param metab_df A Tibble of metab data in nested format.
#' LoLinR must have  already been applied using \code{calc_metab}
#' @param SDR_no An integter. Indexes a given SDR in a single run.
#'
#' @return A 24 panel plot of each of best fitting models for each
#' cell of single SDR
#' @export
#'
#' @examples
#' plot_metab_cells(metab_df = test_data_merged, SDR = 8)

plot_sdr_cells <- function(metab_df, SDR_no){

  assertthat::assert_that(assertthat::is.count(SDR_no))
  assertthat::assert_that(SDR_no <= length(unique(metab_df$SDR)),
                          msg = paste("Requested SDR is greater than the ", length(unique(metab_df$SDR)), " available"))

  SDR_vector = unique(metab_df$SDR)[SDR_no]

  gg_metab <- function(ID, Cell, df, model){
    # Function to generate plot for each SDR cell

    model_L <- model$allRegs$Lbound[1]
    model_R <- model$allRegs$Rbound[1]

    df <- df %>% mutate(limits = ifelse(row_number(Time_min) < model_L | row_number(Time_min) > model_R,
                                        "N", "Y"))

    p <- ggplot(df, aes(x = Time_min, y = V02, colour = limits)) +
      geom_point(shape = 1, size = 2) +
      geom_smooth(data = subset(df, limits == "Y"),
                  method = lm, se = FALSE, na.rm = TRUE) +
      ylim(c(min(c((max(df$V02 - 5)), min(df$V02))),
             max(df$V02))) +
      annotate("text",  x = -Inf, y = -Inf,
               hjust = -0.1, vjust = -1,
               label = paste(Cell, ": ", ID, sep = "")) +
      theme_bw() +
      theme(legend.position = "none", axis.title = element_blank()) +
      scale_colour_manual(values = c("black", "dodgerblue"))

    return(p)
  }

  gg_metab_data <- metab_df %>%
    filter(SDR == SDR_vector) %>%
    mutate(plot = pmap(list(Indiv_ID, Cell, data, model), gg_metab))

  gridExtra::grid.arrange(grobs = gg_metab_data$plot,  ncol = 4,
                          top = paste("Time (min) versus Oxygen change for SDR: ", gg_metab_data$SDR, sep = ""))
}
