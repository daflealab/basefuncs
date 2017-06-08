#' Format coefficient table
#'
#'\code{format_coef} takes the tibble of coefficients and p.values
#'from a tidy call, and roudns to 3 dp and adds stars as a column.
#'
#' @param df A tibble from a tidy broom call
#' @param ... Extra calls to round.
#'
#' @return A tibble
#' @export
#'
#' @examples
#'
#' all_data_wide %>% filter(Treatment != "CON") %>%
#'   group_by(Host, Treatment) %>%
#'   do(tidy(glm(Inf_rate ~ MRS2, data = ., family = binomial()))) %>%
#'   format_num()

format_coef  <- function(df, ...) {

  df <- df %>%
    mutate_if(is.numeric, round, digits = 3, ...) %>%
    mutate(signif = ifelse(p.value > 0.05, "",
                           ifelse(p.value <= 0.05 & p.value > 0.01, "*",
                                  ifelse(p.value <= 0.01 & p.value > 0.001, "**", "***"))))

  return(df)
}
