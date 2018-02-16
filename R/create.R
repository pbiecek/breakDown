#' Clean the object of the broken class
#' Internal function
#' @param broken_intercept list with variable contributions
#' @param baseline level on which the baseline line shall be plotted
#'
#' @return enriched broken class
#'
create.broken <- function(broken_intercept, baseline = 0) {
  broken_cumm <- data.frame(broken_intercept,
                            cummulative = cumsum(as.numeric(broken_intercept$contribution)),
                            sign = factor(sign(as.numeric(broken_intercept$contribution)), levels = c(-1, 0, 1)),
                            position = seq_along(broken_intercept$variable))
  broken_cumm <- rbind(broken_cumm,
                       data.frame(variable = "final_prognosis",
                                  contribution = sum(broken_cumm$contribution),
                                  variable_name = "",
                                  variable_value = "",
                                  cummulative = sum(broken_cumm$contribution),
                                  sign = "X",
                                  position = max(broken_cumm$position)+1))
 attr(broken_cumm, "baseline") <- baseline
 class(broken_cumm) <- "broken"
 broken_cumm
}
