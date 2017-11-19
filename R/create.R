#' Clean the object of the broken class
#' Internatl function
#'
#' @return enriched broken class
#'
create.broken <- function(broken_intercept) {
  broken_cumm <- data.frame(broken_intercept,
                            cummulative = cumsum(as.numeric(broken_intercept$contribution)),
                            sign = factor(sign(as.numeric(broken_intercept$contribution)), levels = c(-1, 0, 1)),
                            position = seq_along(broken_intercept$variable))
  broken_cumm <- rbind(broken_cumm,
                       data.frame(variable = "final_prognosis",
                                  contribution = sum(broken_cumm$contribution),
                                  cummulative = sum(broken_cumm$contribution),
                                  sign = "X",
                                  position = max(broken_cumm$position)+1))
 class(broken_cumm) = "broken"
 broken_cumm
}
