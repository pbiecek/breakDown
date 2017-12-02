#' Break Down Print
#'
#' @param x the model model of 'broken' class
#' @param ... other parameters
#'
#' @return a data frame
#'
#' @export
print.broken <- function(x, ...) {
  broken_cumm <- x
  class(broken_cumm) = "data.frame"
  broken_cumm$contribution <- signif(broken_cumm$contribution, 2)
  rownames(broken_cumm) <- broken_cumm$variable
  print(broken_cumm[, "contribution", drop=FALSE])
  cat("baseline: ",attr(broken_cumm, "baseline"),"\n")
}
