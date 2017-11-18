#' Break Down Print
#'
#' @param x the model model of 'broken' class
#' @param ... other parameters
#'
#' @return a data frame
#'
#' @examples
#' @export
print.broken <- function(x, ...) {
  broken_cumm <- x
  class(broken_cumm) = "data.frame"
  broken_cumm$contribution <- signif(broken_cumm$contribution, 2)
  print(broken_cumm[,c("variable", "contribution")])
}
