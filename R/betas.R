#' Extract betas values of a model for specific observations
#'
#' @param object a model
#' @param newdata new observation(s) with columns that correspond to variables used in the model
#' @param ... unused additional parameters
#' @author Joseph Larmarange
#' @importFrom stats coef
#' @importFrom stats delete.response
#' @importFrom stats model.matrix
#' @export

betas <- function (object, newdata, ...)
{
  tt <- terms(object)
  Terms <- delete.response(tt)
  mm <- model.matrix(Terms, newdata)
  ass <- attr(mm, "assign")
  tl <- attr(Terms, "term.labels")

  co <- coef(object)
  pred <- co * mm

  ret <- matrix(rep_len(NA, length.out = length(tl) * nrow(newdata)), nrow = nrow(newdata))
  colnames(ret) <- tl
  rownames(ret) <- rownames(ret)

  for (i in 1:length(tl)) {
    ret[, i] <- rowSums(pred[, ass == i, drop = FALSE], na.rm = TRUE)
  }
  attr(ret, "constant") <- rowSums(pred[, ass == 0, drop = FALSE], na.rm = TRUE)

  ret
}
