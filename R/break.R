#' Create the broken object for lm models
#'
#' @param model a lm model
#' @param new_observation a new observation with collumns that correspnd to variables used in the model
#' @param ... other parameters
#'
#' @return an object of the broken class
#' @export
#' @importFrom stats predict.lm
#'
#' @examples
#' model <- lm(Sepal.Length~., data=iris)
#' new_observation <- iris[1,]
#' br <- break(model, new_observation)
#' plot(br)

breakDown.lm <- function(model, new_observation, ...) {
  model <- lm(Sepal.Length~., data=iris)

  ny <- predict.lm(model, newdata = new_observation, type="terms")

  broken <- data.frame(variable = paste(colnames(ny),  "=",
                                        sapply(new_observation[colnames(ny)], as.character)),
                       contribution = c(ny))
  broken_sorted <- broken[order(-abs(broken$contribution)),]
  broken_intercept <- rbind(
    data.frame(variable = "(Intercept)",
               contribution = attributes(ny)$constant),
    broken_sorted)

  create(broken_intercept)
}

breakDown.xgboost <- function() {

}

