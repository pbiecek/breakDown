#' Create the broken object
#'
#' @param model a model
#' @param new_observation a new observation with collumns that corresponds to variables used in the model
#' @param ... other parameters
#'
#' @return
#' @export
#'
#' @examples
broken <- function(model, new_observation, ...) {
  UseMethod("broken")
}

#' Create the broken object for lm models
#'
#' @param model a lm model
#' @param new_observation a new observation with collumns that corresponds to variables used in the model
#' @param ... other parameters
#'
#' @return an object of the broken class
#' @export
#' @importFrom stats predict.lm
#'
#' @examples
#' model <- lm(Sepal.Length~., data=iris)
#' new_observation <- iris[1,]
#' br <- broken(model, new_observation)
#' plot(br)

broken.lm <- function(model, new_observation, ...) {
  model <- lm(Sepal.Length~., data=iris)

  ny <- predict.lm(model, newdata = new_observation, type="terms")

  broken_obj <- data.frame(variable = paste(colnames(ny),  "=",
                                        sapply(new_observation[colnames(ny)], as.character)),
                       contribution = c(ny))
  broken_sorted <- broken_obj[order(-abs(broken_obj$contribution)),]
  broken_intercept <- rbind(
    data.frame(variable = "(Intercept)",
               contribution = attributes(ny)$constant),
    broken_sorted)

  create.broken(broken_intercept)
}

broken.xgboost <- function() {

}

