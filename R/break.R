#' Generic Function for Breaking Down of Model Predictions
#'
#' The \code{broken} function is a generic function for decomposition of model predictions.
#' For linear models please use \link{broken.lm}, for generic linear models please use \link{broken.glm}.
#' For all other models please use the model agnostic version \link{broken.default}.
#' Please note, that some of these functions have additional parameters.
#'
#' @param model a model
#' @param new_observation a new observation with columns that corresponds to variables used in the model
#' @param ... other parameters
#'
#' @export
#'
#' @return an object of the broken class
#'
#' @examples
#' library("breakDown")
#' library("randomForest")
#' library("ggplot2")
#' set.seed(1313)
#' model <- randomForest(factor(left)~., data = HR_data, family = "binomial", maxnodes = 5)
#' predict.function <- function(model, new_observation)
#'       predict(model, new_observation, type="prob")[,2]
#' predict.function(model, HR_data[11,-7])
#' explain_1 <- broken(model, HR_data[11,-7], data = HR_data[,-7],
#' predict.function = predict.function, direction = "down")
#' explain_1
#' plot(explain_1) + ggtitle("breakDown plot (direction=down) for randomForest model")
#'
#' explain_2 <- broken(model, HR_data[11,-7], data = HR_data[,-7],
#' predict.function = predict.function, direction = "down", keep_distributions = TRUE)
#' plot(explain_2, plot_distributions = TRUE) +
#'          ggtitle("breakDown distributions (direction=down) for randomForest model")
#'
#' explain_3 <- broken(model, HR_data[11,-7], data = HR_data[,-7],
#' predict.function = predict.function, direction = "up", keep_distributions = TRUE)
#' plot(explain_3, plot_distributions = TRUE) +
#'          ggtitle("breakDown distributions (direction=up) for randomForest model")
broken <- function(model, new_observation, ...) {
  UseMethod("broken")
}

#' Breaking Down of Model Predictions for lm models
#'
#' @param model a lm model
#' @param new_observation a new observation with columns that corresponds to variables used in the model
#' @param ... other parameters
#' @param baseline the orgin/baseline for the breakDown plots, where the rectangles start. It may be a number or a character "Intercept". In the latter case the orgin will be set to model intercept.
#' @param predict.function function that will calculate predictions out of model (typically \code{predict} or \code{betas})
#'
#' @return an object of the broken class
#' @export
#' @importFrom stats predict
#' @importFrom stats predict.lm
#' @importFrom stats predict.glm
#' @importFrom stats terms
#'
#' @examples
#' model <- lm(Sepal.Length~., data=iris)
#' new_observation <- iris[1,]
#' br <- broken(model, new_observation)
#' plot(br)
#'
#' # works for interactions as well
#' model <- lm(Sepal.Length ~ Petal.Width*Species, data = iris)
#' summary(model)
#'
#' new_observation <- iris[1,]
#' br <- broken(model, new_observation)
#' br
#' plot(br)
#'
#' br2 <- broken(model, new_observation, predict.function = betas)
#' br2
#' plot(br2)

broken.lm <- function(model, new_observation, ..., baseline = 0, predict.function = stats::predict.lm) {
  ny <- predict.function(model, newdata = new_observation, type = "terms")
  terms <- NULL

  # add terms with :
  labels <- attr(terms(model), "term.labels")
  ilabels <- grep(labels, pattern = ":", value = TRUE)
  for (interact in ilabels) {
    vars <- strsplit(interact, split = ":")[[1]]
    new_observation[,interact] <- apply(new_observation[,vars], 1, paste0, collapse=":")
  }

  broken_obj <- data.frame(variable = paste(colnames(ny),  "=",
                                        sapply(new_observation[colnames(ny)], as.character)),
                       contribution = c(ny),
                       variable_name = colnames(ny),
                       variable_value = sapply(new_observation[colnames(ny)], as.character))
  broken_sorted <- broken_obj[order(-abs(broken_obj$contribution)),]

  # set the baseline to the model (Intercept)
  if (tolower(baseline) == "intercept") {
    baseline <- attributes(ny)$constant
  } else {
    broken_sorted <- rbind(
      data.frame(variable = "(Intercept)",
                 contribution = attributes(ny)$constant,
                 variable_name = "Intercept",
                 variable_value = 1),
      broken_sorted)
  }

  create.broken(broken_sorted, baseline)
}

#' Breaking Down of Model Predictions for glm models
#'
#' @param model a glm model
#' @param new_observation a new observation with columns that corresponds to variables used in the model
#' @param ... other parameters
#' @param baseline the origin/baseline for the breakDown plots, where the rectangles start. It may be a number or a character "Intercept". In the latter case the orgin will be set to model intercept.
#' @param predict.function function that will calculate predictions out of model (typically \code{predict} or \code{betas})
#'
#' @return an object of the broken class
#' @importFrom stats predict.lm
#' @importFrom stats predict
#'
#' @examples
#' # example for wine data
#' wine$qualityb <- factor(wine$quality > 5.5, labels = c("bad", "good"))
#' modelg <- glm(qualityb~fixed.acidity + volatile.acidity + citric.acid +
#'               residual.sugar + chlorides + free.sulfur.dioxide +
#'               total.sulfur.dioxide + density + pH + sulphates + alcohol,
#'     data=wine, family = "binomial")
#' new_observation <- wine[1,]
#' br <- broken(modelg, new_observation)
#' logit <- function(x) exp(x)/(1+exp(x))
#' plot(br, logit)
#'
#' # example for HR_data
#' model <- glm(left~., data = HR_data, family = "binomial")
#' explain_1 <- broken(model, HR_data[1,])
#' explain_1
#' plot(explain_1)
#' plot(explain_1, trans = function(x) exp(x)/(1+exp(x)))
#'
#' explain_2 <- broken(model, HR_data[1,], predict.function = betas)
#' explain_2
#' plot(explain_2, trans = function(x) exp(x)/(1+exp(x)))
#' @export

broken.glm <- function(model, new_observation, ..., baseline = 0, predict.function = stats::predict.glm) {
  ny <- predict.function(model, newdata = new_observation, type = "terms")
  terms <- NULL

  # add terms with :
  labels <- attr(terms(model), "term.labels")
  ilabels <- grep(labels, pattern = ":", value = TRUE)
  for (interact in ilabels) {
    vars <- strsplit(interact, split = ":")[[1]]
    new_observation[,interact] <- apply(new_observation[,vars], 1, paste0, collapse = ":")
  }

  broken_obj <- data.frame(variable = paste(colnames(ny),  "=",
                                            sapply(new_observation[colnames(ny)], as.character)),
                           contribution = c(ny),
                           variable_name = colnames(ny),
                           variable_value = sapply(new_observation[colnames(ny)], as.character))
  broken_sorted <- broken_obj[order(-abs(broken_obj$contribution)),]

    # set the baseline to the model (Intercept)
  if (tolower(baseline) == "intercept") {
    baseline <- attributes(ny)$constant
    broken_sorted <- rbind(
      data.frame(variable = "(Intercept)",
                 contribution = 0,
                 variable_name = "Intercept",
                 variable_value = 1),
      broken_sorted)
  } else {
    broken_sorted <- rbind(
      data.frame(variable = "(Intercept)",
                 contribution = attributes(ny)$constant - baseline,
                 variable_name = "Intercept",
                 variable_value = 1),
      broken_sorted)
  }

  create.broken(broken_sorted, baseline)
}
