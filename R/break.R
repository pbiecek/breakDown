#' Create the broken object
#'
#' @param model a model
#' @param new_observation a new observation with columns that corresponds to variables used in the model
#' @param ... other parameters
#'
#' @export
#'
broken <- function(model, new_observation, ...) {
  UseMethod("broken")
}

#' Create the broken object for lm models
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

broken.lm <- function(model, new_observation, ..., baseline = 0, predict.function = predict) {
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

#' Create the broken object for glm models
#'
#' @param model a glm model
#' @param new_observation a new observation with columns that corresponds to variables used in the model
#' @param ... other parameters
#' @param baseline the origin/baseline for the breakDown plots, where the rectangles start. It may be a number or a character "Intercept". In the latter case the orgin will be set to model intercept.
#' @param predict.function function that will calculate predictions out of model (typically \code{predict} or \code{betas})
#'
#' @return an object of the broken class
#' @importFrom stats predict.lm
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

broken.glm <- function(model, new_observation, ..., baseline = 0, predict.function = predict) {
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


#' Create the model agnostic broken object
#'
#' @param model a ranger model
#' @param new_observation a new observation with columns that corresponds to variables used in the model
#' @param data the original data used for model fitting, should have same collumns as the 'new_observation'.
#' @param direction either 'up' or 'down' determined the exploration strategy
#' @param ... other parameters
#' @param baseline the orgin/baseline for the breakDown plots, where the rectangles start. It may be a number or a character "Intercept". In the latter case the orgin will be set to model intercept.
#' @param predict.function function that will calculate predictions out of model. It shall return a single numeric value per observation. For classification it may be a probability of the default class.
#'
#' @return an object of the broken class
#'
#' @examples
#' library(breakDown)
#' library(randomForest)
#' library(ggplot2)
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
#' @export

broken.default <- function(model, new_observation, data, direction = "up", ..., baseline = 0,
                          predict.function = predict) {
  # just in case only some variables are specified
  # this will work only for data.frames
  if ("data.frame" %in% class(data)) {
    common_variables <- intersect(colnames(new_observation), colnames(data))
    new_observation <- new_observation[,common_variables, drop = FALSE]
    data <- data[,common_variables, drop = FALSE]
  }

  if (direction == "up") {
    broken_sorted <- broken_go_up(model, new_observation, data,
                                    predict.function, ...)
  } else {
    broken_sorted <- broken_go_down(model, new_observation, data,
                                    predict.function, ...)
  }

  if (tolower(baseline) == "intercept") {
    baseline <- mean(predict.function(model, data, ...))
    broken_sorted <- rbind(
      data.frame(variable = "(Intercept)",
                 contribution = 0,
                 variable_name = "Intercept",
                 variable_value = 1),
      broken_sorted)
  } else {
    broken_sorted <- rbind(
      data.frame(variable = "(Intercept)",
                 contribution = mean(predict.function(model, data, ...)) - baseline,
                 variable_name = "Intercept",
                 variable_value = 1),
      broken_sorted)
  }

  create.broken(broken_sorted, baseline)
}

broken_go_up <- function(model, new_observation, data,
                           predict.function = predict, ...) {
  # set target distribution
  new_data <- new_observation[rep(1L, nrow(data)),]

  # set target
  target_yhat <- predict.function(model, new_observation, ...)
  baseline_yhat <- mean(predict.function(model, data, ...))

  # set variable indicators
  open_variables <- 1:ncol(data)

  important_variables <- c()
  important_yhats <- list()

  for (i in 1:ncol(data)) {
    yhats <- list()
    yhats_diff <- rep(-Inf, ncol(data))
    for (tmp_variable in open_variables) {
      current_data <- data
      current_data[,tmp_variable] <- new_data[,tmp_variable]
      yhats[[tmp_variable]] <- predict.function(model, current_data, ...)
      yhats_diff[tmp_variable] <- abs(baseline_yhat - mean(yhats[[tmp_variable]]))
    }
    important_variables[i] <- which.max(yhats_diff)
    important_yhats[[i]] <- yhats[[which.max(yhats_diff)]]
    data[, important_variables[i]] <- new_data[, important_variables[i]]
    open_variables <- setdiff(open_variables, which.max(yhats_diff))
  }

  varNames <- colnames(data)[important_variables]
  varValues <- sapply(new_observation[,important_variables], as.character)
  contributions <- diff(c(baseline_yhat, sapply(important_yhats, mean)))

  broken_sorted <- data.frame(variable = paste("+", varNames,  "=", varValues),
                              contribution = contributions,
                              variable_name = varNames,
                              variable_value = varValues)

  broken_sorted
}

broken_go_down <- function(model, new_observation, data,
                           predict.function = predict, ...) {
  # set target distribution
  new_data <- new_observation[rep(1L, nrow(data)),]

  # set target
  target_yhat <- predict.function(model, new_observation, ...)
  baseline_yhat <- mean(predict.function(model, data, ...))

  # set variable indicators
  open_variables <- 1:ncol(data)

  important_variables <- c()
  important_yhats <- list()

  for (i in 1:ncol(data)) {
    yhats <- list()
    yhats_diff <- rep(Inf, ncol(data))
    for (tmp_variable in open_variables) {
      current_data <- new_data
      current_data[,tmp_variable] <- data[,tmp_variable]
      yhats[[tmp_variable]] <- predict.function(model, current_data, ...)
      yhats_diff[tmp_variable] <- abs(target_yhat - mean(yhats[[tmp_variable]]))
    }
    important_variables[i] <- which.min(yhats_diff)
    important_yhats[[i]] <- yhats[[which.min(yhats_diff)]]
    new_data[, important_variables[i]] <- data[, important_variables[i]]
    open_variables <- setdiff(open_variables, which.min(yhats_diff))
  }

  varNames <- rev(colnames(data)[important_variables])
  varValues <- sapply(rev(new_observation[,important_variables]), as.character)
  contributions <- diff(c(baseline_yhat, rev(sapply(important_yhats, mean))))

  broken_sorted <- data.frame(variable = c(paste("-", varNames,  "=", varValues)),
                              contribution = contributions,
                              variable_name = varNames,
                              variable_value = varValues)

  broken_sorted
}
