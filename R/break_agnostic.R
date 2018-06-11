#' Model Agnostic Approach to Breaking Down of Model Predictions
#'
#' This function implements two greedy strategies for decompositions of model predictions (see the \code{direction} parameter).
#' Both stategies are model agnostic, they are greedy but in most cases they give very similar results.
#' Find more information about these strategies in \url{https://arxiv.org/abs/1804.01955}.
#'
#' @param model a model, it can be any predictive model, find examples for most popular frameworks in vigniettes
#' @param new_observation a new observation with columns that corresponds to variables used in the model
#' @param data the original data used for model fitting, should have same collumns as the 'new_observation'.
#' @param direction either 'up' or 'down' determined the exploration strategy
#' @param ... other parameters
#' @param baseline the orgin/baseline for the breakDown plots, where the rectangles start. It may be a number or a character "Intercept". In the latter case the orgin will be set to model intercept.
#' @param predict.function function that will calculate predictions out of model. It shall return a single numeric value per observation. For classification it may be a probability of the default class.
#' @param keep_distributions if TRUE, then the distribution of partial predictions is stored in addition to the average.
#'
#' @return an object of the broken class
#'
#' @examples
#' \dontrun{
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
#' }
#' @export

broken.default <- function(model, new_observation, data, direction = "up", ..., baseline = 0,
                           keep_distributions = FALSE, predict.function = predict) {
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

  if (keep_distributions) {
    # calcuate distribution for partial predictions
    open_variables <- as.character(broken_sorted$variable_name)
    current_data <- data
    yhats_distribution <- list(data.frame(variable = "all data",
                                          label = "all data",
                                          id = 1:nrow(current_data),
                                          prediction = predict.function(model, current_data, ...)))
    for (i in seq_along(open_variables)) {
      tmp_variable <- open_variables[i]
      current_data[,tmp_variable] <- new_observation[,tmp_variable]
      yhats_distribution[[tmp_variable]] <- data.frame(variable = tmp_variable,
                                                       label = as.character(broken_sorted$variable[i]),
                                                       id = 1:nrow(current_data),
                                                       prediction = predict.function(model, current_data, ...)
      )
    }
    yhats_df <- do.call(rbind, yhats_distribution)
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

  result <- create.broken(broken_sorted, baseline)

  if (keep_distributions) {
    attr(result, "yhats_distribution") <- yhats_df
  }

  result
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
  contributions <- diff(c(rev(sapply(important_yhats, mean)), target_yhat))

  broken_sorted <- data.frame(variable = c(paste("-", varNames,  "=", varValues)),
                              contribution = contributions,
                              variable_name = varNames,
                              variable_value = varValues)

  broken_sorted
}
