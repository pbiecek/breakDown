#' Model Agnostic Experimental Approach to Break Down Plots with Interactions
#'
#' This function implements decomposition of model predictions with identification
#' of interactions.
#' The complexity of this function is O(2*p) for additive models and O(2*p^2) for interactions
#'
#' @param model a model, it can be any predictive model, find examples for most popular frameworks in vigniettes
#' @param new_observation a new observation with columns that corresponds to variables used in the model
#' @param data the original data used for model fitting, should have same collumns as the 'new_observation'.
#' @param ... other parameters that will be pased to the predict function
#' @param check_interactions the orgin/baseline for the breakDown plots, where the rectangles start. It may be a number or a character "Intercept". In the latter case the orgin will be set to model intercept.
#' @param predict_function function that will calculate predictions out of model. It shall return a single numeric value per observation. For classification it may be a probability of the default class.
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

break_down <- function(model, new_observation, data,
                       ...,
                       predict_function = predict,
                       check_interactions = TRUE,
                       keep_distributions = FALSE) {
  # just in case only some variables are specified
  # this will work only for data.frames
  if ("data.frame" %in% class(data)) {
    common_variables <- intersect(colnames(new_observation), colnames(data))
    new_observation <- new_observation[,common_variables, drop = FALSE]
    data <- data[,common_variables, drop = FALSE]
  }

  # set target
  target_yhat <- predict_function(model, new_observation, ...)
  baseline_yhat <- mean(predict_function(model, data, ...))

  # 1d changes
  # how the average would change if single variable is changed
  average_yhats <- calculate_1d_changes(model, new_observation, data, predict_function, ...)
  diffs_1d <- average_yhats - baseline_yhat

  # impact summary for 1d variables
  tmp <- data.frame(diff = diffs_1d,
                    adiff = abs(diffs_1d),
                    diff_norm = diffs_1d,
                    adiff_norm = abs(diffs_1d),
                    ind1 = 1:p,
                    ind2 = NA)

  if (check_interactions) {
    inds <- data.frame(ind1 = unlist(lapply(2:p, function(i) i:p)),
                       ind2 = unlist(lapply(2:p, function(i) rep(i - 1, p - i + 1))))

    # 2d changes
    # how the average would change if two variables are changed
    changes <- calculate_2d_changes(model, new_observation, data, predict_function, inds, diffs_1d, ...)

    diffs_2d <- changes$average_yhats - baseline_yhat
    diffs_2d_norm <- changes$average_yhats_norm - baseline_yhat

    # impact summary for 2d variables
    tmp2 <- data.frame(diff = diffs_2d,
                       adiff = abs(diffs_2d),
                       diff_norm = diffs_2d_norm,
                       adiff_norm = abs(diffs_2d_norm),
                       ind1 = inds$ind1,
                       ind2 = inds$ind2)
    tmp <- rbind(tmp, tmp2)
  }

  # sort impacts and look for most importants elements
  tmp <- tmp[order(tmp$adiff_norm, decreasing = TRUE),]

  # Now we know the path, so we can calculate contributions
  # set variable indicators
  open_variables <- 1:ncol(data)
  current_data <- data

  step <- 0
  yhats <- NULL
  yhats_mean <- c()
  selected_rows <- c()
  for (i in 1:nrow(tmp)) {
    candidates <- tmp$ind1[i]
    if (!is.na(tmp$ind2[i]))
      candidates[2] <- tmp$ind2[i]
    if (all(candidates %in% open_variables)) {
      # we can add this effect to out path
      current_data[,candidates] <- new_observation[,candidates]
      step <- step + 1
      yhats_pred <- predict_function(model, current_data, ...)
      if (keep_distributions) {
        yhats[[step]] <- data.frame(variable = colnames(data)[candidates],
                                    label = paste("*",
                                                  paste(colnames(data)[candidates], collapse = ":"),
                                                  "=",
                                                  nice_pair(new_observation, candidates[1], candidates[2] )),
                                    id = 1:nrow(data),
                                    prediction = yhats_pred)
      }
      yhats_mean[step] <- mean(yhats_pred)
      selected_rows[step] <- i
      open_variables <- setdiff(open_variables, candidates)
    }
  }
  selected <- tmp[selected_rows,]

  # extract values
  selected_values <- sapply(1:nrow(selected), function(i) {
    nice_pair(new_observation, selected$ind1[i], selected$ind2[i] )
  })

  # prepare values
  variable_name  <- c("Intercept", rownames(selected), "")
  variable_value <- c("1", selected_values, "")
  variable       <- c("(Intercept)",
                      paste("*", rownames(selected), "=",  selected_values) ,
                      "final_prognosis")
  cummulative <- c(baseline_yhat, yhats_mean, target_yhat)
  contribution <- c(0, diff(cummulative))
  contribution[1] <- cummulative[1]
  contribution[length(contribution)] <- cummulative[length(contribution)]

  result <- data.frame(variable = variable,
                    contribution = contribution,
                    variable_name = variable_name,
                    variable_value = variable_value,
                    cummulative = cummulative,
                    sign = factor(c(as.character(sign(contribution)[-length(contribution)]), "X"), levels = c("-1", "0", "1", "X")),
                    position = 1:(step+2))

  class(result) <- "broken"
  attr(result, "baseline") <- 0
  if (keep_distributions) {
    yhats0 <- data.frame(variable = "all data",
                         label = "all data",
                         id = 1:nrow(data),
                         prediction = predict_function(model, current_data, ...)
    )

    yhats_distribution <- rbind(yhats0, do.call(rbind, yhats))
    attr(result, "yhats_distribution") = yhats_distribution
  }

  result
}


# helper functions
nice_format <- function(x) {
  if (is.numeric(x)) {
    as.character(signif(x, 2))
  } else {
    as.character(x)
  }
}

nice_pair <- function(x, ind1, ind2) {
  if (is.na(ind2)) {
    nice_format(x[1,ind1])
  } else {
    paste(nice_format(x[1,ind1]), nice_format(x[1,ind2]), sep=":")
  }
}

# 1d changes
# how the average would change if single variable is changed
calculate_1d_changes <- function(model, new_observation, data, predict_function, ...) {
  p <- ncol(data)
  average_yhats <- numeric(p)
  for (i in 1:p) {
    current_data <- data
    current_data[,i] <- new_observation[,i]
    yhats <- predict_function(model, current_data, ...)
    average_yhats[i] <- mean(yhats)
  }
  names(average_yhats) <- colnames(data)
  average_yhats
}

# 2d changes
# how the average would change if two variables are changed
calculate_2d_changes <- function(model, new_observation, data, predict_function, inds, diffs_1d, ...) {
  average_yhats <- numeric(nrow(inds))
  average_yhats_norm <- numeric(nrow(inds))
  for (i in 1:nrow(inds)) {
    current_data <- data
    current_data[,inds[i, 1]] <- new_observation[,inds[i, 1]]
    current_data[,inds[i, 2]] <- new_observation[,inds[i, 2]]
    yhats <- predict_function(model, current_data, ...)
    average_yhats[i] <- mean(yhats)
    average_yhats_norm[i] <- mean(yhats) - diffs_1d[inds[i, 1]] - diffs_1d[inds[i, 2]]
  }
  names(average_yhats) <- paste(colnames(data)[inds[,1]],
                                colnames(data)[inds[,2]],
                                sep=":")
  list(average_yhats = average_yhats, average_yhats_norm = average_yhats_norm)
}
