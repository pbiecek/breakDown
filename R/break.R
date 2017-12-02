#' Create the broken object
#'
#' @param model a model
#' @param new_observation a new observation with collumns that corresponds to variables used in the model
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
#' @param new_observation a new observation with collumns that corresponds to variables used in the model
#' @param ... other parameters
#' @param baseline the orgin/baseline for the breakDown plots, where the rectangles start. It may be a number or a character "Intercept". In the latter case the orgin will be set to model intercept.
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

broken.lm <- function(model, new_observation, ..., baseline = 0) {
  ny <- predict.lm(model, newdata = new_observation, type="terms")

  broken_obj <- data.frame(variable = paste(colnames(ny),  "=",
                                        sapply(new_observation[colnames(ny)], as.character)),
                       contribution = c(ny))
  broken_sorted <- broken_obj[order(-abs(broken_obj$contribution)),]

  # set the baseline to the model (Intercept)
  if (tolower(baseline) == "intercept") {
    baseline <- attributes(ny)$constant
  } else {
    broken_sorted <- rbind(
      data.frame(variable = "(Intercept)",
                 contribution = attributes(ny)$constant),
      broken_sorted)
  }

  create.broken(broken_sorted, baseline)
}

#' Create the broken object for glm models
#'
#' @param model a glm model
#' @param new_observation a new observation with collumns that corresponds to variables used in the model
#' @param ... other parameters
#' @param baseline the orgin/baseline for the breakDown plots, where the rectangles start. It may be a number or a character "Intercept". In the latter case the orgin will be set to model intercept.
#'
#' @return an object of the broken class
#' @importFrom stats predict.lm
#'
#' @examples
#' url <- 'https://archive.ics.uci.edu/ml/machine-learning-databases/wine-quality/winequality-white.csv'
#' wine <- read.table(url, header = T, sep=";")
#' wine$qualityb <- factor(wine$quality > 5.5, labels = c("bad", "good"))
#' modelg <- glm(qualityb~fixed.acidity + volatile.acidity + citric.acid + residual.sugar + chlorides + free.sulfur.dioxide + total.sulfur.dioxide + density + pH + sulphates + alcohol,
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
#' @export

broken.glm <- function(model, new_observation, ..., baseline = 0) {
  ny <- predict.glm(model, newdata = new_observation, type="terms")

  broken_obj <- data.frame(variable = paste(colnames(ny),  "=",
                                            sapply(new_observation[colnames(ny)], as.character)),
                           contribution = c(ny))
  broken_sorted <- broken_obj[order(-abs(broken_obj$contribution)),]

    # set the baseline to the model (Intercept)
  if (tolower(baseline) == "intercept") {
    baseline <- attributes(ny)$constant
  } else {
    broken_sorted <- rbind(
      data.frame(variable = "(Intercept)",
                 contribution = attributes(ny)$constant),
      broken_sorted)
  }

  create.broken(broken_sorted, baseline)
}


#' Create the broken object for ranger models
#'
#' @param model a ranger model
#' @param new_observation a new observation with collumns that corresponds to variables used in the model
#' @param ... other parameters
#' @param baseline the orgin/baseline for the breakDown plots, where the rectangles start. It may be a number or a character "Intercept". In the latter case the orgin will be set to model intercept.
#'
#' @return an object of the broken class
#'
#' @examples
#' library(ranger)
#' model <- ranger(factor(left) ~ ., data = HR_data, importance = 'impurity')
#' importance(model)
#' new_observation <- HR_data[89,]
#' # here
#' # put
#' # examples
#'
#' @export

broken.ranger <- function(model, new_observation, ..., baseline = 0) {

  yhat <- ranger:::predict.ranger(model, new_observation, predict.all=TRUE)
  terminalNodes <- ranger:::predict.ranger(model, new_observation, type="terminalNodes")$predictions
  varNames <- model$forest$independent.variable.names
  # find variables on the path
  selectedVarsForAllTrees <- lapply(seq_along(terminalNodes), function(i){
    nodesLeft <- model$forest$child.nodeIDs[[i]][[1]]
    nodesRight <- model$forest$child.nodeIDs[[i]][[2]]
    nonZeroLeft <- which(nodesLeft > 0)
    nonZeroRight <- which(nodesRight > 0)
    parents <- numeric(length(nodesLeft))
    parents[nodesLeft[nonZeroLeft] + 1] <- nonZeroLeft
    parents[nodesRight[nonZeroRight] + 1] <- nonZeroRight

    startSearch <- terminalNodes[i] + 1
    allVars <- model$forest$split.varIDs[[i]]
    selectedVars <- numeric(length(varNames))
    while (parents[startSearch] > 0) {
      selectedVars[allVars[parents[startSearch]]] <- 1
      startSearch <- parents[startSearch]
    }
    selectedVars/sum(selectedVars)
  })

  varImportance <- matrix(0, nrow = 2, ncol = length(varNames))
  for (j in seq_along(yhat$predictions[1,])) {
    wclass <- yhat$predictions[1,j]
    varImportance[wclass,] <- varImportance[wclass,] + selectedVarsForAllTrees[[j]]
  }
  colnames(varImportance) <- varNames


  broken_obj <- data.frame(variable = paste(varNames,  "=",
                                            sapply(new_observation[varNames], as.character)),
                           contribution = (varImportance[1,] - varImportance[2,])/(2*sum(varImportance)))
  broken_sorted <- broken_obj[order(-abs(broken_obj$contribution)),]

  baseline <- 0.5

  create.broken(broken_sorted, baseline)
}

