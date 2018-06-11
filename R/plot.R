#' Break Down Plot
#'
#' @param x the model model of 'broken' class
#' @param trans transformation that shal be applied to scores
#' @param ... other parameters
#' @param top_features maximal number of variables from model we want to plot
#' @param min_delta minimal stroke value of variables from model we want to plot
#' @param add_contributions shall variable contributions to be added on plot?
#' @param vcolors named vector with colors
#' @param digits number of decimal places (round) or significant digits (signif) to be used.
#' See the \code{rounding_function} argument
#' @param rounding_function function that is to used for rounding numbers.
#' It may be \code{signif()} which keeps a specified number of significant digits.
#' Or the default \code{round()} to have the same precision for all components
#' @param plot_distributions if TRUE then distributions of conditional propotions will be plotted. This requires keep_distributions=TRUE in the broken.default().
#'
#' @return a ggplot2 object
#' @import ggplot2
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
#'
#' model <- lm(quality~., data=wine)
#' new_observation <- wine[1,]
#' br <- broken(model, new_observation)
#' plot(br)
#' plot(br, top_features = 2)
#' plot(br, top_features = 2, min_delta = 0.01)
#'}
#' @export
plot.broken <- function(x, trans = I, ..., top_features = 0, min_delta = 0, add_contributions = TRUE,
                        vcolors = c("-1" = "#d8b365", "0" = "#f5f5f5", "1" = "#5ab4ac", "X" = "darkgrey"),
                        digits = 3, rounding_function = round, plot_distributions = FALSE) {
  position <- cummulative <- prev <- trans_contribution <- prediction <- label <- id <- NULL

  if(top_features > 0){
    if(top_features >= (length(x[["variable"]])-1)){
      message(paste0("We have only ", length(x[["variable"]])-1, " variables in our model."))
    }else{
    contributions <- x[["contribution"]]
    contributions <- contributions[-length(contributions)]
    contributions <- abs(contributions)
    top_values <- contributions[order(contributions, decreasing = T)][1:top_features]
    logical_condition <- abs(x[["contribution"]]) %in% top_values
    logical_condition[length(logical_condition)] <- TRUE #we always want to have the final prognosis
    }
  }

  if(min_delta > 0){
    if((min_delta > max(abs(x[["contribution"]])))){
      message(paste0("The maximum stroke of variables in our model is ", max(abs(x[["contribution"]]))))
    }else{
    logical_condition_2 <- abs(x[["contribution"]]) > min_delta
    logical_condition_2[length(logical_condition_2)] <- TRUE #we always want to have the final prognosis

    }
  }

  if((top_features > 0 & top_features < (length(x[["variable"]])-1)) & (min_delta > 0 & min_delta <= max(abs(x[["contribution"]])))){
    logical_condition <- "&"(logical_condition, logical_condition_2)
  }else if(top_features > 0 & top_features < (length(x[["variable"]])-1)){
    logical_condition <- logical_condition
  }else if((min_delta > 0 & min_delta <= max(abs(x[["contribution"]])))){
    logical_condition <- logical_condition_2
  }

  if((top_features > 0 & top_features < (length(x[["variable"]])-1))|| (min_delta > 0 & min_delta <= max(abs(x[["contribution"]])))){
    list_names <- names(x)
    df <- list()
    for(i in 1:length(x)){
      df[[i]] <- x[[i]][logical_condition]
      names(df)[[i]] <- list_names[i]
    }

    df_rest <- list()

    for(i in 1:length(x)){
      df_rest[[i]] <- x[[i]][!logical_condition]
      names(df_rest)[[i]] <- list_names[i]
    }

    variable_rest <- as.factor(paste0("remaining ", length(df_rest[["variable"]]), " variables"))
    contribution_rest <- sum(df_rest[["contribution"]])
    position_rest <- length(df[["variable"]])
    sign_rest <- sign(contribution_rest)
    cummulative_rest <- df[["cummulative"]][(length(df[["cummulative"]])-1)] + contribution_rest
    constant <- attr(x, "baseline")
    attr(df, "baseline") <- constant
    distribution <- attr(x, "yhats_distribution")
    attr(df, "yhats_distribution") <- distribution

    x<-df

    for(i in seq_along(x[["position"]])){
      x[["position"]][i] <- i
    }

    x[["position"]][length(x[["position"]])] <- (length(x[["position"]])+1)


    levels(x[["variable"]]) <- c(levels(x[["variable"]]), levels(variable_rest))
    x[["variable"]][length(x[["variable"]])+1] <- variable_rest
    x[["contribution"]][length(x[["variable"]])] <- contribution_rest
    x[["sign"]][length(x[["variable"]])] <- sign_rest
    x[["cummulative"]][length(x[["variable"]])] <- cummulative_rest
    x[["position"]][length(x[["variable"]])] <- position_rest
    x[["variable_name"]][length(x[["variable"]])] <- ""
    x[["variable_value"]][length(x[["variable"]])] <- ""
  }


  if (plot_distributions) {
    df <- attr(x, "yhats_distribution")
    if (is.null(df))
      stop("You need to use keep_distributions=TRUE in the broken.default() ")

    pl <- ggplot(df, aes(factor(label), prediction, group=factor(label))) +
      geom_line(aes(group=id), alpha=0.01) +
      geom_violin(scale = "width", adjust=3) +
      stat_summary(fun.y = "mean", colour = "red", size = 4, geom = "point") +
      xlab("") + ylab("")
  } else {
    broken_cumm <- x
    constant <- attr(broken_cumm, "baseline")
    broken_cumm$prev <- trans(constant + broken_cumm$cummulative - broken_cumm$contribution)
    broken_cumm$cummulative <- trans(constant + broken_cumm$cummulative)
    if((min_delta > 0 & min_delta <= max(abs(x[["contribution"]]))) || (top_features > 0 & top_features < (length(x[["variable"]])-1))){
      broken_cumm <- as.data.frame(broken_cumm)
      broken_cumm <- broken_cumm[c(1:(nrow(broken_cumm)-2), nrow(broken_cumm), (nrow(broken_cumm)-1)),]
    }else{
      class(broken_cumm) = "data.frame"
    }

    broken_cumm$trans_contribution <- broken_cumm$cummulative - broken_cumm$prev
    broken_cumm <- droplevels(broken_cumm)
    pl <- ggplot(broken_cumm, aes(x = position + 0.5,
                                  y = pmax(cummulative, prev),
                                  xmin = position, xmax=position + 0.95,
                                  ymin = cummulative, ymax = prev,
                                  fill = sign,
                                  label = sapply(trans_contribution, function(tmp) as.character(rounding_function(tmp, digits))))) +
      geom_errorbarh(data=broken_cumm[-nrow(broken_cumm),],
                     aes(xmax = position,
                         xmin = position + 2,
                         y = cummulative), height=0,
                     lty="F2") +
      geom_rect(alpha = 0.9) +
      geom_hline(yintercept = trans(constant))

    if(add_contributions)
      pl <- pl + geom_text(nudge_y = 0.1, vjust = 0.5, hjust=0)

    pl <- pl +
      scale_y_continuous(expand = c(0.1,0.1), name="") +
      scale_x_continuous(labels = broken_cumm$variable, breaks = broken_cumm$position+0.5, name="") +
      scale_fill_manual(values = vcolors)
  }

   pl + coord_flip() + theme_classic() +
     theme(legend.position = "none", panel.border = element_blank())
}
