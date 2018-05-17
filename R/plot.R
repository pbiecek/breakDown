#' Break Down PLot
#'
#' @param x the model model of 'broken' class
#' @param trans transformation that shal be applied to scores
#' @param ... other parameters
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
#' @export
plot.broken <- function(x, trans = I, ..., add_contributions = TRUE,
                        vcolors = c("-1" = "#d8b365", "0" = "#f5f5f5", "1" = "#5ab4ac", "X" = "darkgrey"),
                        digits = 3, rounding_function = round, plot_distributions = FALSE) {
  position <- cummulative <- prev <- trans_contribution <- prediction <- label <- id <- NULL

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
    class(broken_cumm) = "data.frame"
    broken_cumm$trans_contribution <- broken_cumm$cummulative - broken_cumm$prev
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
