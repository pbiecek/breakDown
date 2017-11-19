#' Break Down PLot
#'
#' @param x the model model of 'broken' class
#' @param trans transformation that shal be applied to scores
#' @param ... other parameters
#'
#' @return a ggplot2 object
#' @import ggplot2
#'
#' @export
plot.broken <- function(x, trans = I, ...) {
  broken_cumm <- x
  constant <- trans(0)
  broken_cumm$prev <- trans(broken_cumm$cummulative - broken_cumm$contribution)
  broken_cumm$cummulative <- trans(broken_cumm$cummulative)
  class(broken_cumm) = "data.frame"
  ggplot(broken_cumm, aes(x = position + 0.5,
                          y = pmax(cummulative, prev),
                          xmin = position, xmax=position + 0.95,
                          ymin = cummulative, ymax = prev,
                          fill = sign,
                          label = sapply(contribution, function(tmp) as.character(signif(tmp, 2))))) +
    geom_rect(alpha=0.9) +
    geom_hline(yintercept = constant) +
    geom_text(nudge_y = 0.1, vjust = 0.5, hjust=0) +
    scale_y_continuous(expand = c(0.1,0.1), name="") +
    scale_x_continuous(labels = broken_cumm$variable, breaks = broken_cumm$position+0.5, name="") +
    scale_fill_manual(values = c("-1" = "#d8b365", "0" = "#f5f5f5", "1" = "#5ab4ac", "X" = "darkgrey")) +
    coord_flip() +
    theme_light() + theme(legend.position = "none", panel.border = element_blank())
}
