#' Plot Coef
#' @param x the coefficients
#'
#' @export
Gg_plot_coef <- function(x, zeroAsNa = TRUE){
    if (zeroAsNa)
      x <- rjd3filters:::trailingZeroAsNa(x)
    data = data.frame(x)
    data$date <- factor(rownames(data), levels = rownames(data),ordered = TRUE)
    dataGraph <- reshape2::melt(data)

    ggplot(data = dataGraph, aes(x = date, y = value, group = variable,
                                 colour = variable)) +
      geom_line(size = 0.7) +
      geom_point(size = 1) +
      theme(panel.background = element_rect(fill = "white", colour = NA),
            panel.border = element_rect(fill = NA, colour = "grey20"),
            panel.grid.major = element_line(colour = "grey92"),
            panel.grid.minor = element_line(colour = "grey92",
                                            size = 0.25),
            strip.background = element_rect(fill = "grey85", colour = "grey20"),
            complete = TRUE, plot.title = element_text(hjust = 0.5),
            legend.title=element_blank()) +
      labs(x = NULL, y = "Coefficients")
  }
