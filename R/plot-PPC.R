#' @name PlotPPC
#'
#' @title Plot the Posterior Predictive Chack on an object \code{PPC}
#'
#' @description
#' This is the generic \code{plot} S3 method for the \code{PPC} class.
#' It plots the predicted values with 95 \% credible intervals versus the observed
#' values for \code{FitTT} objects.
#'
#' The coordinates of black points are the observed values of the number of survivors
#' (pooled replicates) for a given concentration (\eqn{X}-axis) and the corresponding
#' predicted values (\eqn{Y}-axis). 95\% prediction intervals are added to each predicted
#' value, colored in green if this interval contains the observed value and in red
#' otherwise.
#' The bisecting line (y = x) is added to the plot in order to see if each
#' prediction interval contains each observed value. As replicates are shifted
#' on the x-axis, this line is represented by steps.
#'
#'
#' @param x an object of class \code{PPC}
#' @param xlab label of the x-axis
#' @param ylab label of the y-axis
#' @param main title of the graphic
#' @param dodge.width dodging width. Dodging preserves the vertical position
#' of an geom while adjusting the horizontal position.
#' See \code{\link[ggplot2]{position_dodge}} for further details.
#' @param \dots Further arguments to be passed to generic methods
#'
#' @return a plot of class \code{\link[ggplot2]{ggplot}}
#'
#' @export
plot.PPC <- function(x,
                     xlab = "Observation",
                     ylab = "Prediction",
                     main = NULL,
                     dodge.width = 0, ...){

    percent_in  = summary(x, quiet = TRUE)$percent_in
    x$color <- factor(x$color, levels = c("TRUE", "FALSE"))

    plt <- ggplot(data = x) +
        theme_minimal() +
        labs(x = xlab, y = ylab, subtitle = paste(percent_in,"% PPC")) +
        theme(legend.position = "none") +
        scale_colour_manual(values = c("green", "red")) +
        geom_abline(slope = 1) +
        geom_linerange(
            aes(x = response, ymin = qinf95, ymax = qsup95, color = color),
            position = position_dodge(width = dodge.width)) +
        geom_point(aes(x = response, y = q50),
                   position = position_dodge(width = dodge.width))
    return(plt)
}
