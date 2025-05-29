#' @name PlotDoseResponse
#'
#' @title Plot dose-response from \code{DoseResponse} objects
#'
#' @description
#' This is the generic \code{plot} S3 method for the \code{DoseResponse}
#' class. It plots the survival probability as a function of concentration at a given
#' target time.
#'
#' - For \code{BinaryData} object: The function plots the observed values of
#' the survival probability at a given time point
#' as a function of concentration. The 95% binomial confidence interval is added
#' to each survival probability. It is calculated using function
#' \code{\link[stats]{binom.test}} from package \code{stats}.
#' Replicates are systematically pooled in this plot.
#'
#' - For \code{CountData} object: The function plots the observed values of
#' the Number of   at a given time point
#' as a function of concentration. The 95% binomial confidence interval is added
#' to each survival probability. It is calculated using function
#' \code{poisson.test}.
#' Replicates are systematically pooled in this plot.
#'
#' - For \code{ContinuousData} object: the function plots observed values of the
#' response at a given time point as a function of concentration. The 95% binomial
#' confidence interval is added
#' to each set of data at each concentration. It is calculated using function
#' \code{\link[stats]{t.test}} from package \code{stats}.
#'
#'
#' @param x an object of class \code{BinaryData}, \code{CountData} or
#' \code{ContinuousData}
#' @param xlab a label for the \eqn{X}-axis, by default \code{Dose}
#' @param ylab a label for the \eqn{Y}-axis, by default \code{Response}
#' @param main main title for the plot
#' @param log.scale if \code{TRUE}, displays \eqn{X}-axis in log-scale
#' @param addlegend if \code{TRUE}, adds a default legend to the plot
#' @param dodge.width dodging width. Dodging preserves the vertical position
#' of an geom while adjusting the horizontal position.
#' @param \dots Further arguments to be passed to generic methods
#'
#' @keywords plot
#'
#' @return a plot of class \code{ggplot}
#'
#' @export
plot.DoseResponse <- function(x,
                              xlab = "Dose",
                              ylab = NULL,
                              main = NULL,
                              log.scale = FALSE,
                              addlegend = TRUE,
                              dodge.width = 0,
                              ...) {

    x$legend_observation <- "Observed values"
    x$legend_binomial <- "Confidence intervals"

    if (log.scale) {
        x <- x[x$conc != 0, ]
        message("When using log-scale, data with concentration at 0 are removed")
    }
    appender <- function(x){
        paste0("time: ", x)
    }

    fd <- ggplot(data = x) +
        theme_minimal() +
        geom_point(
            aes(x = conc, y = response, fill = legend_observation),
            col = "black", position = position_dodge(width = dodge.width)) +
        geom_segment(
            aes(x = conc, xend = conc, y = qinf95, yend = qsup95,
                linetype = legend_binomial), col = "black",
            position = position_dodge(width = dodge.width)) +
        scale_fill_hue("") +
        scale_linetype(name = "") +
        expand_limits(x = 0, y = 0) +
        ggtitle(main) +
        labs(x = xlab, y = ylab) +
        facet_wrap(vars(time), strip.position = "top", labeller = as_labeller(appender))


    if (log.scale) {
        fd <- fd + scale_x_log10()
    }
    if (!addlegend) {
        fd <- fd + theme(legend.position = "none")
    }
    return(fd)
}

#' @name PlotDoseResponse
#' @export
plot.BinaryDoseResponse <- function(x,
                                  xlab = "Concentration",
                                  ylab = NULL,
                                  main = NULL,
                                  log.scale = FALSE,
                                  addlegend = TRUE,
                                  dodge.width = 0,
                                  ...){
    if (is.null(ylab)) { ylab <- "Probability" }
    plot.DoseResponse(x, xlab, ylab, main, log.scale, addlegend, ...)
}
#' @name PlotDoseResponse
#' @export
plot.CountDoseResponse <- function(x,
                                  xlab = "Concentration",
                                  ylab = NULL,
                                  main = NULL,
                                  log.scale = FALSE,
                                  addlegend = TRUE,
                                  dodge.width = 0,
                                  ...){
    if (is.null(ylab)) { ylab <- "Cumulated Count" }
    plot.DoseResponse(x, xlab, ylab, main, log.scale, addlegend, ...)
}

#' @name PlotDoseResponse
#' @export
plot.ContinuousDoseResponse <- function(x,
                                   xlab = "Concentration",
                                   ylab = NULL,
                                   main = NULL,
                                   log.scale = FALSE,
                                   addlegend = TRUE,
                                   dodge.width = 0,
                                   ...){
    if (is.null(ylab)) { ylab <- "Measure" }
    plot.DoseResponse(x, xlab, ylab, main, log.scale, addlegend, ...)
}
