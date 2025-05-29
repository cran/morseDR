#' @name PlotFitTT
#'
#' @title Plotting method for \code{FitTT} objects.
#'
#' @description
#' This is the generic \code{plot} S3 method for the \code{FitTT} class. It
#' plots concentration-response fit under target time analysis.
#'
#' The fitted curve represents the
#' \strong{response} at
#' the target time as a function of the concentration of chemical compound;
#' When \code{adddata = TRUE} the black dots depict the observation data
#' at each tested concentration. Note that since our model does not take
#' inter-replicate variability into consideration, replicates are systematically
#' pooled in this plot.
#'
#' The function plots both 95\% credible intervals for the response
#' (by default the grey area around the fitted curve) and 95\% confidence
#' intervals (as black segments if
#' \code{adddata = TRUE}), either binomial, poisson or normal for respectivelly,
#' \code{BinaryFitTT}, \code{CountFitTT} and\code{ContinuousFitTT}.
#' Both types of intervals are taken at the same level. Typically
#' a good fit is expected to display a large overlap between the two intervals.
#'
#' If spaghetti = TRUE, the credible intervals are represented by two dotted
#' lines limiting the credible band, and a spaghetti plot is added to this band.
#' This spaghetti plot consists of the representation of simulated curves using parameter values
#' sampled in the posterior distribution (10\% of the MCMC chains are randomly
#' taken for this sample).
#'
#' @param x an object of class \code{FitTT}
#' @param xlab a label for the \eqn{X}-axis, default is \code{Concentration}
#' @param ylab a label for the \eqn{Y}-axis.
#' For \code{BinaryFitTT} default is \code{Probability};
#' For \code{CountFitTT}, default is \code{Count}, and
#' For \code{ContinuousFitTT}, default is \code{Measure}.
#' @param display.conc Vector of numeric on which the plot is done. Default is
#' \code{NULL} to use the concentration given in the \code{FitTT} object.
#' @param main main title for the plot
#' @param spaghetti if \code{TRUE}, the credible interval is represented by
#' multiple curves
#' @param adddata if \code{TRUE}, adds the observed data with confidence intervals
#' to the plot
#' @param addlegend if \code{TRUE}, adds a default legend to the plot
#' @param log.scale if \code{TRUE}, displays \eqn{X}-axis in log-scale
#' @param \dots Further arguments to be passed to generic methods
#'
#' @return a plot of class  \code{\link[ggplot2]{ggplot}}
#'
#' @keywords plot
#'
#' @import grDevices
#' @import ggplot2
#' @importFrom stats aggregate
#'
NULL
