#' @name Summary
#'
#' @title Summary of \code{FitTT} object
#'
#' @description
#' This is the generic \code{summary} S3 method for the \code{FitTT} class.
#' It shows the quantiles of priors and posteriors on parameters and the quantiles
#' of the posteriors on the ECx and LCx estimates.
#'
#' @param object an object of class \code{FitTT}
#' @param quiet when \code{TRUE}, does not print
#' @param \dots Further arguments to be passed to generic methods
#'
#' @return The function returns a list with the following information:
#' \item{Qpriors}{quantiles of the model priors}
#' \item{Qposteriors}{quantiles of the model posteriors}
#' \item{QLCx}{quantiles of LCx estimates}
#'
#'
#' @importFrom stats qnorm qunif
#'
NULL
