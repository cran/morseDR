#' @name Predict
#'
#' @title Prediction base on \code{FitTT}
#'
#' @param fit object of class \code{BinaryFitTT}, \code{CountFitTT} or
#' \code{ConitnuousFitTT}.
#' @param display.conc  vector of concentrations values (x axis)
#' @param \dots Further arguments to be passed to generic methods
#'
#' @return A list
#' of 3 elements: 'display.conc' a vector of the display concentration of
#' exposure, 'mcmc': a data.frame of the mcmc and 'quantile' a data.frame
#' of quantiles.
#'
#' @export
predict <- function(fit, ...){
    UseMethod("predict")
}
