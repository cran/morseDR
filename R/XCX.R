#' @name RiskMeasure
#'
#' @title Predict X% Concentration at the target time (default).
#'
#' @description
#' Predict median and 95\% credible interval of the x% Lethal or Effect
#'  Concentration.
#'
#' @param fit An object used to select a method
#' @param x vector of values of LCx or ECX
#' @param \dots Further arguments to be passed to generic methods
#'
#' @return returns an object of class \code{XCX} consisting in a data.frame with
#' the estimated ECx or LCx and their CIs
#' 95% (3 columns (values, CIinf, CIsup) and length(x) rows)
#'
#' @importFrom stats quantile
#'
#' @export
#'
xcx <- function(fit, x, ...){
    UseMethod("xcx")
}

#' @name RiskMeasure
#' @export
#'
xcx.FitTT <- function(fit, x, ...){

    mctot <- do.call("rbind", fit$mcmc)
    b <- 10^mctot[, "log10b"]
    e <- 10^mctot[, "log10e"]

    # Calculation XCx median and quantiles
    XCx <- sapply(x, function(x_) {e * ((100 / (100 - x_)) - 1)^(1 / b)})
    if (is.matrix(XCx)) {
        colnames(XCx) <- paste("x:", x)
    }

    q50 <- apply(XCx, 2, function(x) {quantile(x, probs = 0.5)})
    qinf95 <- apply(XCx, 2, function(x) {quantile(x, probs = 0.025)})
    qsup95 <- apply(XCx, 2, function(x) {quantile(x, probs = 0.975)})

    # create the dataframe with ECx median and quantiles
    res <- data.frame(
        median = q50,
        Qinf95 = qinf95,
        Qsup95 = qsup95)

    ls <- list(x = x, distribution = XCx, quantiles = res)

    class(ls) <- append("XCX", class(ls))

    return(ls)
}
