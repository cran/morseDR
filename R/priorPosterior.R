#' @title Return Prior and Posterior density of parameters of \code{FitTT} object
#'
#' @description
#' Return Prior and Posterior density of parameters of \code{FitTT} object
#'
#' @name PriorPosterior
#'
#' @param fit An object of class \code{FitTT}
#' @param size_sample graphical backend, can be \code{'generic'} or \code{'ggplot'}
#' @param \dots Further arguments to be passed to generic methods
#'
#' @return a data frame of class \code{PriorPosterior}
#'
#' @export
priorPosterior <- function(fit, size_sample, ...){
    UseMethod("priorPosterior")
}


#' @name PriorPosterior
#' @export
priorPosterior.BinaryFitTT <- function(fit, size_sample = 1e3, ...) {

    jd = fit$jags.data
    df_prior <- data.frame(
        d = runif(size_sample, jd$dmin, jd$dmax),
        log10b = runif(size_sample, jd$log10bmin, jd$log10bmax),
        log10e = rnorm(size_sample, jd$meanlog10e, sqrt(1/jd$taulog10e))
    )
    if ( fit$model.specification$det.part == "loglogisticbinom_3") {
        df_prior$d <- runif(size_sample, jd$dmin, jd$dmax)
    }
    df_posterior <- as.data.frame(do.call("rbind", fit$mcmc))
    df_posterior <- df_posterior[, colnames(df_prior)]

    df_prior$pp <- "prior"
    df_posterior$pp <- "posterior"
    df <- rbind(df_prior, df_posterior, make.row.names = FALSE)
    class(df) <- append("PriorPosterior", class(df))
    return(df)
}

#' @rdname PriorPosterior
#' @export
priorPosterior.CountFitTT <- function(fit, size_sample = 1e3, ...) {
    jd = fit$jags.data
    df_prior <- data.frame(
        d = rnorm(size_sample, jd$meand, sqrt(1/jd$taud)),
        log10b = runif(size_sample, jd$log10bmin, jd$log10bmax),
        log10e = rnorm(size_sample, jd$meanlog10e, sqrt(1/jd$taulog10e))
    )
    if ( "log10omega" %in% fit$parameters) {
        df_prior$log10omega <- runif(size_sample, jd$log10omegamin, jd$log10omegamax)
    }
    df_posterior <- as.data.frame(do.call("rbind", fit$mcmc))
    df_posterior <- df_posterior[, colnames(df_prior)]

    df_prior$pp <- "prior"
    df_posterior$pp <- "posterior"
    df <- rbind(df_prior, df_posterior, make.row.names = FALSE)
    class(df) <- append("PriorPosterior", class(df))
    return(df)
}

#' @rdname PriorPosterior
#' @export
priorPosterior.ContinuousFitTT <- function(fit, size_sample = 1e3, ...) {
    jd = fit$jags.data
    df_prior <- data.frame(
        d = runif(size_sample, 0, jd$dmax),
        log10b = runif(size_sample, jd$log10bmin, jd$log10bmax),
        log10e = rnorm(size_sample, jd$meanlog10e, sqrt(1/jd$taulog10e)),
        sigma = runif(size_sample, 0, jd$dmax/2)
    )
    df_posterior <- as.data.frame(do.call("rbind", fit$mcmc))
    df_posterior <- df_posterior[, colnames(df_prior)]

    df_prior$pp <- "prior"
    df_posterior$pp <- "posterior"
    df <- rbind(df_prior, df_posterior, make.row.names = FALSE)
    class(df) <- append("PriorPosterior", class(df))
    return(df)
}


#' @title Extract posterior of parameters from a \code{FitTT} object
#'
#' @description
#' Extract posterior of parameters from a \code{FitTT} object#'
#'
#' @name PriorPosterior
#'
#' @param fit An object of class \code{FitTT}
#' @param \dots Further arguments to be passed to generic methods
#'
#' @return Return an object of class \code{Posterior}
#'
#'
#' @export
posterior <- function(fit, ...){
    UseMethod("posterior")
}

#' @rdname PriorPosterior
#' @export
posterior.FitTT <- function(fit, ...) {
    df <- as.data.frame(do.call("rbind", fit$mcmc))
    rownames(df) <- NULL
    class(df) <- append("Posterior", class(df))
    return(df)
}
