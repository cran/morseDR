#' @name PPC
#'
#' @title Posterior Predictive Check data.frame for \code{FitTT} objects
#'
#' @description
#' Create the \code{PPC} object to be pass in plot function for plotting the
#' Posterior Predictive Check.#'
#'
#' @param fit An object of class \code{FitTT}
#' @param \dots Further arguments to be passed to generic methods
#'
#' @return An object of class 'PPC'
#'
#' @export
ppc <- function(fit, ...){
    UseMethod("ppc")
}

#' @name PPC
#' @export
ppc.ContinuousFitTT <- function(fit, ...) {
    display.conc <-  fit$dataTT$conc
    predictTT <- predict(fit, display.conc)
    df <- data.frame(fit$dataTT, predictTT$quantile)
    df$color <- is.between(df$measure, df$qinf95, df$qsup95)
    df$response <- df$measure
    class(df) <- append("PPC", class(df))
    return(df)
}

#' @name PPC
#' @export
ppc.CountFitTT <- function(fit, ...) {

    # 1. compute predictions
    display.conc <-  fit$dataTT$conc
    predictTT <- predict(fit, display.conc)
    # 2. compute Nsurv predicted
    Nindtime <- fit$dataTT$Nindtime
    mcmc <- predictTT$mcmc
    len_prediction <- ncol(mcmc)
    ls <- lapply(1:nrow(mcmc), function(i){
        p <- as.numeric(mcmc[i,])
        rpois(len_prediction, p * Nindtime[i])
    })
    df_mcmc <- as.data.frame(do.call("rbind", ls))
    df_quantile = as.data.frame(
        t(apply(df_mcmc, 1, quantile,
                probs = c(0.025, 0.5, 0.975), na.rm = TRUE))
    )
    colnames(df_quantile) <- c("qinf95", "q50", "qsup95")
    df <- data.frame(fit$dataTT, df_quantile)
    df$color <- is.between(df$Nreprocumul, df$qinf95, df$qsup95)
    df$response <- df$Nreprocumul
    # SET CLASS
    class(df) <- append("PPC", class(df))
    return(df)
}


#' @name PPC
#' @export
ppc.BinaryFitTT <- function(fit, ...) {

    # 1. compute predictions
    display.conc <-  fit$dataTT$conc
    predictTT <- predict(fit, display.conc)
    # 2. compute Nsurv predicted
    Ninit <- fit$dataTT$Ninit
    mcmc <- predictTT$mcmc
    len_prediction <- ncol(mcmc)
    ls <- lapply(1:nrow(mcmc), function(i){
        p <- as.numeric(mcmc[i,])
        rbinom(len_prediction, Ninit[i], p)
    })
    df_mcmc <- as.data.frame(do.call("rbind", ls))
    df_quantile = as.data.frame(
        t(apply(df_mcmc, 1, quantile,
                probs = c(0.025, 0.5, 0.975), na.rm = TRUE))
    )
    colnames(df_quantile) <- c("qinf95", "q50", "qsup95")
    df <- data.frame(fit$dataTT, df_quantile)
    df$color <- is.between(df$Nsurv, df$qinf95, df$qsup95)
    df$response <- df$Nsurv
    # SET CLASS
    class(df) <- append("PPC", class(df))
    return(df)
}
