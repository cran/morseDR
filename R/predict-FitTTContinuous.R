#' @name Predict
#' @export
predict.ContinuousFitTT <- function(fit, display.conc = NULL, ...){

    if (is.null(display.conc)) {
        display.conc <- fit$dataTT$conc
    }
    mctot <- do.call("rbind", fit$mcmc)
    # parameters
    d <- mctot[, "d"]
    sigma <- mctot[, "sigma"]
    log10b <- mctot[, "log10b"]
    b <- 10^log10b
    log10e <- mctot[, "log10e"]
    e <- 10^log10e
    if (fit$model.specification$det.part == "loglogistic_4") {
        c <- mctot[, "c"]
        lstemp <- lapply(display.conc, function(conc){
            c + (d - c) / (1 + (conc / e)^b)
        })
    } else {
        lstemp <- lapply(display.conc, function(conc){
            d / (1 + (conc / e)^b)
        })
    }

    ls <- lapply(lstemp, function(m){
        if ( fit$model.specification$stoc.part == "normal") {
            r = rnorm(n = length(m), mean = m, sd = sigma)
        }
        if ( fit$model.specification$stoc.part == "gamma") {
            r = rgamma(n = length(m), shape = m / sigma, rate = sigma)
        }
        if ( fit$model.specification$stoc.part == "normal_truncated") {
            r = rnorm(n = length(m), mean = m, sd = sigma)
            r[r < 0] = NA
        }
        return(r)
    })

    df_mcmc <- as.data.frame(do.call("rbind", ls))
    df_quantile <- as.data.frame(
        t(apply(df_mcmc, 1, quantile,
                probs = c(0.025, 0.5, 0.975), na.rm = TRUE))
    )
    colnames(df_quantile) <- c("qinf95", "q50", "qsup95")

    return(list(display.conc = display.conc, mcmc = df_mcmc, quantile = df_quantile))
}
