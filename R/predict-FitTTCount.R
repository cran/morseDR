#' @name Predict
#' @export
predict.CountFitTT <- function(fit, display.conc = NULL, ...){

    if (is.null(display.conc)) {
        display.conc <- fit$dataTT$conc
    }
    mctot <- do.call("rbind", fit$mcmc)
    len_mcmc <- nrow(mctot)
    # parameters
    d2 <- mctot[, "d"]
    log10b2 <- mctot[, "log10b"]
    b2 <- 10^log10b2
    log10e2 <- mctot[, "log10e"]
    e2 <- 10^log10e2

    if (fit$model.specification$det.part == "GP") {
        log10omega2 <- mctot[, "log10omega"]
        omega2 <- 10^(log10omega2)
    }

    # all theoretical
    if (fit$model.specification$det.part == "P") {
        ls <- lapply( display.conc, function(x){
             d2 / (1 + (x / e2)^(b2)) # mean curve
        })
    }
    if (fit$model.specification$det.part == "GP") {
        lstemp <- lapply(display.conc, function(x){
            d2 / (1 + (x / e2)^(b2)) # mean curve
        })
        ls <- lapply(seq_along(display.conc), function(i){
            rgamma(n = len_mcmc, shape = lstemp[[i]] / omega2, rate = 1 / omega2)
        })
    }
    df_mcmc <- as.data.frame(do.call("rbind", ls))
    df_quantile = as.data.frame(
        t(apply(df_mcmc, 1, quantile,
                probs = c(0.025, 0.5, 0.975), na.rm = TRUE))
    )
    colnames(df_quantile) <- c("qinf95", "q50", "qsup95")

    return(list(display.conc = display.conc, mcmc = df_mcmc, quantile = df_quantile))
}
