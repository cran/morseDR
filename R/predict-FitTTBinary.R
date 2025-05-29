#' @name Predict
#' @export
predict.BinaryFitTT <- function(fit, display.conc = NULL, ...){

    if (is.null(display.conc)) {
        display.conc <- fit$dataTT$conc
    }
    mctot <- do.call("rbind", fit$mcmc)
    # parameters
    if (fit$model.specification$det.part == "loglogisticbinom_3") {
        d2 <- mctot[, "d"]
    }
    log10b2 <- mctot[, "log10b"]
    b2 <- 10^log10b2
    log10e2 <- mctot[, "log10e"]
    e2 <- 10^log10e2

    if (fit$model.specification$det.part == "loglogisticbinom_2") {
        ls <- lapply(display.conc, function(conc){
            1 / (1 + (conc / e2)^(b2)) # mean curve
        })
    }
    if (fit$model.specification$det.part == "loglogisticbinom_3") {
        ls <- lapply(display.conc, function(conc){
            d2 / (1 + (conc / e2)^(b2)) # mean curve
        })
    }
    df_mcmc <- as.data.frame(do.call("rbind", ls))
    df_quantile <- as.data.frame(
        t(apply(df_mcmc, 1, quantile,
                probs = c(0.025, 0.5, 0.975), na.rm = TRUE))
    )
    colnames(df_quantile) <- c("qinf95", "q50", "qsup95")

    return(list(display.conc = display.conc, mcmc = df_mcmc, quantile = df_quantile))
}
