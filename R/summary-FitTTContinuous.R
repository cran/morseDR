#' @name Summary
#' @export
summary.ContinuousFitTT <- function(object, quiet = FALSE, ...) {
    # d
    d <- qunif(p = c(0.5, 0.025, 0.975), min = 0, max = object$jags.data$dmax)
    # b
    log10b <- qunif(p = c(0.5, 0.025, 0.975),
                    min = object$jags.data$log10bmin,
                    max = object$jags.data$log10bmax)
    b <- 10^log10b
    # e
    log10e <- qnorm(p = c(0.5, 0.025, 0.975),
                    mean = object$jags.data$meanlog10e,
                    sd = 1 / sqrt(object$jags.data$taulog10e))
    e <- 10^log10e
    # sigma
    sigma <- qunif(p = c(0.5, 0.025, 0.975),
                   min = 0,
                   max = object$jags.data$dmax / 2)
    # c
    if (object$model.specification$det.part == "loglogistic_4") {
        c <- qunif(p = c(0.5, 0.025, 0.975), min = 0, max = object$jags.data$dmax)
        res <- rbind(b, d, e, c, sigma)
    } else{
        res <- rbind(b, d, e, sigma)
    }

    ans1 <- format(data.frame(res), scientific = TRUE, digits = 4)
    colnames(ans1) <- c("50%", "2.5%", "97.5%")

    # quantiles of estimated model parameters
    estim.par <-  continuousPARAMS(object$mcmc, det.part = object$model.specification$det.part)
    ans2 <- format(estim.par, scientific = TRUE, digits = 4)
    colnames(ans2) <- c("50%", "2.5%", "97.5%")

    # print
    if (!quiet) {
        cat("Summary: \n\n")
        cat("The ", object$model.specification$det.part, " model with a binomial stochastic part was used !\n\n")
        cat("Priors on parameters (quantiles):\n\n")
        print(ans1)
        cat("\nPosteriors of the parameters (quantiles):\n\n")
        print(ans2)
    }

    invisible(list(Qpriors = ans1, Qpost = ans2))
}
