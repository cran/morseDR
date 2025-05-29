#' @name Summary
#' @export
summary.BinaryFitTT <- function(object, quiet = FALSE, ...) {

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

    # d
    if (object$model.specification$det.part == "loglogisticbinom_3") {

        d <- qunif(p = c(0.5, 0.025, 0.975),
                   min = object$jags.data$dmin,
                   max = object$jags.data$dmax)

        res <- rbind(b, d, e)
    } else {
        res <- rbind(b, e)
    }

    ans1 <- format(data.frame(res), scientific = TRUE, digits = 4)
    colnames(ans1) <- c("50%", "2.5%", "97.5%")

    # quantiles of estimated model parameters
    estim.par <-  binaryPARAMS(object$mcmc, object$model.specification$det.part)
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

