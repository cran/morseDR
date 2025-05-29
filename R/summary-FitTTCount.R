#' @name Summary
#' @export
summary.CountFitTT <- function(object, quiet = FALSE, ...) {

    det.part <- object$model.specification$det.part
    # b
    log10b <- qunif(p = c(0.5, 0.025, 0.975),
                    min = object$jags.data$log10bmin,
                    max = object$jags.data$log10bmax)
    b <- 10^log10b
    # d
    d <- qnorm(p = c(0.5, 0.025, 0.975),
               mean = object$jags.data$meand,
               sd = 1 / sqrt(object$jags.data$taud))
    # e
    log10e <- qnorm(p = c(0.5, 0.025, 0.975),
                    mean = object$jags.data$meanlog10e,
                    sd = 1 / sqrt(object$jags.data$taulog10e))
    e <- 10^log10e
    if (det.part == "P") {
        res <- rbind(b, d, e)
    }
    if (det.part == "GP") {
        # omega
        log10omega <- qunif(p = c(0.5, 0.025, 0.975),
                            min = object$jags.data$log10omegamin,
                            max = object$jags.data$log10omegamax)
        omega <- 10^log10omega
        res <- rbind(b, d, e, omega)
    }

    ans1 <-  format(data.frame(res), scientific = TRUE, digits = 4)
    colnames(ans1) <- c("50%", "2.5%", "97.5%")
    # quantiles of estimated model parameters
    estim.par <-  countPARAMS(object$mcmc, det.part)

    ans2 <- format(estim.par, scientific = TRUE, digits = 4)
    colnames(ans2) <- c("50%", "2.5%", "97.5%")
    if (!quiet) {
        cat("Summary: \n\n")
        if (det.part == "GP")
            cat("The ", det.part, "model with a Gamma Poisson stochastic part was used !\n\n")
        if (det.part == "P")
            cat("The ", det.part, "model with a Poisson stochastic part was used !\n\n")
        cat("Priors on parameters (quantiles):\n\n")
        print(ans1)
        cat("\nPosteriors of the parameters (quantiles):\n\n")
        print(ans2)
    }
    invisible(list(Qpriors = ans1, Qposteriors = ans2))
}

