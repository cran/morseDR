#' @name FitTT
#' @export
fit.ContinuousData <- function(data,
                         stoc.part = "gamma",
                         target.time = NULL,
                         inits = NULL,
                         n.chains = 3,
                         n.adapt = 3000,
                         quiet = FALSE,
                         warning.print = TRUE,
                         n.iter = NA,
                         low.asympt = FALSE,
                         ...) {

    # stocastic verification
    stoc.partpossible <- c("normal", "gamma", "normal_truncated")
    if (!any(stoc.partpossible == stoc.part))
        stop("Invalid value for argument [stoc.part]")

    # 1. select Data at target.time and pool replicates
    dataTT <- selectDataTT(data, target.time)

    # 2. create priors parameters
    jags.data <- continuousCreateJagsData(dataTT)

    # 3. initialize the model
    if (low.asympt == FALSE) {
        det.part <- "loglogistic_3"
        if ( stoc.part == "normal") {
            model.text <- llnormal3.model
        }
        if ( stoc.part == "gamma") {
            model.text <- llgamma3.model
        }
        if ( stoc.part == "normal_truncated") {
            model.text <- llnormal_truncated3.model
        }
        parameters <- c("d", "log10b", "log10e", "sigma")
    } else{
        det.part <- "loglogistic_4"
        if ( stoc.part == "normal") {
            model.text <- llnormal4.model
        }
        if ( stoc.part == "gamma") {
            model.text <- llgamma4.model
        }
        if ( stoc.part == "normal_truncated") {
            model.text <- llnormal_truncated4.model
        }
        parameters <- c("c", "d", "log10b", "log10e", "sigma")
    }

    # 4. define model
    model <- load.model.JAGS(model.program = model.text,
                             data = jags.data,
                             inits = inits,
                             n.chains = n.chains,
                             n.adapt = n.adapt,
                             quiet = quiet)

    # 5. determine sampling parameters
    sampling.parameters <- modelSamplingParameters(model, parameters,
                                                   n.chains, quiet, n.iter)
    if (is.na(n.iter) & sampling.parameters$niter > 100000) {
        stop("The model needs too many iterations
         to provide reliable parameter estimates !")
    }
    # 7. sampling
    prog.b <- ifelse(quiet == TRUE, "none", "text")

    # monitoring parameters
    mcmc <- coda.samples(model,
                         c(parameters, "sim"),
                         n.iter = sampling.parameters$niter,
                         thin = sampling.parameters$thin,
                         progress.bar = prog.b)

    # summarize estime.par et CIs
    # calculate from the estimated parameters
    estim.par <- continuousPARAMS(mcmc, det.part)

    # check if estimated LC50 lies in the tested concentration range
    EC50 <- log10(estim.par["e", "median"])
    warnings <- c()
    if (!(min(log10(data$conc)) < EC50 & EC50 < max(log10(data$conc)))) {
        msg <- "The EC50 estimation (model parameter e) lies outside the range
        of tested concentrations and may be unreliable as the prior
        distribution on this parameter is defined from this range !"
        warnings <- append(msg, warnings)
        if (warning.print) {
            warning(msg, call. = FALSE)
        }
    }

    # output
    model.specification = list(
        n.iter = sampling.parameters$niter,
        thin = sampling.parameters$thin,
        stoc.part = stoc.part,
        det.part = det.part,
        model.text = model.text
    )
    OUT <- list(mcmc = mcmc,
                warnings = warnings,
                parameters = parameters,
                model.specification = model.specification,
                jags.data = jags.data,
                original.data = data,
                dataTT = dataTT)

    class(OUT) <- append(c("ContinuousFitTT", "FitTT"), class(OUT))
    return(OUT)
}



#' @title Create prior parameters
#'
#' @description
#' Creates the parameters to define the prior of the log-logistic binomial model
#'
#' @param data object of class \code{ContinuousData}
#'
#' @return jags.data a list of data required for the jags.model function
#'
#' @noRd
#'
continuousCreateJagsData <- function(data) {

    ####################################################
    # for d prior (upper asymptote)
    ymax <- max(data$measure)
    dmax <- ymax*2

    # for e prior (ER50)
    concmin <- min(sort(unique(data$conc))[-1])
    concmax <- max(data$conc)
    meanlog10e <- (log10(concmin) + log10(concmax))/2
    sdlog10e <- (log10(concmax) - log10(concmin))/4
    taulog10e <- 1/sdlog10e^2

    # for b prior
    log10bmin <- -2
    log10bmax <- 2
    # remove the observation with highest measure measurement
    data <- data[data$measure != ymax, ]

    # create data for JAGS
    jags.data <- list(n = nrow(data),
                      xconc = data$conc,
                      measure = data$measure,
                      meanlog10e = meanlog10e,
                      taulog10e = taulog10e,
                      dmax = dmax,
                      log10bmin = log10bmin,
                      log10bmax = log10bmax)

    return(jags.data)
}

#' @title posterior quantiles
#'
#' @description
#' Create the table of posterior estimated parameters for the continuous
#' data analyses
#'
#' @param mcmc list of estimated parameters for the model with each
#'  item representing a chain
#' @param  det.part model type
#'
#' @return a data frame with 3 columns (values, CIinf, CIsup) and
#'  rows for the estimates parameters
#'
#' @noRd
#'
continuousPARAMS <- function(mcmc, det.part) {

    mcmctot <- do.call("rbind", mcmc)
    paramd <- mcmctot[, "d"]
    paramb <- 10^mcmctot[, "log10b"]
    parame <- 10^mcmctot[, "log10e"]
    paramsigma <- mcmctot[, "sigma"]
    if (det.part == "loglogistic_4") {
        paramc <- mcmctot[, "c"]
        estim.params <- lapply(
            list(b = paramb, d = paramd, e = parame, c = paramc, sigma = paramsigma),
            quantile, probs = c(0.5, 0.025, 0.975))
    } else{
        estim.params <- lapply(
            list(b = paramb, d = paramd, e = parame, sigma = paramsigma),
            quantile, probs = c(0.5, 0.025, 0.975))
    }
    estim.params <- as.data.frame(t(as.data.frame(estim.params)))
    colnames(estim.params) <- c("median", "Q2.5", "Q97.5")

    return(estim.params)
}

