#' @name FitTT
#' @export
fit.BinaryData <- function(data,
                         target.time = NULL,
                         inits = NULL,
                         n.chains = 3,
                         n.adapt = 3000,
                         quiet = FALSE,
                         warning.print = TRUE,
                         n.iter = NA,
                         ...) {

    # 1. select Data at target.time and pool replicates
    dataTT <- selectDataTT(data, target.time)

    # 2. gather replicates according to time and conc
    dataTT <- aggregate(cbind(Nsurv, Ninit) ~ time + conc, dataTT, sum)

    # 3. choose model by testing mortality in the control
    control <- dataTT[dataTT$conc == 0, ]
    if (any(control$Nsurv < control$Ninit)) {
        det.part <- "loglogisticbinom_3"
        model.text <- llbinom3.model.text
        parameters <- c("log10b", "d", "log10e")
    } else {
        det.part <- "loglogisticbinom_2"
        model.text <- llbinom2.model.text
        parameters <- c("log10b", "log10e")
    }

    # 4. create priors parameters
    jags.data <- binaryCreateJagsData(det.part, dataTT)

    # 5. define model
    model <- load.model.JAGS(model.program = model.text,
                           data = jags.data,
                           inits = inits,
                           n.chains = n.chains,
                           n.adapt = n.adapt,
                           quiet = quiet)

    # 6. determine sampling parameters
    sampling.parameters <- modelSamplingParameters(model, parameters,
                                                   n.chains, quiet, n.iter)
    if (is.na(n.iter) & sampling.parameters$niter > 100000) {
        stop("The model needs too many iterations
             to provide reliable parameter estimates !")
    }

    # 7. sampling
    prog.b <- ifelse(quiet == TRUE, "none", "text")

    mcmc <- coda.samples(model,
                         c(parameters, "sim"),
                         n.iter = sampling.parameters$niter,
                         thin = sampling.parameters$thin,
                         progress.bar = prog.b)

    # estim
    estim.par <- binaryPARAMS(mcmc, det.part)

    # check if estimated LC50 lies in the tested concentration range
    LC50 <- log10(estim.par["e", "median"])
    warnings <- c()
    if (!(min(log10(data$conc)) < LC50 & LC50 < max(log10(data$conc)))) {
        msg <- "The LC50 estimation (model parameter e) lies outside the range
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

    class(OUT) <- append(c("BinaryFitTT", "FitTT"), class(OUT))
    return(OUT)
}


#' @title Create prior parameters
#'
#' @description
#' Creates the parameters to define the prior of the log-logistic binomial model
#'
#'
#' @param det.part model name
#' @param data object of class \code{BinaryData}
#'
#' @return jags.data  list of data required for the jags.model function
#'
#' @noRd
#'
binaryCreateJagsData <- function(det.part, data) {
    # Parameter calculation of concentration min and max
    concmin <- min(sort(unique(data$conc))[-1])
    concmax <- max(data$conc)

    # Create prior parameters for the log logistic model
    # Params to define e
    meanlog10e <- (log10(concmin) + log10(concmax)) / 2
    sdlog10e <- (log10(concmax) - log10(concmin)) / 4
    taulog10e <- 1 / sdlog10e^2
    # Params to define b
    log10bmin <- -2
    log10bmax <- 2
    # list of data use by jags
    jags.data <- list(meanlog10e = meanlog10e,
                      Ninit = data$Ninit,
                      Nsurv = data$Nsurv,
                      taulog10e = taulog10e,
                      log10bmin = log10bmin,
                      log10bmax = log10bmax,
                      n = length(data$conc),
                      xconc = data$conc)

    # list of data use by jags
    if (det.part == "loglogisticbinom_3") {
        jags.data <- c(jags.data,
                       dmin = 0,
                       dmax = 1)
    }
    return(jags.data)
}


#' @title posterior quantiles
#'
#' @description
#' Create the table of posterior estimated parameters for the survival analyses
#'
#' @param mcmc list of estimated parameters for the model with each
#'  item representing a chain
#' @param  det.part model type
#'
#' @return a data frame with 3 columns (values, CIinf, CIsup) and
#'  3-4rows (the estimated parameters)
#'
#' @noRd
#'
binaryPARAMS <- function(mcmc, det.part) {
    # Retrieving parameters of the model
    res.M <- summary(mcmc)

    if (det.part ==  "loglogisticbinom_3") {
        d <- res.M$quantiles["d", "50%"]
        dinf <- res.M$quantiles["d", "2.5%"]
        dsup <- res.M$quantiles["d", "97.5%"]
    }
    # for loglogisticbinom_2 and 3
    b <- 10^res.M$quantiles["log10b", "50%"]
    e <- 10^res.M$quantiles["log10e", "50%"]
    binf <- 10^res.M$quantiles["log10b", "2.5%"]
    einf <- 10^res.M$quantiles["log10e", "2.5%"]
    bsup <- 10^res.M$quantiles["log10b", "97.5%"]
    esup <- 10^res.M$quantiles["log10e", "97.5%"]

    # Definition of the parameter storage and storage data
    # If Poisson Model

    if (det.part == "loglogisticbinom_3") {
        # if mortality in control
        rownames <- c("b", "d", "e")
        params <- c(b, d, e)
        CIinf <- c(binf, dinf, einf)
        CIsup <- c(bsup, dsup, esup)
    } else {
        # if no mortality in control
        # Definition of the parameter storage and storage data
        rownames <- c("b", "e")
        params <- c(b, e)
        CIinf <- c(binf, einf)
        CIsup <- c(bsup, esup)
    }

    res <- data.frame(median = params,
                      Q2.5 = CIinf,
                      Q97.5 = CIsup,
                      row.names = rownames)

    return(res)
}
