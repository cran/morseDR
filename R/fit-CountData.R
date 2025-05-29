#' @name FitTT
#' @export
fit.CountData <- function(data,
                       stoc.part = "bestfit",
                       target.time = NULL,
                       inits = NULL,
                       n.chains = 3,
                       n.adapt = 3000,
                       quiet = FALSE,
                       warning.print = TRUE,
                       n.iter = NA,
                       ...) {
    # stocastic verification
    stoc.partpossible <- c("poisson", "gammapoisson", "bestfit")
    if (!any(stoc.partpossible == stoc.part))
        stop("Invalid value for argument [stoc.part]")

    # check 0 Nreprocumul
    if (all(data$Nreprocumul == 0))
        stop("Nreprocumul contains only 0 values !")

    # parameters
    parameters <- list(poisson = c("d", "log10b", "log10e"),
                       gammapoisson = c("d", "log10b","log10e", "log10omega"))

    # select Data at target.time
    dataTT <- selectDataTT(data, target.time)
    # create priors parameters
    jags.data <- countCreateJagsData(stoc.part, dataTT)


    if (stoc.part != "bestfit") {

        # Poisson model only
        if (stoc.part == "poisson") {
            jags.data[c("meanlog10omega", "taulog10omega", "log10omegamin", "log10omegamax")] <- NULL

            model_program <- llm.poisson.model.text
            model_label <- "P"
            parameters_select <- parameters$poisson
        }
        # Gamma-poisson model only
        if (stoc.part == "gammapoisson") {
            model_program <- llm.gammapoisson.model.text
            model_label <- "GP"
            parameters_select <- parameters$gammapoisson
        }

        # Define model
        model <- load.model.JAGS(
            model.program = model_program,
            data = jags.data,
            inits = inits,
            n.chains = n.chains,
            n.adapt = n.adapt,
            quiet = quiet)
        # Determine sampling parameters
        sampling.parameters <- modelSamplingParameters(
            model,
            parameters_select,
            n.chains, quiet, n.iter)

        if (is.na(n.iter) && sampling.parameters$niter > 100000)
            stop("The model needs too many iterations to provide reliable parameter estimates !")

        # calcul DIC
        DIC <- calcDIC(model, sampling.parameters, quiet)
        # list of objet for the coda.sample function
        coda.arg <- list(model = model,
                         model.label = model_label,
                         niter = sampling.parameters$niter,
                         thin = sampling.parameters$thin,
                         nburnin = sampling.parameters$burnin,
                         parameters = parameters_select,
                         DIC = DIC)
    }
    # Model Selection by the DIC
    if (stoc.part == "bestfit") {

        gammapoisson.model <- load.model.JAGS(
            model.program = llm.gammapoisson.model.text,
            data = jags.data,
            inits = inits,
            n.chains = n.chains,
            n.adapt = n.adapt,
            quiet = quiet)

        jags.data2 = jags.data
        jags.data2[c("meanlog10omega", "taulog10omega", "log10omegamin", "log10omegamax")] <- NULL
        # Define models
        poisson.model <- load.model.JAGS(
            model.program = llm.poisson.model.text,
            data = jags.data2,
            inits = inits,
            n.chains = n.chains,
            n.adapt = n.adapt,
            quiet = quiet)


        # Determine sampling parameters
        poisson.sampling.parameters <- modelSamplingParameters(
            poisson.model, parameters$poisson, n.chains, quiet, n.iter)

        gammapoisson.sampling.parameters <- modelSamplingParameters(
            gammapoisson.model, parameters$gammapoisson, n.chains, quiet, n.iter)

        if (is.na(n.iter) && poisson.sampling.parameters$niter > 100000 &&
            gammapoisson.sampling.parameters$niter > 100000) {
            stop("The model needs too many iterations to provide reliable parameter estimates !")
        }

        # calcul DIC
        poisson.DIC <- calcDIC(poisson.model, poisson.sampling.parameters, quiet)
        gammapoisson.DIC <- calcDIC(
            gammapoisson.model, gammapoisson.sampling.parameters, quiet)

        if (is.na(n.iter) & gammapoisson.sampling.parameters$niter > 100000) {
            # list of object for the coda.sample function
            coda.arg <- list(model = poisson.model,
                             model.label = "P",
                             niter = poisson.sampling.parameters$niter,
                             thin = poisson.sampling.parameters$thin,
                             nburnin = poisson.sampling.parameters$burnin,
                             parameters = parameters$poisson,
                             DIC = poisson.DIC)
        }

        if (is.na(n.iter) & poisson.sampling.parameters$niter > 100000) {
            # list of object for the coda.sample function
            coda.arg <- list(model = gammapoisson.model,
                             model.label = "GP",
                             niter = gammapoisson.sampling.parameters$niter,
                             thin = gammapoisson.sampling.parameters$thin,
                             nburnin = gammapoisson.sampling.parameters$burnin,
                             parameters = parameters$gammapoisson,
                             DIC = gammapoisson.DIC)
        }
        if (poisson.sampling.parameters$niter <= 100000 && gammapoisson.sampling.parameters$niter <= 100000) {
            if (poisson.DIC <= (gammapoisson.DIC + 10)) {
                # list of objet for the coda.sample function
                coda.arg <- list(model = poisson.model,
                                 model.label = "P",
                                 niter = poisson.sampling.parameters$niter,
                                 thin = poisson.sampling.parameters$thin,
                                 nburnin = poisson.sampling.parameters$burnin,
                                 parameters = parameters$poisson,
                                 DIC = poisson.DIC)
            } else {
                # list of objet for the coda.sample function
                coda.arg <- list(model = gammapoisson.model,
                                 model.label = "GP",
                                 niter = gammapoisson.sampling.parameters$niter,
                                 thin = gammapoisson.sampling.parameters$thin,
                                 nburnin = gammapoisson.sampling.parameters$burnin,
                                 parameters = parameters$gammapoisson,
                                 DIC = gammapoisson.DIC)
            }
        }
    }

    # Sampling
    prog.b <- ifelse(quiet == TRUE, "none", "text")
    mcmc <- coda.samples(coda.arg$model,
                         c(coda.arg$parameters, "sim"),
                         n.iter = coda.arg$niter,
                         thin = coda.arg$thin,
                         progress.bar = prog.b)

    # summarize estime.par et CIs
    # calculate from the estimated parameters
    estim.par <- countPARAMS(mcmc, coda.arg$model.label)

    # check if the maximum measured concentration is in the EC50's range of
    # 95% percentile
    EC50 <- log10(estim.par["e", "median"])
    warnings <- c()
    if (!(min(log10(data$conc)) < EC50 & EC50 < max(log10(data$conc)))) {
        ##store warning in warnings table
        msg <- "The EC50 estimation (model parameter e) lies outside the range of
    tested concentration and may be unreliable as the prior distribution on
    this parameter is defined from this range !"
        warnings <- append(msg, warnings)
        if (warning.print) {
            warning(msg, call. = FALSE)
        }
    }


    # output
    if (coda.arg$model.label == "GP") {
        model.text = llm.gammapoisson.model.text
    } else{
        model.text = llm.poisson.model.text
    }
    model.specification = list(
        n.iter = coda.arg$niter,
        thin = coda.arg$thin,
        det.part = coda.arg$model.label,
        model.text = model.text
    )
    OUT <- list(mcmc = mcmc,
                warnings = warnings,
                parameters = coda.arg$parameters,
                model.specification = model.specification,
                jags.data = jags.data,
                original.data = data,
                dataTT = dataTT)

    class(OUT) <- append(c("CountFitTT", "FitTT"), class(OUT))
    return(OUT)
}

#' @title create the parameters to define the prior of the log-logistic model
#' for reproduction data analysis
#'
#' @param stoc.part model name
#' @param data object of class \code{CountData}
#'
#'
#' @return `jags.data`: list data require for the jags.model function
#'
#' @importFrom stats sd
#'
#' @noRd
#'
countCreateJagsData <- function(stoc.part, data) {

    # separate control data to the other
    # tab0: data at conc = 0
    tab0 <- data[data$conc == min(data$conc), ]
    # tab: data at conc != 0
    tab <- data[data$conc != min(data$conc), ]

    Nindtime <- tab$Nindtime
    NreprocumulIndtime0 <- tab0$Nreprocumul / tab0$Nindtime # cumulated number of
    # offspring / number of
    # individual-days
    conc <- tab$conc
    Ncumul <- tab$Nreprocumul
    n <- nrow(tab) # number of observation != from the control

    # Parameter calculation of concentration min and max
    concmin <- min(sort(unique(conc))[-1])
    concmax <- max(conc)

    # create priors parameters for the log logistic model

    # Params to define log10e
    meanlog10e <- (log10(concmin) + log10(concmax)) / 2
    sdlog10e <- (log10(concmax) - log10(concmin)) / 4
    taulog10e <- 1 / sdlog10e^2

    # Params to define d
    meand <- mean(NreprocumulIndtime0)
    SEd <- sd(NreprocumulIndtime0) / sqrt(length(unique(tab0$replicate)))
    taud <- 1 / (SEd)^2

    # Params to define b
    log10bmin <- -2
    log10bmax <- 2

    # list of data use by jags
    jags.data <- list(meanlog10e = meanlog10e,
                      taulog10e = taulog10e,
                      meand = meand,
                      taud = taud,
                      log10bmin = log10bmin,
                      log10bmax = log10bmax,
                      n = n,
                      xconc = conc,
                      Nindtime = Nindtime,
                      Ncumul = Ncumul)

    # Params to define overdispersion rate
    if (stoc.part == "bestfit" || stoc.part == "gammapoisson") {
        log10omegamin <- -4
        log10omegamax <- 4

        # list of data use by jags
        jags.data <- c(jags.data,
                       log10omegamin = log10omegamin,
                       log10omegamax = log10omegamax)
    }
    return(jags.data)
}


#' @title create the table of posterior estimated parameters for the count data
#' analyses
#'
#' @param mcmc list of estimated parameters for the model with each item representing
#' @param MODEL a position flag model with P: poisson model and GP: gammapoisson
#' model
#'
#' @return data frame with 3 columns (values, CIinf, CIsup) and 3-4rows (the estimated
#' parameters)
#'
#' @noRd
#'
countPARAMS <- function(mcmc, MODEL = "P") {

    # Retrieving parameters of the model
    res.M <- summary(mcmc)

    b <- 10^res.M$quantiles["log10b", "50%"]
    d <- res.M$quantiles["d", "50%"]
    e <- 10^res.M$quantiles["log10e", "50%"]
    binf <- 10^res.M$quantiles["log10b", "2.5%"]
    dinf <- res.M$quantiles["d", "2.5%"]
    einf <- 10^res.M$quantiles["log10e", "2.5%"]
    bsup <- 10^res.M$quantiles["log10b", "97.5%"]
    dsup <- res.M$quantiles["d", "97.5%"]
    esup <- 10^res.M$quantiles["log10e", "97.5%"]

    # Definition of the parameter storage and storage data

    # If Poisson Model
    if (MODEL == "P") {
        rownames <- c("b", "d", "e")
        params <- c(b, d, e)
        CIinf <- c(binf, dinf, einf)
        CIsup <- c(bsup, dsup, esup)
    }
    # If Gamma Poisson Model
    if (MODEL == "GP") {
        # Calculation of the parameter omega
        omega <- 10^res.M$quantiles["log10omega", "50%"]
        omegainf <- 10^res.M$quantiles["log10omega", "2.5%"]
        omegasup <- 10^res.M$quantiles["log10omega", "97.5%"]
        # Definition of the parameter storage and storage data
        rownames <- c("b", "d", "e", "omega")
        params <- c(b, d, e, omega)
        CIinf <- c(binf, dinf, einf, omegainf)
        CIsup <- c(bsup, dsup, esup, omegasup)
    }

    res <- data.frame(median = params, Q2.5 = CIinf, Q97.5 = CIsup,
                      row.names = rownames)

    return(res)
}
