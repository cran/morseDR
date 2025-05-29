#' @importFrom stats quantile
#' @importFrom stats median
#' @importFrom stats predict
#' @importFrom stats rbinom
#' @importFrom stats rgamma
#' @importFrom stats rnorm
#' @importFrom stats rpois
#' @importFrom stats runif
#' @importFrom stats aggregate
#' @importFrom stats binom.test
#' @importFrom stats poisson.test
#' @import ggplot2
NULL

#' @noRd
add_binomial <- function(df){
    # compute binomial
    ls_binom = lapply(1:nrow(df), function(r) {
        binom.test(df[r,][["Nsurv"]], df[r,][["Ninit"]])$conf.int
    })
    df_binom = do.call("rbind", ls_binom)
    colnames(df_binom) = c("qinf95", "qsup95")
    df_out <- data.frame(df, df_binom)
    return(df_out)
}

#' @importFrom stats t.test
#' @noRd
add_t_test <- function(df){
    subdata <- split(df, list(df$time, df$conc), drop = TRUE)
    # compute t.test
    ls_t_test = lapply(subdata, function(r) {
        if (nrow(r) == 1) {
            m = rep(r[["measure"]], 2)
        } else{
            m = t.test(r[["measure"]])$conf.int
        }
        do.call(rbind, replicate(nrow(r), m, simplify = FALSE))
    })
    df_t_test = do.call("rbind", ls_t_test)
    colnames(df_t_test) = c("qinf95", "qsup95")
    df_out <- data.frame(df, df_t_test)
    return(df_out)
}

#' @noRd
add_poisson <- function(df){
    # compute poisson
    ls_pois = lapply(1:nrow(df), function(r) {
        poisson.test(df[r,][["Nreprocumul"]], df[r,][["Nindtime"]])$conf.int
    })
    df_pois = do.call("rbind", ls_pois)
    colnames(df_pois) = c("qinf95", "qsup95")
    df_out <- data.frame(df, df_pois)
    return(df_out)
}

#' @title Test if \code{x} is between \code{min} and \code{max}
#' @noRd
is.between <- function(x, min, max){ (x >= min & x <= max) }

#' @title filter dataset at target time
#'
#' @description
#' filter the dataset reproData or survData on the target time selected.
#'
#' @param data an object of class reproData or survData
#' @param target.time the time targetted to consider as the last time for the analysis
#'
#' @return subset the dataframe for target.time used by function with target.time arg
#'
#' @noRd
#'
selectDataTT <- function(data, target.time = NULL) {
    if (is.null(target.time)) {
        target.time <- max(data$time)
    }
    if (!any(data$time == target.time) || target.time == 0) {
        stop("target.time is not one of the possible time !")
    }
    dataTT <- data[data$time == target.time, ]
    return(dataTT)
}

#' @title Calculate the dic for a jags model
#'
#' @description
#' Compute the DIC using the \code{\link[rjags]{dic.samples}} function
#'
#'
#' @param m.M jags model object
#' @param sampling.parameters a \code{list} of parameters to compute DIC
#' - \code{niter}: number of iteration (mcmc)
#' - \code{thin}: thining rate parameter
#' - \code{burnin}: number of iteration burned
#' @param quiet silent option
#'
#' @return DIC computed with \code{\link[rjags]{dic.samples}}
#'
#' @import rjags
#'
#' @noRd
#'
calcDIC <- function(m.M, sampling.parameters, quiet) {
    prog.b <- ifelse(quiet == TRUE, "none", "text")
    dic <- rjags::dic.samples(m.M,
                              n.iter = sampling.parameters$niter,
                       thin = sampling.parameters$thin,
                       progress.bar = prog.b)

    # return penalised DIC
    penaliser_dic <- round(sum(sapply(dic$deviance, mean) + sapply(dic$penalty, mean)))
    return(penaliser_dic)
}

#' @title Estimate the number of iteration
#'
#' @description
#' estimate the number of iteration required for the estimation
#' by using the \code{[coda]raftery.diag}, default value is 3746.
#'
#' @param model jags model from loading function
#' @param parameters parameters from loading function
#' @param n.chains the number of parallel chains for the model
#' @param quiet silent control
#'
#' @returns
#' - \code{niter}: number of iteration (mcmc)
#' - \code{thin}: thining rate parameter
#' - \code{burnin}: number of iteration burned
#'
#' @import rjags
#' @importFrom coda raftery.diag
#'
#' @noRd
#'
modelSamplingParameters <- function(model, parameters, n.chains, quiet, n.iter = NA) {
    if (is.na(n.iter)) {
        niter.init <- 5000
        prog.b <- ifelse(quiet == TRUE, "none", "text") # plot progress bar option
        mcmc <- rjags::coda.samples(
            model, parameters, n.iter = niter.init,
            thin = 1, progress.bar = prog.b)
        RL <- raftery.diag(mcmc)

        # check raftery.diag result (number of sample for the diagnostic procedure)
        if (n.chains < 2) {
            stop('2 or more parallel chains required !')
        }
        # extract raftery diagnostic results
        resmatrix <- RL[[1]]$resmatrix
        for (i in 2:length(RL)) {
            resmatrix <- rbind(resmatrix, RL[[i]]$resmatrix)
        }
        # creation of sampling parameters
        thin <- round(max(resmatrix[, "I"]) + 0.5) # autocorrelation
        niter <- max(resmatrix[, "Nmin"]) * thin # number of iteration
        burnin <- max(resmatrix[, "M"]) # burnin period
    } else{
        thin <- 1
        niter <- n.iter
        burnin <- 0
    }
    return(list(niter = niter, thin = thin, burnin = burnin))
}



#' @title Extract simulation from the fit
#'
#' @description
#' Extract simulation from the fit
#'
#'
#' @param fit object of class \code{FitTT}
#'
#' @returns
#' return a \code{list} with 2 \code{data.frame}:
#' - \code{mcmc}: a data.frame with simulated data.
#' - \code{quantile}: a data.frame with quantile of the simulated data.
#'
#' @export
extract_sim <- function(fit){
    mcmc = as.data.frame(do.call("rbind", fit$mcmc))
    d = mcmc[, grep("sim", colnames(mcmc))]
    if (ncol(d) > 0) {
        dq = as.data.frame(t(apply(d, 2, quantile, probs = c(0.5, 0.025, 0.975))))
        colnames(dq) = c("median", "Q2.5", "Q97.5")
        return(list(mcmc = d, quantile = dq))
    } else{
        stop("No simulation output [sim]")
    }
}
