################################################################################
#
# BINARY
#
#' @title Loglogistic binomial model with 3 parameters
#'
#' @details specification of priors (may be changed if needed) using parameters
#' `dmin`, `dmax`, `log10min`, `log10max`, `meanlog10e` and `taulog10e`
#'
#' @noRd
#'
llbinom3.model.text <- "
model {
    for (i in 1:n){
        p[i] <- d/ (1 + (xconc[i]/e)^b)
        Nsurv[i] ~ dbin(p[i], Ninit[i])
        sim[i] ~ dbin(p[i], Ninit[i])
    }
    # specification of priors (may be changed if needed)
    d ~ dunif(dmin, dmax)
    log10b ~ dunif(log10bmin, log10bmax)
    log10e ~ dnorm(meanlog10e, taulog10e)

    b <- pow(10, log10b)
    e <- pow(10, log10e)
}"

#' @title Loglogistic binomial model with 2 parameters
#'
#' @details specification of priors (may be changed if needed) using parameters
#' `log10min`, `log10max`, `meanlog10e` and `taulog10e`
#'
#' @noRd
#'
llbinom2.model.text <- "
model {
    for (i in 1:n){
        p[i] <- 1/ (1 + (xconc[i]/e)^b)
        Nsurv[i]~ dbin(p[i], Ninit[i])
        sim[i]~ dbin(p[i], Ninit[i])
    }
    log10b ~ dunif(log10bmin, log10bmax)
    log10e ~ dnorm(meanlog10e, taulog10e)

    b <- pow(10, log10b)
    e <- pow(10, log10e)
}"
################################################################################
#
# COUNT
#
#' @title Loglogistic Poisson model
#'
#' @description
#' Explicit writting of a Poisson law for each replicate
#' mean is given by the theoretical curve
#'
#' @details specification of priors (may be changed if needed) using parameters
#'
#' @noRd
#'
llm.poisson.model.text <- "
model{
    for (j in 1:n){
        ytheo[j] <- d / (1 + pow(xconc[j]/e, b))
        nbtheo[j] <- ytheo[j]*Nindtime[j]
        Ncumul[j] ~ dpois(nbtheo[j])
        sim[j] ~ dpois(nbtheo[j])
    }
    # Prior distributions
    d ~ dnorm(meand, taud)T(0,)
    log10b ~ dunif(log10bmin, log10bmax)
    log10e ~ dnorm(meanlog10e, taulog10e)

    b <- pow(10,log10b)
    e <- pow(10,log10e)
}"

#' @title  Loglogisitc Gamma poisson model
#'
#' @description
#' Explicit writting of a gamma-Poisson law for each replicate
#' the mean is given by a gamma law centered on the theoretical curve
#'
#' @details specification of priors (may be changed if needed) using parameters
#'
#' @noRd
#'
llm.gammapoisson.model.text <- "
model {
    for (j in 1:n){
        rate[j] <- d / (1 + pow(xconc[j]/e, b)) / omega
        p[j] <- 1 / (Nindtime[j] * omega + 1)
        Ncumul[j] ~ dnegbin(p[j], rate[j])
        sim[j] ~ dnegbin(p[j], rate[j])
    }
    # Prior distributions
    d ~ dnorm(meand, taud)T(0,)
    log10b ~ dunif(log10bmin, log10bmax)
    log10e ~ dnorm(meanlog10e, taulog10e)
    log10omega ~ dunif(log10omegamin, log10omegamax)

    omega <- pow(10,log10omega)
    b <- pow(10,log10b)
    e <- pow(10,log10e)
}"

################################################################################
#
# CONTINUOUS
#' @title Loglogistic normal model
#'
#' @description
#' A 3-parameters  log-logistic normal model for response data
#'
#' @details specification of priors (may be changed if needed) using parameters
#'
#' @noRd
#'
llnormal3.model <- "
model {
  for (k in 1:n) {
    mu[k] <- d / (1 + (xconc[k] / e)^b)
    measure[k] ~ dnorm(mu[k], tau)
    sim[k] ~ dnorm(mu[k], tau)
  }
  # specification of priors (may be changed if needed)
  d ~ dunif(0, dmax)
  log10b ~ dunif(log10bmin, log10bmax)
  log10e ~ dnorm(meanlog10e, taulog10e)
  sigma ~ dunif(0, dmax/2)

  b <- pow(10, log10b)
  e <- pow(10, log10e)
  tau <- 1/pow(sigma, 2)
}
"

#' @title Loglogistic normal model with 4 parameters
#'
#' @description
#' A 4-parameters  log-logistic normal model for response data
#'
#' @details specification of priors (may be changed if needed) using parameters
#'
#' @noRd
#'
llnormal4.model <- "
model {
  for (k in 1:n) {
    mu[k] <- c + (d-c) / (1 + (xconc[k] / e)^b)
    measure[k] ~ dnorm(mu[k], tau)
    sim[k] ~ dnorm(mu[k], tau)
  }
  # specification of priors (may be changed if needed)
  d ~ dunif(0, dmax)
  log10c ~ dunif(-12, log(dmax)/log(10))
  log10b ~ dunif(log10bmin, log10bmax)
  log10e ~ dnorm(meanlog10e, taulog10e)
  sigma ~ dunif(0, dmax/2)

  c <- pow(10, log10c)
  b <- pow(10, log10b)
  e <- pow(10, log10e)
  tau <- 1/pow(sigma, 2)
}
"

#' @title Loglogistic gamma model
#'
#' @description
#' A 3-parameters  log-logistic gamma model for response data
#'
#' @details specification of priors (may be changed if needed) using parameters
#'
#' @noRd
#'
llgamma3.model <- "
model {
  for (k in 1:n) {
    mu[k] <- d / (1 + (xconc[k] / e)^b)
    measure[k] ~ dgamma(mu[k] / sigma, sigma)
    sim[k] ~ dgamma(mu[k] / sigma, sigma)
  }
  # specification of priors (may be changed if needed)
  d ~ dunif(0, dmax)
  log10b ~ dunif(log10bmin, log10bmax)
  log10e ~ dnorm(meanlog10e, taulog10e)
  sigma ~ dunif(0, dmax/2)

  b <- pow(10, log10b)
  e <- pow(10, log10e)
}
"

#' @title Loglogistic gamma model with 4 parameters
#'
#' @description
#' A 4-parameters  log-logistic gamma model for response data
#'
#' @details specification of priors (may be changed if needed) using parameters
#'
#' @noRd
#'
llgamma4.model <- "
model {
  for (k in 1:n) {
    mu[k] <- c + (d-c) / (1 + (xconc[k] / e)^b)
    measure[k] ~ dgamma(mu[k] / sigma, sigma)
    sim[k] ~ dgamma(mu[k] / sigma, sigma)
  }
  # specification of priors (may be changed if needed)
  d ~ dunif(0, dmax)
  log10c ~ dunif(-12, log(dmax)/log(10))
  log10b ~ dunif(log10bmin, log10bmax)
  log10e ~ dnorm(meanlog10e, taulog10e)
  sigma ~ dunif(0, dmax/2)

  c <- pow(10, log10c)
  b <- pow(10, log10b)
  e <- pow(10, log10e)
}
"

#' @title Loglogistic gamma model
#'
#' @description
#' A 3-parameters  log-logistic gamma model for response data
#'
#' @details specification of priors (may be changed if needed) using parameters
#'
#' @noRd
#'
llnormal_truncated3.model <- "
model {
  for (k in 1:n) {
    mu[k] <- d / (1 + (xconc[k] / e)^b)
    measure[k] ~ dnorm(mu[k], tau) T(0,)
    sim[k] ~ dnorm(mu[k], tau) T(0,)
  }
  # specification of priors (may be changed if needed)
  d ~ dunif(0, dmax)
  log10b ~ dunif(log10bmin, log10bmax)
  log10e ~ dnorm(meanlog10e, taulog10e)
  sigma ~ dunif(0, dmax/2)

  b <- pow(10, log10b)
  e <- pow(10, log10e)
  tau <- 1/pow(sigma, 2)
}
"

#' @title Loglogistic gamma model with 4 parameters
#'
#' @description
#' A 4-parameters  log-logistic gamma model for response data
#'
#' @details specification of priors (may be changed if needed) using parameters
#'
#' @noRd
#'
llnormal_truncated4.model <- "
model {
  for (k in 1:n) {
    mu[k] <- c + (d-c) / (1 + (xconc[k] / e)^b)
    measure[k] ~ dnorm(mu[k], tau) T(0,)
    sim[k] ~ dnorm(mu[k], tau) T(0,)
  }
  # specification of priors (may be changed if needed)
  d ~ dunif(0, dmax)
  log10c ~ dunif(-12, log(dmax)/log(10))
  log10b ~ dunif(log10bmin, log10bmax)
  log10e ~ dnorm(meanlog10e, taulog10e)
  sigma ~ dunif(0, dmax/2)

  c <- pow(10, log10c)
  b <- pow(10, log10b)
  e <- pow(10, log10e)
  tau <- 1/pow(sigma, 2)
}
"


