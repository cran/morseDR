#' @name FitTT
#'
#' @title Fits a Bayesian concentration-response model for target-time
#'
#' @description
#'
#' - **binary data**: This function estimates the parameters of a
#'  concentration-response
#' model for target-time binary data analysis using Bayesian inference. In this model,
#' the rate of binary effect (survival or immobility) individuals at a given
#' time point (called target time) is modeled
#' as a function of the chemical compound concentration. The actual number of
#' surviving individuals is then modeled as a stochastic function of the survival
#' rate. Details of the model are presented in the
#' vignette accompanying the package.
#'
#' - **count data**: This function estimates the parameters of a concentration-effect model for
#' target-time reproduction analysis using Bayesian inference.
#' In this model the endpoint is the cumulated number of event
#' (like reproduction)  over time, with potential failure (death)
#'  all along the experiment. Particularly dedicated to reproduction
#'  data, because some individuals may die
#'  during the observation period, the
#' reproduction rate alone is not sufficient to account for the observed number
#' of offspring at a given time point. In addition, we need the time individuals have stayed alive
#' during this observation period. The \code{fit} function estimates the number
#' of individual-days in an experiment between its start and the target time.
#' This covariable is then used to estimate a relation between the chemical compound
#' concentration and the reproduction rate \emph{per individual-day}.
#' The \code{fit} function on \code{CountData} fits two models, one where inter-individual
#' variability is neglected ("Poisson" model) and one where it is taken into
#' account ("gamma-Poisson" model). When setting \code{stoc.part} to
#' \code{"bestfit"}, a model comparison procedure is used to choose between
#' both. More details are presented in the vignette accompanying the package.
#'
#' - **continuous data**: This function estimates the parameters of a
#'  concentration-response
#' model for target-time of any continuous data analysis using Bayesian inference.
#' This model is particularly well-suited for growth data.
#' Details of the model are presented in the vignette accompanying the package.
#' We can choose the stochastic part to be either "gamma" or "normal", with a
#' default to "gamma".
#'
#' @param data an object of class \code{BinaryData}, \code{CountData} or
#' \code{CountinuousData}
#' @param stoc.part a string for stochastic part. For "" model, the
#' \code{stoc.part} is \code{"gamma"} (default) but can be \code{"normal"}.
#' @param target.time the chosen endpoint to evaluate the effect of the chemical compound
#' concentration, by default the last time point available for
#' all concentrations
#' @param inits See \link[rjags]{jags.model}. Optional specification of initial values.
#' @param n.chains number of MCMC chains, the minimum required number of chains
#' is 2
#' @param n.adapt The number of iterations for adaptation.
#' See \link[rjags]{jags.model} for further details.
#' @param quiet if \code{TRUE}, does not print messages and progress bars from
#' JAGS
#' @param warning.print if \code{TRUE}, print the warnings in REPL
#' @param n.iter if \code{NA}, default, the number of iteration is estimated from
#' \code{raftery.diag} process, otherwise, set the n.iter provided.
#' @param low.asympt binary TRUE/FALSE. If TRUE, a parameter for the lower side of
#' the assymptote is compute in case of Continuous Data. Default is FALSE.
#' @param \dots Further arguments to be passed to generic methods
#'
#' @return
#' The function returns an object of class \code{FitTT} and
#' \code{BinaryFitTT}, which is a list with the following information:
#'
#' \describe{
#' \item{mcmc}{an object of class \code{mcmc.list} with the posterior
#' distribution}
#' \item{warnings}{a table with warning messages}
#' \item{parameters}{a list of parameter names used in the model}
#' \item{model.specification}{a set of parameters describing th model used}
#' \item{jags.data}{a list of the data passed to the JAGS model}
#' \item{original.data}{the \code{survData} object passed to the function}
#' \item{dataTT}{the dataset with which the parameters are estimated}
#' }
#'
#' @import rjags
#'
#' @export
#'
fit <- function(data, ...){
    UseMethod("fit")
}
