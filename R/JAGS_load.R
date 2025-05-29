#' @title Load JAGS models
#'
#' @description
#' Defined a  \code{\link[rjags]{jags.model}} which is used to create an object
#' representing a Bayesian graphical model, specified with a BUGS-language
#' description of the prior distribution, and a set of data.
#'
#' @param model.program character string containing a jags model description
#' @param data list of data created by \code{\link{binaryCreateJagsData}},
#' \code{\link{countCreateJagsData}} or \code{\link{continuousCreateJagsData}}
#' @param inits See \link[rjags]{jags.model} Optional specification of initial values.
#' @param n.chains Number of chains desired
#' @param n.adapt length of the adaptation phase
#' @param quiet silent option
#'
#' @return description
#'
#' @noRd
#'
#' @import rjags
#'
load.model.JAGS <- function(model.program,
                          data,
                          inits,
                          n.chains,
                          n.adapt,
                          quiet) {

    # load model text in a temporary file
    model.file <- tempfile() # temporary file address
    fileC <- file(model.file) # open connection
    writeLines(model.program, fileC) # write text in temporary file
    close(fileC) # close connection to temporary file

    # creation of the jags model
    model <- jags.model(file = model.file,
                        data = data,
                        inits = inits,
                        n.chains = n.chains,
                        n.adapt = n.adapt,
                        quiet = quiet)
    unlink(model.file)
    return(model)
}
