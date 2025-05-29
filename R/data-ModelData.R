#' @name ModelData
#'
#' @title Creates a data set for binary data analysis
#'
#' @description
#' The \code{data} argument contains the experimental data provided as
#' a \code{data.frame}. It as to satisfied requirement of \code{BinaryData},
#' \code{CounData} or \code{ContinuousData} as detailled below.
#' The function fails if \code{data} does not meet the
#' expected requirements.
#' Note that experimental data with time-variable exposure are not supported.
#'
#' - **\code{binaryData}** This function creates a \code{BinaryData} object
#' from experimental data. The resulting object
#' can then be used for plotting and model fitting. It can also be used
#' to generate \emph{individual-time} estimates. The \code{BinaryData} argument describes
#' experimental results from a survival (or mobility) toxicity test.
#' Each line of the \code{data.frame}
#' corresponds to one experimental measurement, that is for instance
#'  a number of alive
#' individuals at a given concentration at a given time point and in
#' a given replicate.
#'  Note that either the concentration
#' or the number of alive individuals may be missing. The data set is inferred
#' to be under constant exposure if the concentration is constant for each
#' replicate and systematically available.
#' Please run \code{\link{binaryDataCheck}} to ensure
#' \code{data} is well-formed.
#'
#' - **\code{countData}**: This function creates a \code{CountData} object from
#' experimental
#' data provided as a \code{data.frame}. The resulting object can then be used
#' for plotting and model fitting. The \code{CountData} class is a sub-class
#' of \code{BinaryData}, meaning that all functions and method available for
#' binary data analysis can be used with \code{CountData} objects.
#' Please run \code{\link{countDataCheck}} to ensure
#' \code{data} is well-formed.
#'
#' - **\code{continuousData}**: This function creates a \code{ContinuousData}
#' object from experimental data provided as a \code{data.frame}. The resulting
#'  object can then be used for plotting and model fitting.
#'  Each line of the \code{data.frame}.
#'  The function \code{continuousData} fails if
#' \code{data} does not meet the expected requirements.
#' Please run \code{\link{continuousDataCheck}} to ensure
#' \code{data} is well-formed.
#'
#' @param data a \code{data.frame} with specific requirement.
#'  - For \code{BinaryData}: it should be a \code{data.frame} containing the following four columns:
#' \itemize{
#' \item \code{replicate}: a vector of any class \code{numeric}, \code{character} or \code{factor} for replicate
#' identification. A given replicate value should identify the same group of
#' individuals followed in time
#' \item \code{conc}: a vector of class \code{numeric} with tested concentrations
#' (positive values, may contain NAs)
#' \item \code{time}: a vector of class \code{integer} with time points, minimal value must be 0
#' \item \code{Nsurv}: a vector of class \code{integer} providing the number of
#' alive individuals at each time point for each concentration and each replicate
#' (may contain NAs)
#' }
#'
#' - For \code{CountData}: it's a \code{data.frame} as expected by \code{BinaryData}
#'  containing one additional \code{Nrepro} column of class \code{integer} with positive
#' values only. This column should
#' provide the number of offspring produced since the last observation.
#'
#' - For \code{continuousData}: a \code{data.frame} containing the following
#' four columns:
#' \itemize{
#' \item \code{conc}: a vector of class \code{numeric} with tested concentrations
#' (positive values, may contain NAs)
#' \item \code{time}: a vector of class \code{integer} with time points, minimal value must be 0
#' \item \code{measure}: a vector of class \code{numeric} providing the measurement
#' (any quantitative continuous variable describing a measure on the
#'  organisms such as length/weight of organism or shoot length and dry weight
#'   for plants.)
#'   \item \code{replicate} (non mandatory): a vector of any class \code{numeric},
#'  \code{character} or \code{factor} for replicate
#' identification. A given replicate value should identify the same group of
#' individuals followed in time
#' }
#' @param type must be declared as 'binary', 'count' or 'continuous'.
#' @param \dots Further arguments to be passed to generic methods
#'
#' @return An object of class \code{BinaryData}, \code{CountData} or
#' \code{ContinuousData}.
#'
#' @keywords format
#' @keywords transformation
#'
#' @seealso \code{\link{binaryDataCheck}}
#' @seealso \code{\link{countDataCheck}}
#' @seealso \code{\link{continuousDataCheck}}
#'
#' @examples
#'
#' # Create an objet of class 'CountData'
#' d <- modelData(zinc, type = "count")
#' class(d)
#'
#' @export
#'
modelData <- function(data, type, ...){
    UseMethod("modelData")
}

#' @rdname ModelData
#' @export
modelData.data.frame <- function(data, type = NULL, ...){
    if (is.null(type)) {
        stop("'type' must be declared as 'binary', 'count' or 'continuous'.")
    }
    if (type == "binary") {
        data <- binaryData(data)
    }
    if (type == "count") {
        data <- countData(data)
    }
    if (type == "continuous") {
        data <- continuousData(data)
    }
    return(data)
}
