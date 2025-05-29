#' @name CheckData
#'
#' @title Checking object structure for analysis
#'
#' @description
#' Checks if an object can be used to perform data analysis.
#'
#' - \code{binaryDataCheck}: the function can be used to check if an object
#' containing survival data is formatted according to the expectations of the
#' \code{BinaryData} function.
#'
#' - \code{continuousDataCheck}: the function can be used to check if an object
#' containing survival data is formatted according to the expectations of the
#' \code{\link{continuousData}} function.
#'
#' - \code{countDataCheck}: the function can be used to check if an object
#' containing data from a reproduction toxicity assay meets the expectations
#' of the function \code{\link{countData}}.
#' The \code{countDataCheck} performs the same checking than
#' \code{\link{binaryDataCheck}} plus additional ones that are specific to
#' reproduction data.
#'
#' @param data Any object, but usually a \code{list} or a \code{data.frame}.
#' @param quiet Binary. Default is \code{False}. If \code{True} it returns messages
#' of the checking function.
#'
#' @return
#' The function returns a \code{data.frame} with message describing the error in the
#' formatting of the data. When no error is detected the object is empty.
#'
#' - For \code{\link{countDataCheck}}, the function returns a \code{data.frame} similar
#'  to the one returned by \code{\link{binaryDataCheck}},
#'  except that it may contain the following additional error \code{id}s:
#' \itemize{
#' \item \code{NreproInteger}: column \code{Nrepro} contains values of class other than \code{integer}
#' \item \code{Nrepro0T0}: \code{Nrepro} is not 0 at time 0 for each concentration and each replicate
#' \item \code{Nsurvt0Nreprotp1P}: at a given time \eqn{T}, the number of
#' alive individuals is null and the number of collected offspring is not null
#' for the same replicate and the same concentration at time \eqn{T+1}
#' }
#'
#' @seealso \code{\link{binaryData}}
#' @seealso \code{\link{countData}}
#' @seealso \code{\link{continuousData}}
#'
NULL
