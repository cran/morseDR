#' @name PlotData
#'
#' @title Plotting method for \code{BinaryData}, \code{CountData} or
#' \code{ContinuousData}.
#'
#' @description
#' This is the generic \code{plot} S3 method for the \code{BinaryData},
#' \code{CountData} and \code{ContinuousData} classes.
#'
#' - \code{BinaryData}: It plots the sum of non-failure (survivor, mobile)
#' individuals as a function of time.
#'
#' - \code{CountData}: It plots the cumulated number of offspring as a
#' function of time.
#'
#' - \code{Continuous}: It plots the continuous data as a
#' function of time by concentration panels.
#'
#' @param x an object of class \code{BinaryData}, \code{CountData} or
#' \code{ContinuousData}
#' @param xlab a label for the \eqn{X}-axis, by default \code{Time}
#' @param ylab a label for the \eqn{Y}-axis, by default
#'  \code{Sum of Non-Failure} for  \code{BinaryData},
#' \code{Cumulated Response} for \code{CountData}, and
#'  \code{Measure} for \code{ContinuousData}.
#' @param main title for the plot
#' @param concentration a numeric value corresponding to some concentration(s) in
#' \code{data}. If \code{concentration = NULL}, draws a plot for each concentration
#' @param pool.replicate if \code{TRUE}, the datapoints of each replicate are
#' summed for a same concentration
#' @param addlegend if \code{TRUE}, adds a default legend to the plot
#' @param \dots Further arguments to be passed to generic methods
#'
#' @keywords plot
#'
#' @return a plot of class \code{ggplot}
#'
#' @import ggplot2
#' @importFrom methods is
#' @importFrom stats aggregate
#'
NULL
