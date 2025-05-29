#' @name Summary
#'
#' @title Summary function for \code{PPC} object
#'
#' @description
#' Summary function on \code{PPC} object
#'
#' @export
#'
summary.PPC <- function(object, quiet = FALSE, ...){

    percent_in <- signif(sum(object$color) * 100 / nrow(object), digits = 3)

    if (!quiet) {
        cat("Summary: \n\n")
        cat("Percent of Observation in the 95% Credible Interval: ",
            percent_in)
    }
    invisible(list(percent_in = percent_in))
}
