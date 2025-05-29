#' @rdname ModelData
#'
#' @examples
#'
#' # (1) Load the data set
#' data(chlordan_daphnia)
#' # (2) Create an objet of class 'continuousData'
#' dat <- continuousData(chlordan_daphnia)
#' class(dat)
#'
#' @export
#'
continuousData <- function(data, ...){
    UseMethod("continuousData")
}

#' @rdname ModelData
#' @export
#'
continuousData.data.frame <- function(data, ...){
    # CHECKING
    tab_check <- continuousDataCheck(data, quiet = TRUE)
    if (nrow(tab_check) > 0) {
        stop("data not well-formed. See 'continuousDataCheck'.")
    }
    # PROCESSING
    rownames(data) <- NULL
    class(data) <- append("ContinuousData", class(data))
    return(data)
}

#' @name CheckData
#'
#' @examples
#' data(chlordan_daphnia)
#' continuousDataCheck(chlordan_daphnia)
#'
#' @export
continuousDataCheck <- function(data, quiet = FALSE) {

    tb <- data.frame(msg = character(0))
    # 0. check colnames
    REFCOLNAMES <- c("conc","time","measure")
    valid_colnames <- REFCOLNAMES %in% names(data)
    if (!all(valid_colnames)) {
        msg = "Colnames are missing: 'conc', 'time', 'measure'."
        tb <- rbind(tb, data.frame(msg = msg))
        return(tb)
    }
    # 1. measure are not NA.
    if (any(is.na(data$measure))) {
        stop("Must have no NA value in 'measure'.")
    }
    # 2. time, are >=0 and numeric.
    numeric <- all(is.numeric(data$time), is.numeric(data$conc), is.numeric(data$measure))
    positivity <- all(data$time >= 0, data$conc >= 0, data$measure >= 0)
    msg = "'time', 'conc' and 'measure' must be numeric and >=0."
    tb <- checking_table(tb, c(numeric, positivity), msg)


    if (quiet == FALSE) {
        if (nrow(tb) == 0) {
            message("Correct formatting")
        } else{
            message("Some mistake in formating. Look the message outputs.")
        }
    }
    return(tb)

}
