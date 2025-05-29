#' @name ModelData
#'
#' @examples
#'
#' # (1) Load reproduction dataset
#' data(cadmium1)
#' # (2) Create an object of class "CountData"
#' dat <- countData(cadmium1)
#' class(dat)
#'
#' @export
countData <- function(data, ...){
    UseMethod("countData")
}

#' @name ModelData
#' @export
countData.data.frame <- function(data, ...) {

    # CHECKING
    tab_check <- countDataCheck(data, quiet = TRUE)
    if (nrow(tab_check) > 0) {
        stop("data not well-formed. See 'countDataCheck'.")
    }
    # PROCESSING
    data <- binaryData(data)
    data <- add_Nreprocumul(data)
    data <- add_Nindtime(data)
    rownames(data) <- NULL
    class(data) <- c("CountData", class(data))
    return(data)
}

#' @title Add \code{Nreprocumul} column
#'
#' @description
#' Add a column \code{Nreprocumul} for each replicate with the cumulative number
#' of Nrepro.
#'
#' @param data a data.frame succeeding \code{countDataCheck} function
#'
#' @noRd
#'
add_Nreprocumul <- function(data){
    subdata <- split(data, list(data$replicate, data$conc), drop = TRUE)
    ls <- lapply(subdata, function(d){
        d <- d[order(d$time), ]
        d$Nreprocumul <- cumsum(d$Nrepro)
        return(d)
    })
    df <- do.call("rbind", ls)
    return(df)
}


#' @title Add \code{Nindtime} column
#'
#' @description
#' Add a column \code{Nindtime} for each replicate with the cumulative number
#' of day of alive individuals.
#'
#' @param data a data.frame succeeding \code{survDataCheck} function
#'
#' @noRd
#'
add_Nindtime <- function(data){
    subdata <- split(data, list(data$replicate, data$conc), drop = TRUE)
    ls <- lapply(subdata, function(d){
        d <- d[order(d$time), ]
        diff_time <- diff(d$time)
        diff_Nsurv <- diff(d$Nsurv)
        NID_alive <- d$Nsurv[-nrow(d)] * diff_time
        NID_dead <- abs(diff_Nsurv) * diff_time / 2
        d$Nindtime  <- c(0, cumsum(NID_dead + NID_alive))
        return(d)
    })
    df <- do.call("rbind", ls)
    return(df)
}

#' @name CheckData
#'
#' @examples
#'
#' # Run the check data function
#' data(copper)
#' countDataCheck(copper)
#'
#' # Now we insert an error in the data set, by setting a non-zero number of
#' # offspring at some time, although there is no surviving individual in the
#' # replicate from the previous time point.
#' copper[148, "Nrepro"] <- as.integer(1)
#' countDataCheck(copper)
#'
#' @export
#'
countDataCheck <- function(data, quiet = FALSE) {

    ## 1. run the tests of survDataCheck
    tb <- binaryDataCheck(data, quiet = TRUE)
    ## 2. test if the column names "Nrepro" exists
    colname_check <- "Nrepro" %in% colnames(data)
    msg <- "The column Nrepro is missing."
    tb <- checking_table(tb, colname_check, msg)
    ## 3. test if Nrepro is integer
    n <- data$Nrepro
    integer <- all(n == as.integer(n))
    positivity <- sum(n < 0) == 0
    Nrepro_check <- all(c(integer, positivity))
    msg <- "Column 'Nrepro' must contain only positive (>=0) integer values."
    tb <- checking_table(tb, Nrepro_check, msg)
    ## 4. test Nrepro = 0 at time 0
    Nrepro_t0_check <- datatime0 <- data[data$time == 0, ]$Nrepro == 0
    msg <- "Nrepro should be 0 at time 0 for each concentration and each replicate."
    tb <- checking_table(tb, Nrepro_t0_check, msg)
    ## 5. At each time T for which Nsurv = 0, Nrepro = 0 at time T+1
    subdata <- split(data, list(data$replicate, data$conc), drop = TRUE)
    NN_check <- sapply(subdata, function(d) check_NsurvNrepro(d))
    msg = "'Nrepro' must be 0 at t+1 when Nsurv=0 at time t."
    tb <- checking_table(tb, NN_check, msg)
    if (quiet == FALSE) {
        if (nrow(tb) == 0) {
            message("Correct formatting")
        } else{
            message("Some mistake in formating. Look the message outputs.")
        }
    }
    return(tb)
}


#' @rdname CheckData
#'
#' @description
#' check if the pair \code{Nsurv} - \code{Nrepro} within a \code{time serie}
#' satisfies that \code{Nrepro} = 0 at t+1 when \code{Nsurv}=0 at t.
#'
#' @param data a data.frame
#'
#' @noRd
#'
check_NsurvNrepro <- function(data) {
    n = data$Nsurv
    s = (1:length(n))[n == 0]
    return(all(data$Nrepro[s[-1]] == 0))
}
