#' @name ModelData
#'
#' @examples
#'
#' # (1) Load the survival data set
#' data(zinc)
#' # (2) Create an objet of class 'BinaryData'
#' dat <- binaryData(zinc)
#' class(dat)
#'
#' @export
#'
binaryData <- function(data, ...){
    UseMethod("binaryData")
}

#' @name ModelData
#' @export
binaryData.data.frame <- function(data, ...){
    # CHECKING
    tab_check <- binaryDataCheck(data, quiet = TRUE)
    if (nrow(tab_check) > 0) {
        stop("data not well-formed. See 'survDataCheck'.")
    }
    # PROCESSING
    data <- add_Ninit(data)
    data$replicate = as.factor(data$replicate)
    rownames(data) <- NULL
    class(data) <- append("BinaryData", class(data))
    return(data)
}

#' @title Add Nsurv initial
#'
#' @description
#' Add a column Ninit for each replicate with the iniital number of survival
#' object the initial number of individuals in the corresponding replicate
#'
#' @param data a data.frame succeeding \code{survDataCheck} function
#'
#' @noRd
#'
add_Ninit <- function(data){
    subdata <- split(data, list(data$replicate, data$conc), drop = TRUE)
    if ("Ninit" %in% colnames(data)) {
        check_Ninit <- sapply(subdata, function(d){
            unique(d$Ninit) == max(d$Nsurv)
        })
        if (!all(check_Ninit)) {
            stop("A column name 'Ninit' has a wrong formatting.
                 If you remove the column, it will be reformatted.")
        }
    } else{
        ls_Ninit <- lapply(subdata, function(d){
            d$Ninit = max(d$Nsurv)
            return(d)
        })
        data <- do.call("rbind", ls_Ninit)
    }
    return(data)
}


#' @name CheckData
#' @export
binaryDataCheck <- function(data, quiet = FALSE) {

    tb <- data.frame(msg = character(0))
    # 0. check colnames
    REFCOLNAMES <- c("replicate","conc","time","Nsurv")
    valid_colnames <- REFCOLNAMES %in% names(data)
    if (!all(valid_colnames)) {
        msg = "Colnames are missing: 'replicate', 'conc', 'time', 'Nsurv'."
        tb <- rbind(tb, data.frame(msg = msg))
        return(tb)
    }
    # Next check are not depending to each other
    subdata <- split(data, list(data$replicate, data$conc), drop = TRUE)
    ## 1. check time is (1) numeric, (2) unique/replicate, (3) minimal value is 0.
    time_check <- sapply(subdata, function(d) check_time(d))
    msg = "'time' column must be numerical values,
    unique/replicate, and min 0 / replicate."
    tb <- checking_table(tb, time_check, msg)
    ## 2. assert concentrations are numeric
    concentration_check <- check_concentration(data)
    msg = "'conc' column must be positive numerical values"
    tb <- checking_table(tb, concentration_check, msg)
    ## 3. assert Nsurv contains integer and positive
    Nsurv_check <- check_Nsurv(data)
    msg = "Column 'Nsurv' must contain only integer values"
    tb <- checking_table(tb, Nsurv_check, msg)
    ## 4. assert Nsurv never increases with time and >0 at 0
    tN_check <- sapply(subdata, function(d) check_TimeNsurv(d))
    msg = "'Nsurv' must be >0 at t=0 and decrease with time."
    tb <- checking_table(tb, tN_check, msg)

    if (quiet == FALSE) {
        if (nrow(tb) == 0) {
            message("Correct formatting")
        } else{
            message("Some mistake in formating. Look the message outputs.")
        }
    }
    return(tb)
}

#' @name check
#'
#' @description
#' check if the \code{time} within a \code{time serie} is (1) numeric,
#' (2) unique, (3) minimal value is 0.
#'
#' @param data a data.frame
#'
#' @noRd
#'
check_time <- function(data){
    t <- data$time
    unicity <- length(unique(t)) == length(t)
    numeric <- is.numeric(t)
    minimal <- min(t) == 0
    positivity <- sum(t < 0) == 0
    return(all(c(unicity, numeric, minimal, positivity)))
}

#' @rdname check
#'
#' @description
#' check if the \code{concentration} is numeric and always positive.
#'
#' @param data a data.frame
#'
#' @noRd
#'
check_concentration <- function(data){
    c <- data$conc
    numeric <- is.numeric(c)
    positivity <- sum(c < 0) == 0
    return(all(c(numeric, positivity)))
}

#' @rdname check
#'
#' @description
#' check if the \code{Nsurv} is (1) integer and (2) always positive
#'
#' @param data a data.frame
#'
#' @noRd
#'
check_Nsurv <- function(data){
    n <- data$Nsurv
    integer <- all(n == as.integer(n))
    positivity <- sum(n < 0) == 0
    return(all(c(integer, positivity)))
}

#' @rdname check
#'
#' @description
#' check if the pair \code{time} - \code{Nsurv} within a \code{time serie}
#' satisfies (1) Nsurv at t=0 is >0, (2) decreasing
#'
#' @param data a data.frame
#'
#' @noRd
#'
check_TimeNsurv <- function(data){
    if (check_time(data) && check_Nsurv(data)) {
        n <- data$Nsurv
        t <- data$time
        nt0_sup0 <- n[t == 0] > 0
        decrease <- all(diff(n[order(t)]) <= 0)
        return(all(c(nt0_sup0, decrease)))
    } else{
        return(FALSE)
    }
}


#' @rdname check
#'
#' @description
#' add \code{msg} in a data.frame \code{tb} if \code{check} are not all TRUE.
#'
#' @param tb a data.frame to check
#' @param check binary vector of TRUE/FALSE
#' @param msg a message to add to the data.frame
#'
#' @noRd
#'
checking_table <- function(tb, check, msg){
    if (!all(check)) {
        tb <- rbind(tb, data.frame(msg = msg))
    }
    return(tb)
}

