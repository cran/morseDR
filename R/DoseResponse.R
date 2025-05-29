#' @title Prepare data for Dose-Reponse
#'
#' @description
#' Reshape data for using function on Dose-Reponse data#'
#'
#' @name DoseResponse
#'
#' @param data an object used to select a method `doseResponse`
#' @param target.time Numeric. Default is \code{NULL}. By default, the last time
#' is considerd as the target-time. Otherwise, the argument set the target time.
#' @param pool.replicate Binary. Default is \code{FALSE}. IF \code{TRUE}, data
#' are summed according to groupd of the same \code{time} and \code{conc}.
#' @param \dots Further arguments to be passed to generic methods
#'
#' @return an object of class \code{DoseResponse}
#'
#' @export
#'
doseResponse <- function(data, ...){
    UseMethod("doseResponse")
}

#' @rdname DoseResponse
#' @export
doseResponse.BinaryData <- function(data, target.time = NULL, pool.replicate = FALSE, ...){
    if (is.null(target.time)) {
        target.time <- max(data$time)
    }
    if (any(!target.time %in% data$time)) {
        stop("[target.time] is not one of the possible time!")
    }
    df_list <- lapply(target.time, function(time) {
        data <- data[data$time %in% time, ]
        if (pool.replicate) {
                data <- aggregate(cbind(Nsurv, Ninit) ~ time + conc, data, sum)
        }
        data$response <- data$Nsurv / data$Ninit
        df <- add_binomial(data)
        df$time <- time
        return(df)
    })
    df_combined <- do.call(rbind, df_list)
    class(df_combined) <- append(c("BinaryDoseResponse", "DoseResponse"), class(df_combined))
    return(df_combined)
}

#' @rdname DoseResponse
#' @export
doseResponse.CountData <- function(data, target.time = NULL, pool.replicate = FALSE, ...){
    if (is.null(target.time)) {
        target.time <- max(data$time)
    }
    if (any(!target.time %in% data$time)) {
        stop("[target.time] is not one of the possible time!")
    }
    df_list <- lapply(target.time, function(time) {
        data <- data[data$time %in% time, ]
        if (pool.replicate) {
            data <- aggregate(
                cbind(Nsurv, Ninit, Nrepro, Nreprocumul, Nindtime) ~ time + conc,
                data, sum)
        }
        data$response <- data$Nreprocumul / data$Nindtime
        df <- add_poisson(data)
        df$time <- time
        return(df)
    })
    df_combined <- do.call(rbind, df_list)
    class(df_combined) <- append(c("CountDoseResponse", "DoseResponse"), class(df_combined))
    return(df_combined)
}

#' @rdname DoseResponse
#' @export
doseResponse.ContinuousData <- function(data, target.time = NULL, pool.replicate = FALSE, ...){
    if (is.null(target.time)) {
        target.time <- max(data$time)
    }
    if (any(!target.time %in% data$time)) {
        stop("[target.time] is not one of the possible time !")
    }
    df_list <- lapply(target.time, function(time) {
        data <- data[data$time %in% time, ]
        if (pool.replicate) {
            data <- aggregate(response ~ time + conc, data, sum)
        }
        data$response <- data$measure
        df <- add_t_test(data)
        df$time <- time
        return(df)
    })
    df_combined <- do.call(rbind, df_list)
    class(df_combined) <- append(c("ContinuousDoseResponse", "DoseResponse"), class(df_combined))
    return(df_combined)
}


