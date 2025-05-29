#' @name PlotRiskMeasure
#'
#' @title Plot an object \code{XCX}
#'
#' @description
#' Plot the median and 95\% credible interval of the x% Lethal or Effect
#'  Concentration.
#'
#' @param x an object of class \code{XCX}
#' @param xlab label of the x-axis
#' @param ylab label of the y-axis
#' @param main title of the graphic
#' @param \dots Further arguments to be passed to generic methods
#'
#' @return a plot of class  \code{\link[ggplot2]{ggplot}}
#'
#' @export
plot.XCX <- function(x, xlab = NULL, ylab = NULL, main = NULL, ...){

    df = as.data.frame(x$distribution)
    ls = as.list(df)
    lsdf = lapply(seq_along(ls), function(i){
        data.frame(value = ls[[i]], X = names(ls)[i])
    })
    dfplt <- do.call("rbind", lsdf)

    dfquant <- x$quantiles
    dfquant$X <- row.names(dfquant)

    plt <- ggplot() +
        theme_minimal() +
        labs(x = xlab, y = ylab, title = main) +
        geom_density(data = dfplt, aes(value), fill = fillci) +
        geom_vline(data = dfquant, mapping = aes(xintercept = Qinf95), linetype = "dashed", color = cinter) +
        geom_vline(data = dfquant, mapping = aes(xintercept = Qsup95), linetype = "dashed", color = cinter) +
        geom_vline(data = dfquant, mapping = aes(xintercept = median), color = colmed) +
        facet_wrap(~ X)

    return(plt)
}
