#' @param x an object of class \code{PriorPosterior}
#' @param xlab label of the x-axis
#' @param ylab label of the y-axis
#' @param main title of the graphic
#'
#' @name PriorPosterior
#'
#' @export
plot.PriorPosterior <- function(x,
                                xlab = "value",
                                ylab = NULL,
                                main = NULL,
                                ...){

    dfr <- x[, colnames(x) != "pp"]
    coldfr = colnames(dfr)

    df_melt <- data.frame(
        value = do.call("c", lapply(1:ncol(dfr), function(i) dfr[,i])),
        name = rep(coldfr, each = nrow(dfr)),
        pp = rep(x$pp, ncol(dfr))
    )
    df_melt$pp <- factor(df_melt$pp, levels = c("prior", "posterior"))

    plt <- ggplot(data = df_melt) +
        theme_minimal() +
        labs(x = xlab, y = ylab, title = main) +
        scale_fill_manual(
            name = NULL,
            labels = c("prior", "posterior"),
            values = c("grey80", colmed)) +
        geom_density(aes(value, fill = pp), color = NA) +
        facet_wrap(~ name, scales = "free")

    return(plt)
}
