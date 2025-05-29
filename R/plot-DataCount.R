#' @name PlotData
#' @export
plot.CountData <- function(x,
                           xlab = "Time",
                           ylab = "Cumulated Response",
                           main = NULL,
                           concentration = NULL,
                           pool.replicate = FALSE,
                           addlegend = FALSE, ...) {
    if (pool.replicate) {
        x <- cbind(stats::aggregate(Nsurv ~ time + conc, x, sum), replicate = 1)
    }
    if (!is.null(concentration)) {
        if (!(concentration %in% x$conc)) {
            stop("The argument [concentration] should correspond to
                 one of the tested concentrations")
        } else{
            x <- x[x$conc == concentration, ]
        }
    }

    if (length(unique(x$replicate)) == 1) {
        df <- ggplot(data = x, aes(x = time, y = Nreprocumul))
    } else {
        df <- ggplot(data = x, aes(x = time, y = Nreprocumul,
                                   color = factor(replicate),
                                   group = replicate))
    }

    fd <- df +
        theme_minimal() +
        geom_line() +
        geom_point() +
        ggtitle(main) +
        theme_minimal() +
        labs(x = xlab,
             y = ylab) +
        scale_color_hue("Replicate") +
        expand_limits(x = 0, y = 0) +
        facet_wrap(~ conc, ncol = 2)

    if (!addlegend) {
        fd <- fd + theme(legend.position = "none")
    }

    return(fd)
}

