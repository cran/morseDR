#' @name PlotData
#' @export
plot.ContinuousData <- function(x,
                           xlab = "Time",
                           ylab = "Measure",
                           main = NULL,
                           concentration = NULL,
                           addlegend = FALSE, ...) {
    if (!is.null(concentration)) {
        if (!(concentration %in% x$conc)) {
            stop("The argument [concentration] should correspond to
                 one of the tested concentrations")
        } else{
            x <- x[x$conc == concentration, ]
        }
    }

    if (length(unique(x$conc)) == 1) {
        df <- ggplot(data = x, aes(x = time, y = measure))
    } else {
        df <- ggplot(data = x, aes(x = time, y = measure,
                                   color = factor(conc),
                                   group = conc))
    }

    fd <- df +
        theme_minimal() +
        geom_point() +
        ggtitle(main) +
        theme_minimal() +
        labs(x = xlab,
             y = ylab) +
        scale_color_hue("Concentration") +
        expand_limits(x = 0, y = 0) +
        facet_wrap(~ conc, ncol = 2)

    if (!addlegend) {
        fd <- fd + theme(legend.position = "none")
    }

    return(fd)
}

