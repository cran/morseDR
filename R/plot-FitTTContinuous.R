#' @name PlotFitTT
#'
#' @export
plot.ContinuousFitTT <- function(x,
                           xlab = "Concentration",
                           ylab = "Measure",
                           main = NULL,
                           display.conc = NULL,
                           spaghetti = FALSE,
                           adddata = FALSE,
                           addlegend = FALSE,
                           log.scale = FALSE, ...) {

    dataTT <- x$dataTT
    # add binomial
    dataTT <- add_t_test(dataTT)
    dataTT$legend_CIobs = "Confidence intervals"
    dataTT$legend_observation = "Observed values"
    # display.conc
    if (is.null(display.conc)) {
        if (log.scale) {
            obs_conc <- dataTT$conc[dataTT$conc != 0]
            conc <- log10(obs_conc)
            s_conc <- seq(min(conc), max(conc), length.out = 100)
            display.conc <- 10^s_conc
        } else{
            obs_conc <- dataTT$conc
            s_conc <- seq(min(obs_conc), max(obs_conc), length.out = 100)
            display.conc <- s_conc
        }
    } else {
        obs_conc <- dataTT$conc
    }
    # compute predictions
    predictTT <- predict(x, display.conc)

    df_quantile <- predictTT$quantile
    df_quantile$conc <- predictTT$display.conc

    df_quantile$legend_median = "loglogistic"
    df_quantile$legend_CI = "Credible interval"
    plt <- ggplot() +
        theme_minimal() +
        labs(x = xlab, y = ylab, title = main) +
        scale_shape_manual(name = "", values = 19) +
        scale_color_manual(name = "", values = "black") +
        scale_linetype_manual(name = "",
                              values = c("loglogistic" = 1, "Credible interval" = 2))

    # 1. spaghetti
    if (spaghetti) {
        len_mcmc <- length(predictTT$mcmc[[1]])
        sample_range <- sample(1:len_mcmc, 500, replace = FALSE)
        ls_spaghetti <- lapply(seq_along(predictTT$mcmc), function(i){
            sampled <- predictTT$mcmc[[i]][sample_range]
            data.frame(
                Psurv = sampled,
                line = 1:length(sampled),
                conc = predictTT$display.conc[[i]])
        })
        df_spaghetti <- as.data.frame(do.call("rbind", ls_spaghetti))

        plt <- plt +
            geom_line(data = df_spaghetti,
                      aes(x = conc, y = Psurv, group = line),
                      color = fillci,  alpha = 0.1)

    } else{
        plt <- plt +
            geom_ribbon(data = df_quantile,
                        aes(x = conc, ymin = qinf95, ymax = qsup95),
                        linetype = 2, fill = fillci, alpha = 0.5, color = NA)
    }
    # 2. median line
    plt <- plt +
        geom_line(data = df_quantile,
                  aes(x = conc, y = q50, linetype = legend_median),
                  color = colmed) +
        geom_line(data = df_quantile,
                  aes(x = conc, y = qinf95, linetype = legend_CI),
                  color = colmed) +
        geom_line(data = df_quantile,
                  aes(x = conc, y = qsup95, linetype = legend_CI),
                  color = colmed)
    # 3. log-scaling
    if (log.scale) {
        plt <- plt + scale_x_log10(breaks = obs_conc)
    } else {
        plt <- plt + scale_x_continuous(breaks = obs_conc)
    }

    # 4. add data
    if (adddata) {
        plt <- plt +
            geom_point(data = dataTT,
                       aes(x = conc, y = measure,
                           shape = legend_observation),
                       color = "black") +
            geom_linerange(
                data = dataTT,
                aes(x = conc, ymin = qinf95, ymax = qsup95,
                    color = legend_CIobs))
    }
    # 5. legend
    if (!addlegend) {
        plt <- plt + theme(legend.position = "none")
    }
    return(plt)
}
