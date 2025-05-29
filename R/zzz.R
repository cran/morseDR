colmed = "#EE9802"
fillci = "#E2D4BB"
cinter = "#936005"

cexpmin = "#30D0FF"
cexpmax = "#005872"


# HACK TO REMOVE NOTES FOR GLOBAL VARIABLES (mainly due to ggplot2)
globalVariables(unique(
    c("time", "measure", "conc", "Psurv", "line", "qinf95", "qsup95", "q50",
      "legend_median", "legend_CI", "legend_observation", "legend_CIobs",
      "Nreprocumul", "NID", "response", "legend_binomial", "value", "pp",
      "Qinf95", "Qsup95", "color")))
