## ----property, include = FALSE------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  fig.width = 7
)

## ----setup, message=FALSE-----------------------------------------------------
library(morseDR)

## ----step1TT------------------------------------------------------------------
data(cadmium2)
binaryDataCheck(cadmium2)

## ----step2TT------------------------------------------------------------------
survData_Cd <- binaryData(cadmium2)
head(survData_Cd)

## ----step2TTm-----------------------------------------------------------------
survData_Cd <- modelData(cadmium2, type = 'binary')
head(survData_Cd)

## ----step3TT------------------------------------------------------------------
plot(survData_Cd, pool.replicate = FALSE)

## ----plotTT-------------------------------------------------------------------
plot(survData_Cd, concentration = 124,
     addlegend = FALSE,
     pool.replicate = FALSE)

## ----plotTTpool---------------------------------------------------------------
plot(survData_Cd, pool.replicate = TRUE)

## ----doseResponse_------------------------------------------------------------
# without pooling replicates
survData_Cd_DR_ <- doseResponse(survData_Cd, target.time = 21)

## ----plot_DoseResponse_-------------------------------------------------------
plot(survData_Cd_DR_)

## ----plot_DoseResponse_logScaled----------------------------------------------
plot(survData_Cd_DR_, log.scale = TRUE)

## ----doseResponse-------------------------------------------------------------
# pooling replicates
survData_Cd_DR <- doseResponse(survData_Cd, target.time = 21, pool.replicate = TRUE)

## ----plot_DoseResponse--------------------------------------------------------
plot(survData_Cd_DR)

## ----plot_DoseResponseNoLegend------------------------------------------------
# removing the legend
plot(survData_Cd_DR, addlegend = FALSE)

## ----plot_DoseResponseTT------------------------------------------------------
plot(survData_Cd_DR, target.time = 21, addlegend = TRUE)

## ----step4, results="hide", cache = TRUE--------------------------------------
survFit_Cd <- fit(survData_Cd, target.time = 21)

## ----summary_survFit_Cd-------------------------------------------------------
summary(survFit_Cd)

## ----LCxBin-------------------------------------------------------------------
LCx_Cd <- xcx(survFit_Cd, x = c(10, 20, 30, 40, 50))
LCx_Cd$quantiles

## ----LCxBinDistribution-------------------------------------------------------
head(LCx_Cd$distribution)

## ----plotLCxBin---------------------------------------------------------------
plot(LCx_Cd)

## ----step4plot, warning=FALSE-------------------------------------------------
plot(survFit_Cd, log.scale = TRUE, adddata = TRUE, addlegend = TRUE)

## ----wrongTT, results="hide", cache = TRUE------------------------------------
data("cadmium1")
doubtful_fit <- fit(binaryData(cadmium1), target.time = 21)

## ----wrongTTplot, warning=FALSE-----------------------------------------------
plot(doubtful_fit, log.scale = TRUE, adddata = TRUE, addlegend = TRUE)

## ----step5TT, results="hide"--------------------------------------------------
survFit_Cd_PPC <- ppc(survFit_Cd)

## ----plotSurvPPC--------------------------------------------------------------
plot(survFit_Cd_PPC)

## ----summarySurvPPC-----------------------------------------------------------
summary(survFit_Cd_PPC)

## ----pp_survCd----------------------------------------------------------------
Cd_PP <- priorPosterior(survFit_Cd)
plot(Cd_PP)

## ----ggpairs------------------------------------------------------------------
library(GGally)
Cd_posterior <- posterior(survFit_Cd)
ggpairs(Cd_posterior) 

## ----DIC----------------------------------------------------------------------
library(rjags)
fit <-  survFit_Cd
model <- rjags::jags.model(file = textConnection(fit$model.specification$model.text),
                    data = fit$jags.data,
                    n.chains = length(fit$mcmc),
                    n.adapt = 3000)

n_iter <- nrow(fit$mcmc[[1]])
dic.samples(model, n.iter = n_iter)

## ----WAIC---------------------------------------------------------------------
load.module("dic")
jags.samples(model, c("deviance", "WAIC"),
             type = "mean", n.iter = n_iter, thin = 10)

## ----countData----------------------------------------------------------------
# (1) load dataset
data(cadmium2)

# (2) check structure and integrity of the dataset
countDataCheck(cadmium2)

# (3) create a `reproData` object
dat <- countData(cadmium2)
head(dat)

## ----countDatam---------------------------------------------------------------
# (3) create a `reproData` object
dat <- modelData(cadmium2, type = 'count')
head(dat)

## ----countData_plt------------------------------------------------------------
# (4) represent the cumulated number of offspring as a function of time
plot(dat, concentration = 124, addlegend = TRUE, pool.replicate = FALSE)

## ----countData_DR-------------------------------------------------------------
# (5) represent the reproduction rate as a function of concentration
dat_DR <- doseResponse(dat, target.time = 28)
plot(dat_DR)

## ----bestFit, cache = TRUE----------------------------------------------------
# (7) fit a concentration-effect model at target-time
reproFit <- fit(dat, stoc.part = "bestfit", target.time = 21, quiet = TRUE)
summary(reproFit)

## ----ECx_count----------------------------------------------------------------
# (8) Get ECx estimates
ECx_count <- xcx(reproFit, x = c(10, 20, 30, 40, 50))
ECx_count$quantiles

## ----plotReproduction, warning=FALSE------------------------------------------
# (9) Plot the fit
plot(reproFit, log.scale = TRUE, adddata = TRUE, addlegend = TRUE)

## ----plotReproPPC-------------------------------------------------------------
# (10) PPC plot
ppcReproFit <- ppc(reproFit)
plot(ppcReproFit)

## ----summaryReproduction------------------------------------------------------
summary(reproFit)

## ----reproData----------------------------------------------------------------
dat <- countData(cadmium2)
plot(binaryData(dat))

## ----summaryReproPPC----------------------------------------------------------
ppcReproFit <- ppc(reproFit)
summary(ppcReproFit)

## ----plotPPCrepro-------------------------------------------------------------
plot(ppcReproFit)

## ----growthData---------------------------------------------------------------
data("cadmium_daphnia")
gCdd <- continuousData(cadmium_daphnia)
head(gCdd)

## ----growthDataModel----------------------------------------------------------
# Equivalent to the above line
gCdd <- modelData(cadmium_daphnia, type = "continuous")
head(gCdd)

## ----growthDataCheck----------------------------------------------------------
continuousDataCheck(cadmium_daphnia)

## ----plotGrowthData-----------------------------------------------------------
plot(gCdd)

## ----plotGrowthDataConc-------------------------------------------------------
plot(gCdd, concentration = 25)

## ----plotGrowthDataNoLegend---------------------------------------------------
plot(gCdd, addlegend = TRUE)

## ----DoseResponse-------------------------------------------------------------
gCdd_DR <- doseResponse(gCdd)
plot(gCdd_DR)

## ----DoseResponseLogScaled----------------------------------------------------
plot(gCdd_DR, log.scale = TRUE)

## ----DoseResponseTT-----------------------------------------------------------
gCdd_DRTT <- doseResponse(gCdd, target.time = 7)
plot(gCdd_DRTT)

## ----growthFit, cache = TRUE--------------------------------------------------
fit_gCdd <- fit(gCdd)

## ----plotGrowthFit------------------------------------------------------------
plot(fit_gCdd, adddata = TRUE)

## ----growthPPC----------------------------------------------------------------
# First calculate the PPC coordinates of the the predictions
ppc_gCdd <- ppc(fit_gCdd)

## ----plot_growthPPC-----------------------------------------------------------
# Plot the PPC
plot(ppc_gCdd)

## ----summaryGrowthPPC---------------------------------------------------------
summary(ppc_gCdd)

## ----xcx_ContinuousFitTT------------------------------------------------------
XCX_gCdd <- xcx(fit_gCdd, x = 50)
XCX_gCdd$quantiles

## ----xcx_ContinuousFitTT_plot-------------------------------------------------
plot(XCX_gCdd)

