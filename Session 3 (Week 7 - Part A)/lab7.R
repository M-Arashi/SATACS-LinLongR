rm(list = ls())
if(!is.null(dev.list())) dev.off()   #### it clears the plots

library(nlmeU)

##### long format for longitudinal analysis
data(armd);str(armd);summary(armd);names(armd)
## In the long format, for each patient, there
## are multiple records containing visual acuity measurements 
## for separate visits.
## The variable "visual" contains the value of the visual acuity measurement 
## at the particular visit.
lm2.form <- formula(visual ~ visual0 + time + treat.f + treat.f:time)
require(nlme)
(fm16.1 <- lme(lm2.form, random = ~1|subject, data = armd)) 
printCoefmat(summary(fm16.1)$tTable,has.Pvalue = TRUE, P.values = TRUE)


# The estimated variance-covariance matrices for random effects
#(D) and residual errors (Sigma_i) for the model

#For D
getVarCov(fm16.1, individual = "2")  ### for the second individual, i.e., subject==2; it isn't important 
VarCorr(fm16.1)
#For Sigma_i 
getVarCov(fm16.1,type = "conditional",individual = "2")

#The estimated marginal variance-covariance matrix and the
#corresponding correlation matrix for the model as in (2.20)
(fm16.1cov <- getVarCov(fm16.1,type = "marginal",individual = "2"))
#Correlation
(cov2cor(fm16.1cov[[1]]))
