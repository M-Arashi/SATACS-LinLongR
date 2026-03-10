rm(list = ls())
if(!is.null(dev.list())) dev.off()   #### it clears the plots

library(nlmeU)
require(nlme)

data(armd)
lm2.form <- formula(visual ~ visual0 + time + treat.f + treat.f:time)
fm16.1 <- lme(lm2.form,random = ~1|subject, data = armd) 


(fm16.2 <- update(fm16.1,weights = varPower(form = ~ time),data = armd))

### Diagnostics 
#Residual plot of conditional Pearson residuals
plot(fm16.2)

plot(fm16.2, resid(., type = "pearson") ~ time | treat.f,id = 0.05)
#Plots (and boxplots) of Pearson residuals per time and treatment
library(lattice)
bwplot(resid(fm16.2, type = "p") ~ time.f | treat.f,data = armd)
## I did not include "panel = panel.bwxplot2" due to the complexity of the R code

#Normal Q-Q plots of Pearson residuals and predicted random effects

qqnorm(fm16.2, ~resid(.) | time.f)
qqnorm(fm16.2, ~ranef(.)) 

# Predicted visual acuity values for model 2
aug.Pred <- augPred(fm16.2,
            primary = ~time, # Primary covariate
            level = 0:1, # Marginal(0) and subj.-spec.(1)
            length.out = 2)
plot(aug.Pred, layout = c(4, 4, 1), 
       key = list(lines = list(lty = c(1,2)),
       text = list(c("Marginal", "Subject-specific")),
       columns = 2))
