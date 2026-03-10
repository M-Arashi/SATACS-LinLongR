rm(list = ls())
library(lme4)
str(sleepstudy)
dim(sleepstudy)
head(sleepstudy)
require(lattice)
xyplot(Reaction ~ Days | Subject, sleepstudy, type = c("g","p","r"),
       index = function(x,y) coef(lm(y ~ x))[1],
       xlab = "Days of sleep deprivation",
       ylab = "Average reaction time (ms)", aspect = "xy")
(fm1 <- lmer(Reaction ~ Days + (Days|Subject), sleepstudy))
coef(fm1)
# Obtaining the p-values
library(lmerTest)
fm1 <- lmer(Reaction ~ Days + (Days|Subject), sleepstudy)
summary(fm1)
