rm(list = ls())
library(lme4)
str(sleepstudy)
require(lattice)
###### random intercept and random slope model --- general LMM
model1 <- lmer(Reaction ~ Days + (Days|Subject), sleepstudy)
###### random intercept model
model2 <- lmer(Reaction ~ Days + (1|Subject), sleepstudy)
###### fixed-effects model 
model3 <- lm(Reaction ~ Days + Subject, sleepstudy)
anova(model1, model2, model3)
