rm(list = ls())
if(!is.null(dev.list())) dev.off()   #### it clears the plots


library(geepack)
data("respiratory")
str(respiratory)
## Analysis using the "gee" package---Note for differenes when you use
## the "geepack" package.
## Note this is a marginal modeling
library(gee)
resp_gee <- gee(outcome ~ center + treat + sex + age + baseline, 
                id = interaction(center, id),
                data = respiratory, family = "binomial",         
                corstr = "exchangeable")
#### Note that the length of "id" should be the same as
####the number of observations.
#### That is why we considered id = interaction(center, id)
summary(resp_gee)
## Associated 95% CI for the treat
se <- summary(resp_gee)$coefficients["treatP", "Robust S.E."]
coef(resp_gee)["treatP"] + c(-1, 1) * se * qnorm(0.975)
## As you know in order to interpret to estimates you must compute
## exp of the estimates for the odds ratio. 


## Analysis using the "lme4" package
## Note this is a mixed-effects modeling---here, the random intercept model 

library(lme4)
## just to be consistent with the "gee" arguments.
id = interaction(respiratory$center, respiratory$id)
resp_glmer <- glmer(outcome ~ center + treat + sex + age + baseline + 
                   (1|id), family = binomial, data = respiratory)
summary(resp_glmer)
resp_glmer
