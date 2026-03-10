rm(list = ls())
if(!is.null(dev.list())) dev.off()   #### it clears the plots

library(geepack)
data("respiratory")
str(respiratory)

gee.ind <- geeglm(outcome ~ center + treat + age + baseline, data=respiratory, id=id,
                  family=binomial(), corstr="independence")

coef(gee.ind)
vcov(gee.ind)
summary(gee.ind)
coef(summary(gee.ind))

gee.exc <- geeglm(outcome ~ center + treat + age + baseline, data=respiratory, id=id,
                  family=binomial(), corstr="exchangeable")
gee.uns <- geeglm(outcome ~ center + treat + age + baseline, data=respiratory, id=id,
                  family=binomial(), corstr="unstructured")
gee.ar1 <- geeglm(outcome ~ center + treat + age + baseline, data=respiratory, id=id,
                  family=binomial(), corstr="ar1")

mlist <- list(gee.ind, gee.exc, gee.uns, gee.ar1)
do.call(rbind, lapply(mlist, QIC))
lapply(mlist, tidy)


