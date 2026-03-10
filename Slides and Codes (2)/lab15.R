rm(list = ls())
if(!is.null(dev.list())) dev.off()   #### it clears the plots

library(nlmeU)
data(armd);str(armd)
head(armd)
dim(armd)


attach(armd)

library(matrixcalc)
treat.f <- as.numeric(treat.f)
treat.f_time <- hadamard.prod(treat.f, time)
## We need to add intercept; use rep() 
x <- cbind(rep(1,dim(armd)[1]),visual0, time, treat.f, treat.f_time)
## We need to add random intercept; use rep() 
z <- cbind(rep(1,dim(armd)[1]),time)
y <- visual
library(splmm)
fit <- splmm(x=x,y=y,z=z,grp=subject,lam1=0.01,
             lam2=0.01,penalty.b="lasso", penalty.L="lasso")
summary(fit)


detach(armd)
