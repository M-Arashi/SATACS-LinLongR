rm(list = ls())
if(!is.null(dev.list())) dev.off()   #### it clears the plots

library(plsgenomics)
data(leukemia)
str(leukemia)
head(leukaemia)
# how many samples and how many genes ?
dim(leukemia$X)

# how many samples of class 1 and 2, respectively ?
sum(leukemia$Y==1)
sum(leukemia$Y==2)

X <- leukemia$X
y <- leukemia$Y

library(lattice)
library(latticeExtra)
library(glmnet)
nfolds <- 10                    # Number of cross-validation folds.
alpha  <- 0.95                  # Elastic net mixing parameter.
lambda <- 10^(seq(0,-2,-0.05))  # Lambda sequence.

model <- glmnet(X,y,family = "binomial",lambda = lambda,alpha = alpha)
cv <- cv.glmnet(X,y,family = "binomial",type.measure = "class",
          alpha = alpha,nfolds = nfolds,lambda = lambda)


# the values of lambdaused in the ???ts
lambda <- cv$lambda
# The mean cross-validated error - a vector of length length(lambda)
cvm <- cv$cvm
# value of lambdathat gives minimum cvm
l_min <- cv$lambda.min
# largest value of lambda that is within 1 standard error
# of the smallest misclassification error
lambda.opt <- cv$lambda.1se

plot(model)
# xvar: What is on the X-axis. "norm" plots against the L1-norm of the coef???cients,
#"lambda" against the log-lambda sequence, 
#and "dev" against the percent deviance explained.
plot(model, xvar="lambda",label=TRUE) # label the curves with variable sequence numbers
#coef(model)
plot(cv)
coef<- coef(cv, s = "lambda.min")
coef
### returns nonzero coefs
rownames(coef(cv, s = 'lambda.min'))[coef(cv, s = 'lambda.min')[,1]!= 0] 
