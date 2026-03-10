rm(list = ls())
if(!is.null(dev.list())) dev.off()   #### it clears the plots

library(lars)
data(diabetes)
str(diabetes)
### Note: x is a matrix with 10 columns (predictor variables)
### We fit lasso 
diabetes.lasso = lars(diabetes$x, diabetes$y,
                      type='lasso')
plot(diabetes.lasso, xvar = "norm")
### we could use different options xvar= c("norm", "df", "arc.length", "step")

### Computing the K-fold cross-validated mean squared prediction error
cv.lars(diabetes$x, diabetes$y, K=10, type='lasso')


#### Applying the package "glmnet"
library(glmnet)
gfit <- glmnet(diabetes$x, diabetes$y)
plot(gfit)
CV <- cv.glmnet(diabetes$x, diabetes$y)
plot(CV)
# lambda.ise: largest value of lambda that is within 1 standard error
# of the smallest misclassification error
beta.hat = coef(gfit, s=CV$lambda.1se)
beta.hat
### Number of non-zero coefficients
sum(abs(beta.hat[,1]) >0)
cv.glmnet(diabetes$x, diabetes$y)$lambda.1se
