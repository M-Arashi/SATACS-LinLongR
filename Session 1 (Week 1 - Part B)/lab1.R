# Clear the Environment
rm(list = ls())
# Generating a high-dimensional MLR model and estimate the parameters
n <- 50
p <- 60
# Generate X matrix n*p
X <- matrix(0, n, p)
for (i in 1:p){
  X[,i]= rnorm(n)
}
# Generate vector parameter beta
beta <- c(runif(p,-1,1))
# Generate the response 
y<-X%*%beta+rnorm(n,0,1)
#Checking independency

model <- lm(y ~ X) 
summary(model)

# We pseudo or Moore-Penrose generalized inverse of X'X 
library("pracma")
betahat <- pinv(t(X)%*%X)%*%t(X)%*%y

# See also ginv
# Inference is not easy! 
