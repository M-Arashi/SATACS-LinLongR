### Cheese example
perceptron <- function(x,w,threshold){
  ifelse(sum(x*w) > threshold, 1, 0) 
}
#some examples
perceptron(x=	c(1,1,1),w=c(6,2,2),threshold = 5)

perceptron(x=	c(0,1,1),w=c(6,2,2),threshold = 5)

perceptron(x=	c(0,1,1),w=c(6,2,2),threshold = 3)


###
# install package
# install.packages("neuralnet")

# creating training data set
TKS=c(20)
CSS=c(90)
Placed=c(1)
# Here, you will combine multiple columns or features into a single set of data
df=data.frame(TKS,CSS,Placed)

# load library
require(neuralnet)

# fit neural network
nn=neuralnet(Placed~TKS+CSS,data=df, hidden=1,act.fct = "logistic",
             linear.output = FALSE)

# - Placed~TKS+CSS, Placed is label annd TKS and CSS are features.
# - df is dataframe,
# - hidden=1: represents single layer with 1 neurons respectively.
# - act.fct = "logistic" used for smoothing the result.
# - linear.ouput=FALSE: set FALSE for apply act.fct otherwise TRUE

# plot neural network
plot(nn)

# creating test set
TKS=c(30,40,85)
CSS=c(85,50,40)
test=data.frame(TKS,CSS)

## Prediction using neural network
Predict=compute(nn,test)
data <- Predict$neurons[[1]]
data %*% matrix(nn$weights[[1]][[1]],nc = 1)

Predict$net.result

# Converting probabilities into binary classes setting threshold level 0.5
prob <- Predict$net.result
pred <- ifelse(prob>0.5, 1, 0)
pred
# 
# nn$weights
# w <- matrix(nn$weights[[1]][[2:3,]],nc = 1)
# b <- nn$weights[[1]][[1]][1]
# b
# w
# w[1]*test[1,1] + w[2]*test[1,2] + b
# w[1]*test[2,1] + w[2]*test[2,2] + b
# w[1]*test[3,1] + w[2]*test[3,2] + b
