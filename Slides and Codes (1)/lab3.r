rm(list = ls())
alpha <- -2
beta <- 2
# We assume we have 5 people (groups) and repeat measures n_1=10, n_2=15
# n_3=12, n_4=15, and n_5=12 times. 
# Thus, repeat measure times for 5 people
n <- c(10, 15, 12, 15, 12)
npeople <- length(n)
# generate fixed-effect covariates
x <- matrix(rep(0, length=max(n) * npeople),ncol = npeople)
for (i in 1:npeople){ 
  x[1:n[i], i] <- runif(n[i], min = 1, max = 5)
  x[1:n[i], i] <- sort(x[1:n[i], i])
}
# random effect
ai <- rnorm(npeople, mean = 0, sd = 1) 
xall <- NULL
yall <- NULL
peopleall <- NULL
for (i in 1:npeople){
  xall <- c(xall, x[1:n[i], i]) # combine x
  # generate y
  y <- rep(alpha + ai[i], length = n[i]) +
    beta * x[1:n[i],i] +
    rnorm(n[i], mean = 0, sd = 2) # noise
  yall <- c(yall, y) # combine y
  people <- rep(i, length = n[i])
  peopleall <- c(peopleall, people)
}
xall
yall
peopleall
# Dataset
data1 <- data.frame(yall,peopleall,xall)

library(nlme)
lme1 <- lme(yall~xall,random=~1|peopleall,data=data1)
summary(lme1)
ranef(lme1)
plot(ranef(lme1), aug = TRUE)
qqnorm(lme1, abline = c(0,1))
## normal plots of random-effects
qqnorm(lme1, ~ranef(.))
