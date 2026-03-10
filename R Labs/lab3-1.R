rm(list = ls())
alpha <- -2
beta <- 2
n <- c(3, 2, 3, 2, 3)
npeople <- length(n)
x <- matrix(rep(0, length=max(n) * npeople),ncol = npeople)
for (i in 1:npeople){ 
  x[1:n[i], i] <- runif(n[i], min = 1, max = 5)
  x[1:n[i], i] <- sort(x[1:n[i], i])
}
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
data1 <- data.frame(yall,peopleall,xall)
library(lme4)
model <- lmer(yall~xall+(1|peopleall), data = data1)
summary(model)
## standardized residuals versus fitted values by peopleall
plot(model, resid(., scaled=TRUE) ~ fitted(.) | peopleall, abline = 0)
## observed versus fitted values by peopleall
plot(model, yall ~ fitted(.) | peopleall, abline = c(0,1))
library(lattice) ## needed for qqmath
qqmath(model, id=0.05)
qqmath(ranef(model, condVar = TRUE), strip = FALSE)$peopleall


