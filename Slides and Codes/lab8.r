rm(list = ls())
if(!is.null(dev.list())) dev.off()   #### it clears the plots

library(GLMsData)
data(quilpie); names(quilpie)
str(quilpie)
# To see exact zeros
head(quilpie)
# How many months with exactly zero rainfall?
sum( quilpie$Rain==0 ) 
########################### Estimation of xi  #############################
library(tweedie)
# Since we consider modeling the total July rainfall as a function 
#of the SOI phase, we define Phase as a factor
quilpie$Phase <- factor(quilpie$Phase) # Declare Phase as a factor

out <- tweedie.profile( Rain ~ Phase, do.plot=TRUE, data=quilpie)
names(out)

# The index parameter, xi
xi.est <- out$xi.max
c( "MLE of xi" = xi.est, "CI for xi" = out$ci )
# Phi
c("MLE of phi"=out$phi.max)

### Fitting Tweedie GLM with variance function var.power
xi.est <- round(xi.est, 2); xi.est
library(statmod)
m.quilpie <- glm( Rain ~ Phase, data=quilpie,
                    family=tweedie(var.power=xi.est, link.power=0) )
printCoefmat(coef(summary(m.quilpie)))

### Model diagnostics 
dres <- resid(m.quilpie) # The default residual
pres <- resid(m.quilpie, type="pearson")
qres <- qresid(m.quilpie) # Quantile resids
qqnorm(pres, main="Pearson residuals", las=1); qqline(pres)
qqnorm(qres, main="Quantile residuals (set 1)", las=1); qqline(qres)
#The Q-Q plot of the quantile residuals for these data
#suggest the model is adequate. 

plot( qres ~ fitted(m.quilpie), las=1, 
      xlab="Fitted values", ylab="Quantile residuals" )
plot( qresid(m.quilpie) ~ factor(quilpie$Phase), las=1,
        xlab="Phase", ylab="Quantile residuals" )

##### Modelled probability of P(Y=0)

new.phase <- factor( c(1, 2, 3, 4, 5) )
mu.phase <- predict(m.quilpie, newdata=data.frame(Phase=new.phase),
                      type="response")
names(mu.phase) <- paste("Phase", 1:5)
mu.phase

phi.mle <- out$phi.max
pi0 <- exp( -mu.phase^(2 - xi.est) / (phi.mle * (2 - xi.est) ) )
## Observed probability of P(Y=0)
prop0 <- tapply(quilpie$Rain, quilpie$Phase,
                    function(x){sum(x==0)/length(x)})
plot( pi0 ~ prop0, xlab="Proportion of zeros in data", ylim=c(0, 1),
          ylab="Expected prob. of zero rainfall", las=1 )
abline(0, 1, lwd=2) # The line of equality
text(prop0, pi0, # Adds labels to the points
       labels=paste("Phase", levels(quilpie$Phase)),
       pos=c(2, 4, 1, 4, 3)) # These position the labels; see ?text
#The proportion of months with zero rainfall are predicted with reasonable
#accuracy. Thus, the Tweedie GLM seems a useful model for 
#the total July rainfall in Quilpie.


