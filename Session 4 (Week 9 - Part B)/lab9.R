rm(list = ls())
if(!is.null(dev.list())) dev.off()   #### it clears the plots

library(gee)
data(warpbreaks)
str(warpbreaks)

require(stats); require(graphics)
summary(warpbreaks)
opar <- par(mfrow = c(1, 2), oma = c(0, 0, 1.1, 0))
plot(breaks ~ tension, data = warpbreaks, col = "lightgray",
     varwidth = TRUE, subset = wool == "A", main = "Wool A")
plot(breaks ~ tension, data = warpbreaks, col = "lightgray",
     varwidth = TRUE, subset = wool == "B", main = "Wool B")
mtext("warpbreaks data", side = 3, outer = TRUE)
par(opar)
## Fixed-effects model
summary(fm1 <- lm(breaks ~ wool*tension, data = warpbreaks))
anova(fm1)

## marginal analysis of random effects model for wool---GEE
summary(gee(breaks ~ tension, id=wool, data=warpbreaks, corstr="exchangeable"))
## test for serial correlation in blocks
summary(gee(breaks ~ tension, id=wool, data=warpbreaks, corstr="AR-M", Mv=1))

if(require(MASS)) {
  data(OME)
  ## not fully appropriate link for these data.
  (fm <- gee(cbind(Correct, Trials-Correct) ~ Loud + Age + OME, id = ID,
             data = OME, family = binomial, corstr = "exchangeable"))
  summary(fm)
}

