rm(list = ls())
if(!is.null(dev.list())) dev.off()   #### it clears the plots

##### Growth curve data on an orthdontic measurement
library(lme4)
data(Orthodont,package="nlme")
str(Orthodont)
plot(Orthodont)

######  Residual analysis and plots

fm1 <- lmer(distance ~ age + (age|Subject), data=Orthodont)
## standardized residuals versus fitted values by gender
plot(fm1, resid(., scaled=TRUE) ~ fitted(.) | Sex, abline = 0)
## box-plots of residuals by Subject
plot(fm1, Subject ~ resid(., scaled=TRUE))
## observed versus fitted values by Subject
plot(fm1, distance ~ fitted(.) | Subject, abline = c(0,1))
## residuals by age, separated by Subject
plot(fm1, resid(., scaled=TRUE) ~ age | Sex, abline = 0)

library(lattice) ## needed for qqmath
qqmath(fm1, id=0.05)

####### Diagnostic plots for the linear mixed-effects fit
library(nlme)
fm1 <- lme(distance ~ age, Orthodont, random = ~ age | Subject)

# scatter plot of coefficients by gender, identifying unusual subjects
pairs(fm1, ~coef(., augFrame = TRUE) | Sex, id = 0.1, adj = -0.5)

# scatter plot of estimated random effects :
pairs(fm1, ~ranef(.))

# Variogram

library(joineR)
v <- variogram(indv = Orthodont$Subject, time = Orthodont$age, 
               Y = Orthodont$distance)
v
plot(v, ylim = c(0,20))

# ACF ######   What do you observe?
res <- residuals(fm1)
acf(res)

##### Some more plays with graphs

ggplot(data = Orthodont, aes(x = age, y = distance, group = Subject))+
  geom_point()+
  geom_line()+
  geom_text(aes(label = Orthodont$Sex), size = 3, color = "red")



ggplot(data = Orthodont, aes(x = age, y = distance, group = Subject,
          shape = Sex, linetype = Sex)) +
  geom_point() + 
  stat_smooth(method = "lm", se = FALSE)


ggplot(data = Orthodont, aes(x = age, y = distance, group = Subject)) +
  geom_line(aes(linetype = Sex), size = 1)


ggplot(data = Orthodont, aes(x = age, y = distance, group = Subject)) +
  geom_smooth(aes(group = Sex), se = FALSE,span = 5) 

