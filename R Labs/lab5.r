rm(list = ls())

##### Example 1  
library(SemiPar)
data(pig.weights)
str(pig.weights)
library(lattice)
xyplot(weight~num.weeks,data=pig.weights,groups=id.num,type="b"
       , ylab = "Weight(kg)", xlab = "Time(weeks)")

##### Example 2

library(catdata)
data(aids)
str(aids)
library(ggplot2)## Note that "ggplot2" allows adding "+"
library(gridExtra)
#### For dot-plot
p1=ggplot(data = aids, aes(x = time, y = cd4, group = person))+
  geom_point(size = 0.9,color= 16)+
  xlab("Years since seroconversion") + ylab("CD4 cell number")
p1
#### For line-plot
p2=ggplot(data = aids, aes(x = time, y = cd4, group = person))+
  geom_point(size = 0.9,color= 16)+
  geom_line(color= 16)+
  geom_smooth(aes(group = 1), se = FALSE,span = 0.1)+
  xlab("Years since seroconversion") + ylab("CD4 cell number")
p2
#### For red lines on a subset of observations
p3=ggplot(data = aids, aes(x = time, y = cd4, group = person))+
  geom_point(size = 0.9,color= 16)+
  geom_line(color= "black")+
  geom_line(data = subset(aids, aids$person %in% unique(aids$person)[1:10]), 
            color = "red", size = 1)+
  geom_smooth(aes(group = 1), se = FALSE,span = 0.1)+
  xlab("Years since seroconversion") + ylab("CD4 cell number")
p3


#### Let's back to Example 1 --- the big weight data
ggplot(pig.weights, aes(x = num.weeks, y = weight, group = id.num)) + 
  geom_point() + geom_path(alpha = .4) +
  labs(x = "Week", y = "Weight") + 
  stat_smooth(method = "lm", aes(group = NULL), se = FALSE, color = "red", size = 1.1)
### The red line shows the mean profile. 


###### Example 3

library(nlme)
library(lattice)
data(BodyWeight)
print(names(BodyWeight))
str(BodyWeight)
BodyWeight <- groupedData(weight ~ Time|Rat, data=BodyWeight, 
                          outer= ~ Diet)
trellis.device(color=F)
rat.plot <- plot(BodyWeight,outer = T,aspect=3)
print(rat.plot)

##### Example 3 cont.
#Fir the linear response for each rat to see random effects
library(nlme)
data(BodyWeight)
Model <- lmList(weight ~ Time, data=BodyWeight)
CI <- intervals(Model)
print(Model)
plot(CI)


