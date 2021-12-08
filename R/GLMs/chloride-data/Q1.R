library(gmodels)
dir = "E:\\Google Drive\\...\\Assignment 3\\"
file1 = "Elephant_Data.csv"
dfElephants = read.table(file=paste(dir,file1, sep=""), header=TRUE, sep=',')

#Q1
#a) poisson regression
p.model=glm(Matings~Age,family=poisson, data = dfElephants)
summary(p.model)
#comment on significance, magnitude and difrection of the effect of age on the mean number of successful matings
#We have strong evidence against the null hypothesis that expected number of successful matings and age are independent. 


#b) 95% confidence interval for B1
ci(p.model)
#interpet this CI for B1

#c)
#Ho: model is adequate
#Ha: model is not adequate

pchisq(51.012, 39, lower.tail = F)
p = 0.094256 
#not enough evidence to reject the null at significance = 0.05 so our model is an adequate fit

#d)
l.model=lm(Matings~Age, data = dfElephants)
summary(l.model)
#interpret B1 for the linear model against B1 for the poisson model

#e)
par(mfrow=c(1,2))
qqnorm(l.model$residuals, pch = 20, main = " ")
qqline(l.model$residuals)
plot(l.model$fitted.values, l.model$residuals, pch = 20,
     xlab = "Fitted Values", ylab = "Residuals")
abline(h = 0, col = "red")

par(mfrow=c(1,1))
#comment about the model assumptions

#f)
plot(dfElephants$Age, dfElephants$Matings, xlab = "Age", ylab = "Matings", 
     main = "Plot of Matings vs Age with fitted lines", sub = "Red = Linear model, Blue = Poisson",
     xlim = c(0,60),
     ylim = c(-10,10))
abline(p.model, col = "blue")
abline(l.model, col = "red")
#comment about which model fits the data better

#g)predict the mean number of mating pairs for elephants that are 37 years old using the linear model


?plot


