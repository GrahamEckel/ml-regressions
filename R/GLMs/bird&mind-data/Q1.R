
dir = "C:\\Users\\graha\\Google Drive\\1 Math Undergrad\\1 UoGuelph\\3_Winter_20\\Stat3510 - Environmental Risk Analysis\\Assignment 2\\"
file1 = "IQBRAIN.csv"
dfBrain = read.table(file=paste(dir,file1, sep=""), header=TRUE, sep=',')

#1a)
plot(dfBrain$AREA, dfBrain$VOL, xlab = "Area, cm^2", ylab = "Volume, cm^3", main = "Scatterplot of Area and Volume of a Brain")
#The data appears linearly related

#b)
model.brain = lm(VOL~AREA, data = dfBrain)
summary(model.brain)
#Ho: Brain Area and Volume are independent       B1 = 0
#Ha: Brain Area and Volume are not independent   B1 =/= 0

#There is a small p value of 0.00507 providing relatively strong evidence against our null hypothesis 
#that Brain Area has no effect on Brain Volume. 

#c)
#Assumptions:
#1. Linearity - Residuals
#2. Normality - Theoretical quantiles 
#3. Constant Variance - Residuals
#4. Independence - Gotta look at the study design

par(mfrow=c(1,2))
qqnorm(model.brain$residuals, pch = 20, main = " ")
qqline(model.brain$residuals)
plot(model.brain$fitted.values, model.brain$residuals, pch = 20, xlab = "Fitted Values", ylab = "Residuals")
abline(h = 0, col = "red")

#d)
#yhat = 307.2205 + 1900(0.4295) = 1123.2705

