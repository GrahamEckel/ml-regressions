library(leaps)
library(MASS)
set.seed(2019-11-26)
dir = "C:\\Users\\graha\\Google Drive\\1 Math Undergrad\\1 UoGuelph\\2_Fall_19\\Applied Regression Analysis\\Assignment 5\\"
file1 = "3240_F19_RiverData.csv"
dfRiver = read.table(file=paste(dir,file1, sep=""), header=TRUE, sep=',')

#Full linear model, no transformations
lmNitrateFull = lm(NO3~DENSITY+NPREC+DEP+PREC+AREA+RUNOFF+DISCHARG, data=dfRiver)
summary(lmNitrateFull)
plot(resid(lmNitrateFull))
abline(0,0)

#boxcox: finding our lambda
par(mfrow=c(1,1))
BoxcoxFull = boxcox(lmNitrateFull, lambda = seq(-2,2,1/100), main="Plot of log likelihood vs Lambda for River data")
BoxcoxFull$x[which.max(BoxcoxFull$y)] #lambda = 0.33

#Full linear model, with conveient lambda = 0.5 transformation on y predicted
lmNitrateFullTrans = lm(NO3^(1/2)~DENSITY+NPREC+DEP+PREC+AREA+RUNOFF+DISCHARG, data=dfRiver)
summary(lmNitrateFullTrans)
plot(lmNitrateFullTrans)


#FORWARD SELECTION Model Building
#Reduced linear model with transformation
lmNitrateReducedTrans = lm(NO3^(1/2)~DENSITY+NPREC, data=dfRiver)
summary(lmNitrateReducedTrans)

#STEP ONE: Linear Models of each variable
lmNitrateReducedTrans1.1 = lm(NO3^(1/2)~DENSITY+NPREC+PREC, data=dfRiver)
lmNitrateReducedTrans1.2 = lm(NO3^(1/2)~DENSITY+NPREC+DEP, data=dfRiver)
lmNitrateReducedTrans1.3 = lm(NO3^(1/2)~DENSITY+NPREC+AREA, data=dfRiver)
lmNitrateReducedTrans1.4 = lm(NO3^(1/2)~DENSITY+NPREC+RUNOFF, data=dfRiver)
lmNitrateReducedTrans1.5 = lm(NO3^(1/2)~DENSITY+NPREC+DISCHARG, data=dfRiver)

summary(lmNitrateReducedTrans1.1)
summary(lmNitrateReducedTrans1.2)
summary(lmNitrateReducedTrans1.3)
summary(lmNitrateReducedTrans1.4)
summary(lmNitrateReducedTrans1.5)

#STEP TWO: Linear Model of each variable, carrying step one forward
lmNitrateReducedTrans2.1 = lm(NO3^(1/2)~DENSITY+NPREC+DEP+AREA, data=dfRiver)
lmNitrateReducedTrans2.2 = lm(NO3^(1/2)~DENSITY+NPREC+DEP+RUNOFF, data=dfRiver)
lmNitrateReducedTrans2.3 = lm(NO3^(1/2)~DENSITY+NPREC+DEP+DISCHARG, data=dfRiver)
lmNitrateReducedTrans2.4 = lm(NO3^(1/2)~DENSITY+NPREC+DEP+PREC, data=dfRiver)

summary(lmNitrateReducedTrans2.1)
summary(lmNitrateReducedTrans2.2)
summary(lmNitrateReducedTrans2.3)
summary(lmNitrateReducedTrans2.4)

#Leaps package analysis
leapspoly = regsubsets(NO3^(1/2)~DENSITY+I(DENSITY^2)+NPREC+I(NPREC^2)+DEP+I(DEP^2), data=dfRiver)
par(mfrow=c(2,2))
plot(leapspoly, scale=c('adjr2'))
plot(leapspoly, scale=c('bic'))
plot(leapspoly, scale=c('r2'))
plot(leapspoly, scale=c('Cp'))

leaps = regsubsets(NO3^(1/2)~I(DENSITY^2)+NPREC+DEP+PREC+AREA+RUNOFF+DISCHARG, data=dfRiver)
par(mfrow=c(2,2))
plot(leaps, scale=c('adjr2'))
plot(leaps, scale=c('bic'))
plot(leaps, scale=c('r2'))
plot(leaps, scale=c('Cp'))

#checking model assumptions
lmNitrateFinal = lm(NO3^(1/2)~DENSITY+NPREC+DEP, data=dfRiver)
avPlots(lmNitrateFinal)

plot(lmNitrateFinal)

# Basic Scatterplot Matrix
par(mfrow=c(2,2))
plot(dfRiver$DENSITY, dfRiver$NO3^(1/2), xlab ="Density", ylab = "NO3, Lambda=0.5", main="Scatterplot, x1")
plot(dfRiver$NPREC, dfRiver$NO3^(1/2), xlab ="NPREC", ylab = "NO3, Lambda=0.5", main="Scatterplot, x2")
plot(dfRiver$DEP, dfRiver$NO3^(1/2), xlab ="DEP", ylab = "NO3, Lambda=0.5", main="Scatterplot, x3")


pairs(~mpg+disp+drat+wt,data=mtcars,
      main="Simple Scatterplot Matrix")

par(mfrow=c(1,1))


#b)
lmNitrateFinal = lm(NO3^(1/2)~DENSITY+NPREC+DEP, data=dfRiver)
lmNitrateReduced = lm(NO3^(1/2)~NPREC+DEP, data=dfRiver)

summary(lmNitrateFinal)

anova(lmNitrateFinal)
summary(aov(lmNitrateFinal))
summary(aov(lmNitrateReduced))

anova(lmNitrateReduced,lmNitrateFinal)

#Null: coefficients on removed predictors = 0
#Alt:  coefficients on removed predictors =/= 0



