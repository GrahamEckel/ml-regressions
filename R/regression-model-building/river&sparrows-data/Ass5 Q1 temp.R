library(leaps)
library(MASS)
set.seed(2019-09-26)
dir = "C:\\Users\\graha\\Google Drive\\1 Math Undergrad\\1 UoGuelph\\2_Fall_19\\Applied Regression Analysis\\Assignment 5\\"
file1 = "3240_F19_RiverData.csv"
dfRiver = read.table(file=paste(dir,file1, sep=""), header=TRUE, sep=',')

#Rename the vars
Nitrate = dfRiver$NO3
Density = dfRiver$DENSITY
NPrecip = dfRiver$NPREC
Precip = dfRiver$PREC
Deposit = dfRiver$DEP
Area = dfRiver$AREA
Runoff = dfRiver$RUNOFF
Discharge = dfRiver$DISCHARG

predictors = cbind(Density, NPrecip, Precip, Deposit, Area, Runoff, Discharge)

#Full linear model, no transformations
lmNitrateFull = lm(Nitrate~Density+NPrecip+Precip+Deposit+Area+Runoff+Discharge)
summary(lmNitrateFull)

#boxcox: finding our lambda
BoxcoxFull = boxcox(lmNitrateFull2, lambda = seq(-2,2,1/100))
BoxcoxFull$x[which.max(BoxcoxFull$y)] #lambda = 0.33

#Full linear model, with conveient lambda = 0.5 transformation on y predicted
NitrateTrans = Nitrate^0.5
lmNitrateFullTrans = lm(NitrateTrans~Density+NPrecip+Precip+Deposit+Area+Runoff+Discharge)
summary(lmNitrateFullTrans)

#Reduced linear model with transformation
lmNitrateReducedTrans = lm(NitrateTrans~Density+NPrecip)
summary(lmNitrateReducedTrans)

#STEPWISE MODEL BUILDING
#Density and Nitrate Precipitation must be included in the model so the transformed reduced model
#is our starting point

lmStep1Precip = lm(NitrateTrans~Density+NPrecip+Precip)
lmStep1Deposit = lm(NitrateTrans~Density+NPrecip+Deposit)
lmStep1Area = lm(NitrateTrans~Density+NPrecip+Area)
lmStep1Runoff = lm(NitrateTrans~Density+NPrecip+Runoff)
lmStep1Discharge = lm(NitrateTrans~Density+NPrecip+Discharge)

dfRiverTrans = dfRiver$NO3=dfRiver$NO3^(1/2)

lm(N03~DENSITY+NPREC, data=dfRiverTrans)

lm(dfRiverTrans$N)

leaps = regsubsets(NO3~DENSITY+NPREC+DEP+PREC+AREA+RUNOFF+DISCHARG, data=dfRiver)
par(mfrow=c(2,2))
plot(leaps, scale=c('adjr2'))
plot(leaps, scale=c('bic'))
plot(leaps, scale=c('r2'))
plot(leaps, scale=c('Cp'))



summary(lmStep1Precip) # p= 0.218
summary(lmStep1Deposit) # p= 0.00000114
summary(lmStep1Area) # p= 0.0994
summary(lmStep1Runoff) #p= 0.223
summary(lmStep1Discharge) #p= 0.148




#ADDING AND REMOVING VARIABLES DOESN'T REALLY IMPROVE THE MODEL R^2 or MSRes 
#from the values in the full model or the base model.
#Perhaps we need to do predictor transformation?
#Perhaps we need to do sum or product of predictors?




#comparing residuals, qq and leverage plots
plot(lmNitrate)
plot(lmNitrateTrans)


#BACKWARDS ELIMINATION
#Full model
summary(lmNitrateTransFull)

#Precipitation is least significant, so we remove it (p<0.2)
lmNitrateTrans6 = lm(NitrateTrans~Density+NPrecip+Deposit+Area+Runoff+Discharge)
summary(lmNitrateTrans6)

#runoff is least significant, so we remove it (p<0.2)
lmNitrateTrans5 = lm(NitrateTrans~Density+NPrecip+Deposit+Area+Discharge)
summary(lmNitrateTrans5)

#Deposit is least significant, so we remove it (p<0.2)
lmNitrateTrans4 = lm(NitrateTrans~Density+NPrecip+Area+Discharge)
summary(lmNitrateTrans4) # not really better

#Area is least significant, so we remove it (p<0.2)
lmNitrateTrans3 = lm(NitrateTrans~Density+NPrecip+Discharge)
summary(lmNitrateTrans3) # not really better


#FORWARD SELECTION
#Base model
lmNitrateTransMin = lm(NitrateTrans~Density+NPrecip)
summary(lmNitrateTransMin)

#All SLRs 
lmNitrateTransPrecip = lm(NitrateTrans~Precip)
lmNitrateTransDeposit = lm(NitrateTrans~Deposit) 
lmNitrateTransArea = lm(NitrateTrans~Area) 
lmNitrateTransRunoff = lm(NitrateTrans~Runoff) 
lmNitrateTransDischarge = lm(NitrateTrans~Discharge) 

summary(lmNitrateTransPrecip) # p= 0.218
summary(lmNitrateTransDeposit) # p= 0.00000114
summary(lmNitrateTransArea) # p= 0.0994
summary(lmNitrateTransRunoff) #p= 0.223
summary(lmNitrateTransDischarge) #p= 0.148

#adding Deposit
lmNitrateTransAdd1 = lm(NitrateTrans~Density+NPrecip+Deposit)
summary(lmNitrateTransAdd1) #not really better


#STEPWISE
#Base model
lmNitrateTransMin = lm(NitrateTrans~Density+NPrecip)
summary(lmNitrateTransMin)

#Base model with each explanatory variable
lmNitrateTransMinw1 = lm(NitrateTrans~Density+NPrecip+Precip)
lmNitrateTransMinw2 = lm(NitrateTrans~Density+NPrecip+Deposit)
lmNitrateTransMinw3 = lm(NitrateTrans~Density+NPrecip+Area)
lmNitrateTransMinw4 = lm(NitrateTrans~Density+NPrecip+Runoff)
lmNitrateTransMinw5 = lm(NitrateTrans~Density+NPrecip+Discharge)

summary(lmNitrateTransMinw1) # p= 0.27340
summary(lmNitrateTransMinw2) # p= 0.28777
summary(lmNitrateTransMinw3) # p= 0.963781
summary(lmNitrateTransMinw4) # p= 0.0921
summary(lmNitrateTransMinw5) # p= 0.34521

#Adding Runoff
lmNitrateTransAddRunoff = lm(NitrateTrans~Density+NPrecip+Runoff)
summary(lmNitrateTransAddRunoff)



#other
plot(dfRiver$NO3, dfRiver$DENSITY)
plot(dfRiver$NO3, dfRiver$NPREC)

scatterplotMatrix(dfRiver)

