install.packages("car")
library(car)

set.seed(2019-11-13)
dir = "C:\\Users\\graha\\Google Drive\\1 Math Undergrad\\1 UoGuelph\\2_Fall_19\\Applied Regression Analysis\\Assignment 4\\"
file1 = "3240_F19_solar.csv"
dfSolar = read.table(file=paste(dir,file1, sep=""), header=TRUE, sep=',')

Flux = dfSolar$flux
Insolation = dfSolar$insolation
East = dfSolar$east
South = dfSolar$south
North = dfSolar$north

mlrFlux = lm(Flux~Insolation+East+South+North)
summary(mlrFlux) # 0.1911

#a)
y = b0 + cInsolation + c2East + c3South + c4North
ypred  = 268.19249 + 0.01911(900) + 3.874(35) + 
         6.84325(35) + (-24.73712)(16)
       = 264.69232

#b) Prediction interval - single value
dfSolarPred = data.frame(Insolation=900, East=35, South=35, North=16)
predict(mlrFlux, newdata = dfSolarPred, interval = "prediction")

#c) Confidence interval - true mean
dfSolarConf = data.frame(Insolation=900, East=35, South=35, North=16)
predict(mlrFlux, newdata = dfSolarConf, interval = "confidence")

#d) standardized regression coefficients
lm.beta(mlrFlux)

predict.lm(mlrFlux, interval = "confidence")
