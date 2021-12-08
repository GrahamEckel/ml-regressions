library(car)
library(MASS)

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

a)
vif(mlrFlux)

b)
influence.measures(mlrFlux)
max(hatvalues(mlrFlux))
mean(hatvalues(mlrFlux))

c) max(abs(rstudent(mlrFlux)))
max(rstudent(mlrFlux)) = 2.568805
min(rstudent(mlrFlux)) = -1.661255

d)
max(cooks.distance(mlrFlux)) = 0.4791133

e)
avPlots(mlrFlux)

f)
test = boxcox(mlrFlux, lambda = seq(0,6,0.05))
test$x[which.max(test$y)] #3.3


