library(Rmisc)

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

confint(mlrFlux, "East", level=0.9)

CI(dfSolar$east, ci=0.9)

