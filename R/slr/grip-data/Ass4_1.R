library(MASS)

set.seed(2019-11-13)
dir = "C:\\Users\\graha\\Google Drive\\1 Math Undergrad\\1 UoGuelph\\2_Fall_19\\Applied Regression Analysis\\Assignment 4\\"
file1 = "s3240_F19_viscosity.csv"
dfVisc = read.table(file=paste(dir,file1, sep=""), header=TRUE, sep=',')

Viscosity = dfVisc$visc
Temperature = dfVisc$temp

slrVisc = lm(Viscosity~Temperature)
summary(slrVisc)

a)
plot(Temperature, Viscosity, ylab = "Viscosity", xlab = "Temperature", main = "SLR of Viscosity on Temperature")
abline(slrVisc)

b)
test = boxcox(slrVisc,lambda = seq(-3,3,1/100))
test$x[which.max(test$y)] 