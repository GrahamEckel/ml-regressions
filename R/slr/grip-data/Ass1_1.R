
set.seed(2019-09-12)
dir = "C:\\Users\\graha\\Google Drive\\1 Math Undergrad\\1 UoGuelph\\2_Fall_2019\\Applied Regression Analysis\\"
file1 = "3240_F19_A1_DDT.txt"
dfDDT = read.table(file=paste(dir,file1, sep=""), header=TRUE, sep=' ')

head(dfDDT)

plot(dfDDT$DDT, dfDDT$thickness, ylab = "Thickness", xlab = "DDT", main = "Scatterplot of Thickness vs DDT")

lmThick = lm(dfDDT$thickness~dfDDT$DDT)
summary(lmThick)

resThick = resid(lmThick)
resThickPlot = plot(dfDDT$DDT, resThick, ylab = "Residuals", xlab = "DDT", main = "Plot of Residuals vs DDT ")
abline(0,0)

plot(dfDDT$DDT, dfDDT$thickness, ylab = "Thickness", xlab = "DDT", main = "SLR of Thickness vs DDT")
abline(lmThick)

dfDDT

dt(x = 97.5, df = 14)

qt(0.975, df = 33)

confint(lmThick, "dfDDT$DDT", level=0.95)

