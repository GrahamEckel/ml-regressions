
set.seed(2019-09-26)
dir = "E:\\Google Drive\\1 Math Undergrad\\1 UoGuelph\\2_Fall_19\\Applied Regression Analysis\\"
file1 = "3240_F19_A1_DDT.txt"
dfDDT = read.table(file=paste(dir,file1, sep=""), header=TRUE, sep=' ')

DDT = dfDDT$DDT

lmThick = lm(dfDDT$thickness~DDT)
summary(lmThick)

#confidence interval
predict.lm(lmThick, interval="confidence")

dfDDT1 = data.frame(DDT=2000)

predict.lm(lmThick, newdata = dfDDT1, interval = "confidence")
b) predict.lm(lmThick, newdata = dfDDT1, interval = "prediction")

c) 0.1838 = Roughly 18% of the variance in eggshell thickness can be explained by DDT