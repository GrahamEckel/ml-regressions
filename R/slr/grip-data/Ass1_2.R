
set.seed(2019-09-12)
dir = "C:\\Users\\graha\\Google Drive\\1 Math Undergrad\\1 UoGuelph\\2_Fall_2019\\Applied Regression Analysis\\"
file1 = "3240_F19_A1_grip.csv"
dfGrip = read.table(file=paste(dir,file1, sep=""), header=TRUE, sep=',')

plot(dfGrip$grip,dfGrip$attractiveness)

lmGrip = lm(dfGrip$attractiveness~dfGrip$grip)
summary(lmGrip)

plot(dfGrip$grip,dfGrip$attractiveness, ylab = "Attractiveness", xlab = "Grip Strength", main = "SLR of Grip Strength vs Attractiveness")
abline(lmGrip)
points(mean(dfGrip$grip),mean(dfGrip$attractiveness),pch=16,cex=3)

qt(0.975, df = 12)