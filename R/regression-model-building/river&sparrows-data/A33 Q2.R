library(aod)
dir = "C:\\Users\\graha\\Google Drive\\1 Math Undergrad\\1 UoGuelph\\2_Fall_19\\Applied Regression Analysis\\Assignment 5\\"
file1 = "3240_F19_sparrows.csv"
dfSparrow = read.table(file=paste(dir,file1, sep=""), header=TRUE, sep=',')

SURVIVED = ifelse(dfSparrow$STATUS=="Survived",1,0)
logitSparrow = glm(SURVIVED~TL+WT+HL+KL, family=binomial, data=dfSparrow)
summary(logitSparrow)
