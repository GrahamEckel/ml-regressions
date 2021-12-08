library(leaps)
library(MASS)
set.seed(2019-11-28)
dir = "C:\\Users\\graha\\Google Drive\\1 Math Undergrad\\1 UoGuelph\\2_Fall_19\\Applied Regression Analysis\\Assignment 5\\"
file1 = "3240_F19_sparrows.csv"
dfSparrow = read.table(file=paste(dir,file1, sep=""), header=TRUE, sep=',')

SURVIVED = ifelse(dfSparrow$Status=="SURVIVED"1,0)
logitSparrow = glm(SURVIVED~TL+WT+HL+KL, family=binomial, data=dfSparrow)