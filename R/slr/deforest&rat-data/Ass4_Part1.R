Assignment 4
Part 1

set.seed(2019-04-01)
dir = "C:\\Users\\graha\\Google Drive\\1. Studies\\1. Math Undergrad\\Winter_19\\Stat2050 - Stat 2\\Assignment 4\\"
file1 = "rat.csv"
dfRat = read.table(file=paste(dir,file1, sep=""), header=TRUE, sep=',')

#a)
PROTEIN = dfRat[,1]
TrtGroup = dfRat[,2]
TREATN = dfRat[,3]
plot(TREATN, PROTEIN, main="Scatterplot of Protein against Treatment")

#b) Write down the linear regression model and assumptions

#Simple Linear Regression Model => Yhat = B0 + B1X + E

#SLR Assumptions:
# 1. Normality: For each value of the explanatory variable (X) there is a subpopulation of
# response variables (Y) that is normally distributed
# 2. Linearity: The means of the subpopulation for each value of X fall on the straight line B0 + B1X
# 3. Constant Variance: All subpopulations have the same standard deviation
# 4. Independence: All observations are independent 

#c)
M1 = lm(PROTEIN~TREATN) #SLR
plot(TREATN, PROTEIN, main="Plot of Protein against Treatment with SLR")
abline(M1)
summary(M1)

#d)
M2 = lm(PROTEIN~TrtGroup) #ANOVA
AnovaM2 = anova(M2)

#Since we now fit an ANOVA model, we use the ANOVA assumptions
#1. Observations are independent
#  - within groups
#  - between groups
#  - determined by the nature of the data
#2. Observations are normally distributed
#3. There is equal variance among the groups

#e)
anova(M1, M2)
Fobs = 0.297
qf(0.95, df1=4, df2=24) = 2.77


