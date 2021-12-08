Assignment 4
Part 2

set.seed(2019-04-01)
dir = "C:\\Users\\graha\\Google Drive\\1. Studies\\1. Math Undergrad\\Winter_19\\Stat2050 - Stat 2\\Assignment 4\\"
file1 = "deforest.csv"
dfDeforest = read.table(file=paste(dir,file1, sep=""), header=TRUE, sep=',')

a)
We are tasked with creating a scatterplot matrix for the deforest dataset. Then, comment on only three
of the four dimensions: LDEBT, LDEFOREST, LPOP. 
pairs(dfDeforest, pch = 19)

b)
LDEBT = dfDeforest[,2]
LDEFOREST = dfDeforest[,3] 
LPOP = dfDeforest[,4]
M1 = lm(LDEFOREST~LDEBT)
summary(M1)

c)
M2 = lm(LDEFOREST~LDEBT+LPOP)
summary(M2)
