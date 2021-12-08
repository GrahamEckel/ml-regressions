Assignment 4
Part 3

dir = "C:\\Users\\graha\\Google Drive\\1. Studies\\1. Math Undergrad\\Winter_19\\Stat2050 - Stat 2\\Assignment 4\\"
file1 = "ex1124.csv"
dfEx1124 = read.table(file=paste(dir,file1, sep=""), header=TRUE, sep=',')

type = dfEx1124[,"type"]
bodymass = dfEx1124[,"bodymass"]
maxdist = dfEx1124[,"maxdist"]

# Create indicator variables for diet type.
omni = herbi = carni = rep(0,64) # make 3 variables of length 64

# all values equal to 0
omni[type=="O"] = 1 # if type ==O, then omni <- 1; else omni <- 0;
herbi[type=="H"] = 1
carni[type=="C"] = 1
lbodymass = log(bodymass)
lmaxdist = log(maxdist)

# Attach new variables to data frame
dfEx1124 = data.frame(dfEx1124,omni,herbi,carni,lbodymass,lmaxdist)
#attach(dfEx1124,pos=1)

a)
par(mfrow=c(2,2))
plot(maxdist,bodymass)
plot(maxdist,lbodymass)
plot(lmaxdist,bodymass)
plot(lmaxdist,lbodymass)

b)
M1 = lm(lmaxdist~lbodymass*type, data=dfEx1124)
summary(M1)

c)
M2 = lm(lmaxdist~lbodymass+type, data=dfEx1124)
summary(M2)
anova(M2)

d)
anova(M2,M1)
qf(0.95, df1=2, df2=58)

e)
M3=lm(lmaxdist~lbodymass, data=dfEx1124) 
summary(M3)
anova(M3)

f)
anova(M3,M2)
qf(0.95, df1=2, df2=60) = 3.1504
