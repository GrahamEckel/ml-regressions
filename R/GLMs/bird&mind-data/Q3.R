library(data.table)
dfNFL = fread("http://users.stat.ufl.edu/~winner/data/fieldgoal.dat")
colnames(dfNFL) = c("Yardage", "SuccessIndicator", "WeekNum")

#Q3)
#a)
nfl.model = glm(SuccessIndicator~Yardage, family = binomial, data = dfNFL)
summary(nfl.model)

#b)
log(odds) = 5.6788 - 0.10991(X1)
# there appears to be a relationship between yardage and successful field goals in that
# as yards decrease the odds of success increases. 

#c) interpret yards on the odds and log odds scale
# I suspect the upper limit is around 60 yards. 
#yhat = 5.69788+(-0.10991)(60)
