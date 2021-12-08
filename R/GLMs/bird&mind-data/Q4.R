dir = "C:\\Users\\graha\\Google Drive\\1 Math Undergrad\\1 UoGuelph\\3_Winter_20\\Stat3510 - Environmental Risk Analysis\\Assignment 2\\"
file1 = "Mine_Stability.csv"
dfMineStab = read.table(file=paste(dir,file1, sep=""), header=TRUE, sep=',')

#a)
MineStabFull.model = glm(Stability~Depth+Width+Height+Uniaxial.Compression.Strength, family = binomial, data = dfMineStab)
summary(MineStabFull.model)

#b)
#Only width and height are significant, the rest aren't 

#c)
MineStabRed.model = glm(Stability~Depth+Width, family = binomial, data = dfMineStab)
summary(MineStabRed.model)

#d)
#interpret on the odds scale. The variables aren't significant

#e)
anova(MineStabRed.model, MineStabFull.model, test = "Chisq")
# comment on the hypothesis tests

#use the surviving model from above and reasonable X values to predict stability 


