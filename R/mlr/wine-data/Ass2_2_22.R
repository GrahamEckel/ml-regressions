
set.seed(2019-09-27)
dir = "C:\\Users\\graha\\Google Drive\\1 Math Undergrad\\1 UoGuelph\\2_Fall_19\\Applied Regression Analysis\\"
file1 = "3240_F19_wine.csv"
dfWine = read.table(file=paste(dir,file1, sep=""), header=TRUE, sep=',')

Clarity = dfWine$Clarity 
Aroma = dfWine$Aroma
Body = dfWine$Body
Flavour = dfWine$Flavor
Oakiness = dfWine$Oakiness
Quality = dfWine$Quality

mlrWine = lm(Quality~Clarity+Aroma+Body+Flavour+Oakiness) 
summary(mlrWine)

a) interpret Aroma - 

resWine = resid(mlrWine)
resWineClarity = plot(Clarity, resWine, ylab = "Residuals", main = "Plot of Residuals vs Clarity")
abline(0,0)
resWineAroma = plot(Aroma, resWine, ylab = "Residuals", main = "Plot of Residuals vs Aroma")
abline(0,0)
resWineBody = plot(Body, resWine, ylab = "Residuals", main = "Plot of Residuals vs Body")
abline(0,0)
resWineFlavour = plot(Flavour, resWine, ylab = "Residuals", main = "Plot of Residuals vs Flavour")
abline(0,0)
resWineOakiness = plot(Oakiness, resWine, ylab = "Residuals", main = "Plot of Residuals vs Oakiness")
abline(0,0)

b) clarity issues? what does 1.0 mean?

c) ANOVA 

summary(mlrWine)
anova(mlrWine)

qf(0.975, df1 = 5, df2 = 26) 
qf(0.95, df1 = 5, df2 = 26) 

d)
how is this different then c?


  
      
qf(0.95, df1 = 5, df2 = 20) = 2.71089

sqrt(5*2.71089) = 3.681637

3.681637*1.27815 = 4.70568
