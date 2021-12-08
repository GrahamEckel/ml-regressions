
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
resWineClarity = plot(Clarity, resWine)
resWineAroma = plot(Aroma, resWine)
resWineBody = plot(Body, resWine)
resWineFlavour = plot(Flavour, resWine)
resWineOakiness = plot(Oakiness, resWine)

b) clarity issues? what does 1.0 mean?

c) 
H1: B1 = 0
H1: B1 = 0
H1: B1 = 0
H1: B1 = 0
H1: B1 = 0
H1: B1 = 0

d)
how is this different then c?
  

