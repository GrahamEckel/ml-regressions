library(DataExplorer)
library(ggplot2)
library(ggcorrplot)
library(dplyr)
library(MASS)
library(gridExtra)
library(reshape2)
library(fmsb)
library(rms)

#data file from Kaggle stored in project directory
dir = "E:\\Google Drive\\1 Math Undergrad\\1 UoGuelph\\3_Winter_20\\Stat3510 - Environmental Risk Analysis\\Group Project\\"
file1 = "heart.csv"
dfHeartDiseaseOriginal = read.table(file=paste(dir,file1, sep=""), header=TRUE, sep=',')

####
####DATA CLEANING
####

#Remove rows with NaN's
which(dfHeartDiseaseOriginal$ca==4)
which(dfHeartDiseaseOriginal$thal==0)
dfHDFull = dfHeartDiseaseOriginal[-c(49,93,159,164,165,252,282), ]

#Cleaning headers
colnames(dfHDFull)[1] = "age"

#Changing categorical variables from numerics to factors
dfHDFull = mutate(dfHDFull, 
                  sex = as.factor(sex),
                  cp = as.factor(cp),
                  fbs = as.factor(fbs),
                  restecg = as.factor(restecg),
                  exang = as.factor(exang),
                  slope = as.factor(slope),
                  ca = as.factor(ca),
                  thal = as.factor(thal),
                  target = as.factor(target))
#reorder columns for easier exploratory analysis
dfHDFull = dplyr::select(dfHDFull, target, sex, fbs, exang, cp, restecg, slope, ca, thal, everything())

####
####EXPLORATORY ANALYSIS OF NUMERICAL VARIABLES
####

#check for missing values
PlotMissing(dfHDFull)

#peak
summary(dfHDFull)

#correlation matrix for independent numerical variables
dfHDFull.cor = cor(dfHDFull[10:14])
ggcorrplot(dfHDFull.cor, hc.order = TRUE, outline.color = "white", type = "lower", lab = T)

#boxplot of numerical variables to explore outliers
ggplot(melt(dfHDFull[,10:14]), aes(variable, value)) +
  geom_boxplot() +
  facet_wrap(~variable, scales = "free") + 
  theme(axis.text.x= element_blank()) +
  #ggtitle("Numerical Variables from Heart Disease Data") +
  xlab("") + ylab("")

#Checking correlation of numerical covariates on categorical dependent
heart.model1 <- glm(target~age, family = binomial, data = dfHDFull)
summary(heart.model1)
heart.model2 <- glm(target~chol, family = binomial, data = dfHDFull)
summary(heart.model2)
heart.model3 <- glm(target~trestbps, family = binomial, data = dfHDFull)
summary(heart.model3)
heart.model4 <- glm(target~thalach, family = binomial, data = dfHDFull)
summary(heart.model4)
heart.model5 <- glm(target~oldpeak, family = binomial, data = dfHDFull)
summary(heart.model5)

####
####EXPLORATORY ANALYSIS OF CATEGORICAL VARIABLES
####

#checking bias of categorical dependent
ggplot(dfHDFull, aes(x=dfHDFull$target, fill=dfHDFull$target)) + 
  geom_bar() +
  xlab("Heart Disease") +
  ylab("Count") +
  ggtitle("Heart Disease Occurances") +
  scale_fill_discrete(name = "Heart Disease", labels = c("No","Yes")) +
  theme(axis.text.x= element_blank())

#Checking correlation of categorical covariates on categorical dependent
chisq.test(dfHDFull$target, dfHDFull$sex)
chisq.test(dfHDFull$target, dfHDFull$cp)
chisq.test(dfHDFull$target, dfHDFull$fbs)
chisq.test(dfHDFull$target, dfHDFull$restecg)
chisq.test(dfHDFull$target, dfHDFull$exang)
chisq.test(dfHDFull$target, dfHDFull$slope)
chisq.test(dfHDFull$target, dfHDFull$ca)
chisq.test(dfHDFull$target, dfHDFull$thal)

#fisher's test for small values
table(dfHDFull$target, dfHDFull$restecg)
fisher.test(dfHDFull$target, dfHDFull$restecg)

#exploring risk difference, risk ratio and odds ratio for binary covarites
table(dfHDFull$target, dfHDFull$sex)
table(dfHDFull$target, dfHDFull$fbs)
table(dfHDFull$target, dfHDFull$exang)

#sex
riskdifference(112, 24, 201, 95, CRC = FALSE, conf.level = 0.95)
riskratio(112, 24, 201, 95, conf.level = 0.95)
oddsratio(112, 24, 89, 71, conf.level = 0.95)

#fbs
riskdifference(20, 116, 43, 253, CRC = FALSE, conf.level = 0.95)
riskratio(20, 116, 43, 253, conf.level = 0.95)
oddsratio(20, 116, 23, 137, conf.level = 0.95)

#exang
riskdifference(74, 62, 97, 199, CRC = FALSE, conf.level = 0.95)
riskratio(74, 62, 97, 199, conf.level = 0.95)
oddsratio(74, 62, 23, 137, conf.level = 0.95)

####
#### MODEL SELECTION & COMPARISON
####

#Full model
dfHDFull.modelFull = glm(target~., family=binomial, data=dfHDFull)

#Through exploratory analysis, we determined that fbs and chol were not significant
#They are dropped from the dataset in use
drops = c("fbs", "chol")
dfHDRed = dfHDFull[ , !(names(dfHDFull) %in% drops)]

#The full model of the reduced data 
dfHDRed.modelFull = glm(target~., family=binomial, data=dfHDRed)

#########STEPWISE SELECTION############

#using min AIC and going forwards and backwards
dfHDFull.modelStep = dfHDFull.modelFull %>% stepAIC(direction = "both", trace = FALSE)
dfHDRed.modelStep = dfHDRed.modelFull %>% stepAIC(direction = "both", trace = FALSE)

#the min AIC stepwise model selection produces the same results 
#for the full dataset as the reduced dataset

#########BACKWARD SELECTION############

#starting with the full model on reduced data, then dropping in order: restecg, slop, age, exang, thal
heartred <- glm(target~ sex+exang+cp+restecg+slope+ca+thal+age+trestbps+thalach+oldpeak, family = binomial, data = dfHDRed)
heart1 <- glm(target~ sex+exang+cp+slope+ca+thal+age+trestbps+thalach+oldpeak, family = binomial, data = dfHDRed)
heart2 <- glm(target~ sex+exang+cp+ca+thal+age+trestbps+thalach+oldpeak, family = binomial, data = dfHDRed)
heart3 <- glm(target~ sex+exang+cp+ca+thal+trestbps+thalach+oldpeak, family = binomial, data = dfHDRed)
heart4 <- glm(target~ sex+cp+ca+thal+trestbps+thalach+oldpeak, family = binomial, data = dfHDRed)
heart5 <- glm(target~ sex+cp+ca+trestbps+thalach+oldpeak, family = binomial, data = dfHDRed)

####
#### MODEL COMPARISON
####

#testing each model in backwards selection using drop in deviance
anova(heart3, heart2, test = "Chisq")
anova(heart4, heart3, test = "Chisq")
anova(heart5, heart4, test = "Chisq")

#testing which is better: backwards or stepwise using drop in deviance
anova(heart4, dfHDRed.modelStep, test = "Chisq")

#model summaries, initial full model, backwards model, stepwise (final) model
summary(dfHDRed.modelFull)
summary(heart4)
summary(dfHDRed.modelStep)

#model fit, R^2
lrm(dfHDRed.modelFull)
lrm(heart4)
lrm(dfHDRed.modelStep)

#Goodness of Fit
pchisq(182.16, 280, lower.tail = F)
pchisq(182.16, 280, lower.tail = F)
pchisq(182.16, 280, lower.tail = F)

