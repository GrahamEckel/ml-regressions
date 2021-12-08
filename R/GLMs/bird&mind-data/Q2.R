install.packages("ggplot2")

library(ggplot2)
dir = "C:\\Users\\graha\\Google Drive\\1 Math Undergrad\\1 UoGuelph\\3_Winter_20\\Stat3510 - Environmental Risk Analysis\\Assignment 2\\"
file1 = "Bird_Extinction_Data.csv"
dfBirdExtinct = read.table(file=paste(dir,file1, sep=""), header=TRUE, sep=',')
dfBirdExtinct$logtime = log(dfBirdExtinct$time)
  
#2a)
p = ggplot(dfBirdExtinct, aes(pairs, logtime, colour=migration, shape = size,  xlab("Nesting Pairs"))) + geom_point()
p + labs(x = "Nesting Pairs", y = "LogTime", title = "Scatterplot of LogTime to Extinction and Nesting Pairs of Birds")

#b)
bird.modelAdd = lm(logtime~pairs+size+migration, data = dfBirdExtinct)
summary(bird.modelAdd)
#interpret this output

#size = small (S) = 1 
#size = large (L) = 0
#migration = resident (R) = 1
#migration = migratory (M) = 0

uhat(y|size=1, migration = 1) = 0.43087 + 0.2651(B1) - 065220 + 0.50417
uhat(y|size=1, migration = 0) = 0.43087 + 0.2651(B1) - 065220
uhat(y|size=0, migration = 1) = 0.43087 + 0.2651(B1) + 0.50417
uhat(y|size=0, migration = 0) = 0.43087 + 0.2651(B1)

#The graph would look like 4 parallel lines where the distance between a point on each line is 
#equivalent to the difference in indicator variables between each model. 

#c)
bird.modelInteract = lm(logtime~pairs+size+migration+pairs*size, data = dfBirdExtinct)
summary(bird.modelInteract)
#interpret the coefficient of the interaction term
#The interaction term, between the size of the bird and the number of nesting pairs, explores whether 
#the the size of a bird makes affects the number of nesting pairs they have.  

#sketch a graph of this (lines will cross)

#With a p-value of 0.20032, we can see that the size of the bird does not have a significant effect
#on the number of nesting pairs they have, that is, the there is not a significant difference between the
#the slope of a model with large birds and and one with small birds. 

#f test
anova(bird.modelAdd, bird.modelInteract)

#Ho: The reduced model, without adjusting for an interaction between bird size and number of nesting pairs is adequate
#Ha: The full model, adjusting for the interaction is necessary 

#In this case, our p value = 0.2003 is quite large which suggests that the reduced model is sufficient. That is,
#there is no evidence that adjusting for the interaction between bird size and number of nesting pairs is necessary
#in a model for the timee to extinction of birds. 

#d)
qqnorm(bird.modelAdd$residuals, pch = 20, main = " ")
qqline(bird.modelAdd$residuals)
plot(bird.modelAdd$fitted.values, bird.modelAdd$residuals, pch = 20, xlab = "Fitted Values", ylab = "Residuals")
abline(h = 0, col = "red")
#comment on whether any of the assumptions for linearity have been violated





