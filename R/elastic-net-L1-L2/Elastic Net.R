library(caret)
library(glmnet)
set.seed(2020-12-12)

#### Data from working directory
file1 = "Processed_S&P.csv"
DataSnP.raw = read.table(file=paste(file1, sep=""), header=TRUE, sep=',')

#### 
#### Cleaning, shaping, partitioning train/test split, creating feature matrix and binomial response
####
DummyCol = function(x)
{
  x[,"Direction"] = NA
  for (i in 2:nrow(x))
  {
    if ((x[i,"Close"]) - x[(i-1),"Close"] > 0)
    {
      x[i,"Direction"] = 1
    } 
    else 
    {
      x[i,"Direction"] = 0 
    }
  }
  return(x)
}

DataSnP = DummyCol(DataSnP.raw)
DataSnP = na.omit(DataSnP)

TrainIndex = createDataPartition(DataSnP$Direction, p = .8, list = FALSE, times = 1)
TrainSnP = DataSnP[ TrainIndex,]
TestSnP  = DataSnP[-TrainIndex,]

y = as.factor(TrainSnP$Direction)
TrainSnP.cleaned = TrainSnP[, !(colnames(TrainSnP) %in% c("Date", "Name","Direction"))]
TestSnP.cleaned = TestSnP[, !(colnames(TrainSnP) %in% c("Date", "Name","Direction"))]
X = as.matrix(TrainSnP.cleaned)


####
#### Tuning alpha in cv.glmnet for the binomial model and plotting the results
####
foldid = sample(1:10,size=length(y),replace=TRUE)
aSeq = seq(0,1,by=0.1)
minCVM = c(1:length(aSeq))
for (i in 1:length(aSeq))
{
  alpha = aSeq[i]
  cv = cv.glmnet(X, y, alpha = alpha, foldid = foldid, family='binomial', type.measure = 'class')
  minCVM[i] = min(cv$cvm)
}
par(mfrow=c(1,1))
plot(aSeq, minCVM, ylim = c(0,0.08), xlab = "", ylab = "", pch = 19, col = "blue", cex = 2)
title(ylab = "Minimum Cross-Validated Mean", xlab = "Alpha", font.lab = 2)


####
#### Comparing MSE vs MCE for tuned alpha (a=1), plotting, and storing value
####
cvSnPMSE = cv.glmnet(X,y, family="binomial", type.measure = "mse", foldid = foldid, alpha = 1)
cvSnPMCE = cv.glmnet(X,y, family="binomial", type.measure = "class", foldid = foldid, alpha = 1)
par(mfrow=c(2,1))
plot(cvSnPMSE)
plot(cvSnPMCE)
SnPMinLamba = cvSnPMCE$lambda.min
SnP1se = cvSnPMCE$lambda.1se

####
#### LASSO, alpha = 1    
####
lassoMC = glmnet(X,y, family="binomial")
par(mfrow=c(2,1))
plot(lassoMC,"lambda", ylim=c(-100,100))
abline(h=0)
abline(v=log(SnPMinLamba))
abline(v=log(SnP1se))

plot(lassoMC,"lambda", ylim=c(-10,10))
abline(h=0)
abline(v=log(SnPMinLamba))
abline(v=log(SnP1se))

#list of betas when lambda equals min classification error
SnPMCEbl = glmnet(X,y, family="binomial", lambda = SnPMinLamba)
SnPMCEbs = glmnet(X,y, family="binomial", lambda = (SnP1se))
#SnPMCEbl$beta


####
#### Prediction accuracy using confusion matrix
####
SnPMCE = glmnet(X, y, family = "binomial", alpha = 1, lambda = SnPMinLamba)
TestX = as.matrix(TestSnP.cleaned)
Testy = as.factor(TestSnP$Direction)
confusion.glmnet(SnPMCE, TestX, Testy, family = "binomial")
model = coef(SnPMCE)
