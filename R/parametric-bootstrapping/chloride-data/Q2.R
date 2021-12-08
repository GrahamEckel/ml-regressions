library(faraway)
dir = "E:\\Google Drive\\1 Math Undergrad\\1 UoGuelph\\3_Winter_20\\Stat3510 - Environmental Risk Analysis\\Assignment 3\\"
file1 = "Driver_data.csv"
dfDriver = read.table(file=paste(dir,file1, sep=""), header=TRUE, sep=',')


#Q2
#a)
p.model=glm(ncrash~offset(lexpos)+DE+adol, data=dfDriver, family=poisson)
summary(p.model)

#b)
#explain why it is necessary to adjust the number of crashes by miles driven 

#c)
#interpret B0 estimate on log(mean) and mean scale and something about miles driven units being adjusted

#d)
#interpret the coefficient for driver's education on both log(mean) and mean scale

#e)
par(mfrow=c(1,2))
res = residuals(p.model, type = "deviance")
pred = predict(p.model, type = "link")
plot(res, pred, main = "Residuals versus Linear Predictor", xlab = "n", ylab ="Deviance Residuals", ylim = c(-1,6))
abline(h=0, col = "red")
halfnorm(residuals(p.model, type = "deviance"), pch = 20, main = "Residuals")

#comment on any violation of model assumptions

#f)
#what is the estimated mean number of crashes/near-crashes (per 1000 miles) for an adolescent who did not take DE

#g)
#if an adolescent who did not take DE drove 7000 miles, now many crashes/near-crashes would we espect to occur?

?residuals

