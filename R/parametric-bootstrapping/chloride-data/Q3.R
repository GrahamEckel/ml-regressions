library(fitdistrplus)
library(ggplot2)
library(reshape2)

dir = "E:\\Google Drive\\1 Math Undergrad\\1 UoGuelph\\3_Winter_20\\Stat3510 - Environmental Risk Analysis\\Assignment 3\\"
file1 = "ONCHLOR.csv"
file2 = "Table3.csv"
dfChlor = read.table(file=paste(dir,file1, sep=""), header=TRUE, sep=',')
dfTable3 = read.table(file=paste(dir,file2, sep=""), header=TRUE, sep=',')

#Q3
######a) construct an acute SSD for chloride in R for the fish
dfTable3 = dfTable3[order(dfTable3$Concentration..mg.Cl..L..), ]
dfTable3$frac = ppoints(dfTable3$Concentration..mg.Cl..L.., 0.5)

ggplot(data = dfTable3) +
  geom_point(aes(x = Concentration..mg.Cl..L.., y = frac), size = 3) +
  geom_text(aes(x = Concentration..mg.Cl..L.., y = frac, label = Species), hjust = 1.1, size = 4) +
  theme_bw()     + 
  scale_x_log10(limits = c(0.0075, max(dfTable3$Concentration..mg.Cl..L..))) +
  labs(x = expression(paste('Concentration of dfChlor [ ', mu, 'g ', L^-1, ' ]')), y = 'Fraction of species affected')

#model
fit2 = fitdist(dfTable3$Concentration..mg.Cl..L.., 'lnorm')
summary(fit2)

#boostrapped
fit2_bootnp = bootdist(fit2, bootmethod = 'nonparam', niter = 1000)
fit2_bootp = bootdist(fit2, bootmethod = 'param', niter = 1000)
quantile(fit2_bootp, probs = 0.05)

#HC5
hc5_2 = quantile(fit2, probs = 0.05)   
hc5_2

#predict new distributions on a grid of 1000
newxs = 10^(seq(log10(0.01), log10(max(dfTable3$Concentration..mg.Cl..L..)), length.out = 1000))
pp = apply(fit2_bootp$estim, 1, function(x) plnorm(newxs, x[1], x[2]))
bootdat = data.frame(pp)
bootdat$newxs = newxs
bootdat = melt(bootdat, id.vars = 'newxs')
cis = apply(pp, 1, quantile, c(0.025, 0.975))
rownames(cis) = c('lwr' ,'upr')

#get confidence interConcentration..mg.Cl..L.. from bootstraps
pdat = data.frame(newxs, py = plnorm(newxs, meanlog = fit2$estimate[1], sdlog = fit2$estimate[2]))

#add confidence interConcentration..mg.Cl..L..s
pdat = cbind(pdat, t(cis))

#add x coordinates for species names from fitted Concentration..mg.Cl..L..ues
dfTable3$fit = 10^(log10(qlnorm(dfTable3$frac, meanlog = fit2$estimate[1], sdlog = fit2$estimate[2])) - 0.4)

#plot
ggplot()+
  geom_line(data = bootdat, aes(x = newxs, y = value, group = variable), col = 'steelblue', alpha = 0.05) + 
  geom_point(data = dfTable3, aes(x = Concentration..mg.Cl..L.., y = frac)) +
  geom_line(data = pdat, aes(x = newxs, y = py), col = 'red') +   
  geom_line(data = pdat, aes(x = newxs, y = lwr), linetype = 'dashed') + 
  geom_line(data = pdat, aes(x = newxs, y = upr), linetype = 'dashed') + 
  geom_text(data = dfTable3, aes(x = fit, y = frac, label = Species), hjust = 1, size = 4) +
  theme_bw() +
  scale_x_log10(breaks = c(0, 1, 10, 100, 1000, 10000, 100000, 1000000), limits = c(100, 100000)) +
  labs(x = expression(paste('Concentration of Chloride [ mg CI/L ]')), y = 'Fraction of vertebrates affected')


#####b)
dfChlor$frac = ppoints(dfChlor$Conc,0.5)

ggplot(data = dfChlor) +
  geom_point(aes(x = Conc, y = frac), size = 2) +
  theme_bw() +
  scale_x_log10(limits = c(0.001, max(dfChlor$Conc))) +
  labs(x = expression(paste('Concentration of Chloride [ mg CT/L ]')), y = 'Percent Rank')

fit3 = fitdist(dfChlor$Conc,'lnorm')
summary(fit3)

hc95_3 = quantile(fit3,probs=0.95)
hc95_3

fit3_bootnp = bootdist(fit3, bootmethod = 'nonparam', niter = 1000)
quantile(fit3_bootnp, probs = 0.95)

fit3_bootp = bootdist(fit3, bootmethod = 'param', niter = 1000)
quantile(fit3_bootp, probs = 0.95)

newxs2 = 10^(seq(log10(0.001), log10(max(dfChlor$Conc)), length.out = 1000))
pp2 = apply(fit3_bootp$estim, 1, function(x) plnorm(newxs2, x[1], x[2]))
bootdat2 = data.frame(pp2)
bootdat2$newxs2 = newxs2
bootdat2 = melt(bootdat2, id.vars = 'newxs2')
cis = apply(pp2, 1, quantile, c(0.025, 0.975))
rownames(cis) = c('lwr' ,'upr')
pdat2 = data.frame(newxs2, py = plnorm(newxs2, meanlog = fit3$estimate[1], sdlog = fit3$estimate[2]))
pdat2 = cbind(pdat2, t(cis))
dfChlor$fit = 10^(log10(qlnorm(dfChlor$frac, meanlog = fit3$estimate[1], sdlog = fit3$estimate[2])) - 0.4)

ggplot()+
  geom_line(data = bootdat2, aes(x = newxs2, y = value, group = variable), col = 'steelblue', alpha = 0.05) + 
  geom_point(data = dfChlor, aes(x = Conc, y = frac)) +
  geom_line(data = pdat2, aes(x = newxs2, y = py), col = 'red') +   
  geom_line(data = pdat2, aes(x = newxs2, y = lwr), linetype = 'dashed') + 
  geom_line(data = pdat2, aes(x = newxs2, y = upr), linetype = 'dashed') + 
  theme_bw() +
  scale_x_log10(breaks = c(0.01, 0, 1, 10, 100, 1000, 10000, 100000, 1000000), limits = c(0.001, 100000)) + 
  labs(x = expression(paste('Concentration of Chloride [ mg CI/L ]')), y = 'Percent Rank')

#c)

ggplot(data = dfTable3) +
  geom_point(aes(x = Concentration..mg.Cl..L.., y = frac), size = 3) +
  geom_point(data = dfChlor, aes(x = Conc, y = frac), size = 2) +
  geom_point(aes(x=hc5_2$quantiles[1], y = 0.05), size = 5, col = "red") +
  geom_point(aes(x=hc95_3$quantiles[1,1], y = 0.95), size = 5, col = "blue") +
  geom_text(aes(x = Concentration..mg.Cl..L.., y = frac, label = Species), hjust = 1.1, size = 2) +
  theme_bw() + 
  scale_x_log10(limits = c(0.0075, 100000)) +
  labs(x = expression(paste('Concentration of Chloride [ mg CI/L ]')), y = 'Fraction of vertebrates affected')

ggplot(data = dfChlor) +
  geom_point(aes(x = Conc, y = frac), size = 2) +
  theme_bw() +
  scale_x_log10(limits = c(0.001, max(dfChlor$Conc))) +
  labs(x = expression(paste('Concentration of Chloride [ mg CT/L ]')), y = 'Percent Rank')

hc95_3$quantiles[1,1]