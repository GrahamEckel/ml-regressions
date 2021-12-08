library(fitdistrplus)
library(ggplot2)
library(reshape2)

dir = "E:\\Google Drive\\1 Math Undergrad\\1 UoGuelph\\3_Winter_20\\Stat3510 - Environmental Risk Analysis\\IC Assignment 3\\"
file1 = "chlorpyrifos.csv"
file2 = "df.csv"
chlorpyrifos = read.table(file=paste(dir,file1, sep=""), header=TRUE, sep=',')
df = read.table(file=paste(dir,file2, sep=""), header=TRUE, sep=',')

df = df[order(df$val), ]
df$frac = ppoints(df$val, 0.5)

ggplot(data = df) +
  geom_point(aes(x = val, y = frac), size = 3) +
  geom_text(aes(x = val, y = frac, label = species), hjust = 1.1, size = 4) +
  theme_bw()     + 
  scale_x_log10(limits = c(0.0075, max(df$val))) +
  labs(x = expression(paste('Concentration of Chlorpyrifos [ ', mu, 'g ', L^-1, ' ]')), y = 'Fraction of species affected')

#lognormal distribution
fit2 = fitdist(df$val, 'lnorm')
fit2

#HC VALUES AND BOOTSTRAPPING
hc5_2 = quantile(fit2, probs = 0.05)   
hc5_2 

#parametric distribution (resample from the distribution)
fit2_boot = bootdist(fit2, bootmethod = 'param', niter = 1000)
fit2_boot
HC5_boot = quantile(fit2_boot, probs = 0.05)
HC5_boot

#parametric distribution (resample from the dataset)
fit2_npboot = bootdist(fit2, bootmethod = 'nonparam', niter = 1000)
fit2_npboot
HC5_npboot = quantile(fit2_boot, probs = 0.05)
HC5_npboot


#CREATE A PLOT OF THE SSD

#predict new distributions on a grid of 1000
newxs = 10^(seq(log10(0.01), log10(max(df$val)), length.out = 1000))
pp = apply(fit2_boot$estim, 1, function(x) plnorm(newxs, x[1], x[2]))
bootdat = data.frame(pp)

#add x-values
bootdat$newxs = newxs
bootdat = melt(bootdat, id.vars = 'newxs')
cis = apply(pp, 1, quantile, c(0.025, 0.975))
rownames(cis) = c('lwr' ,'upr')

#get confidence interval from bootstraps
pdat = data.frame(newxs, py = plnorm(newxs, meanlog = fit2$estimate[1], sdlog = fit2$estimate[2]))

#add confidence intervals
pdat = cbind(pdat, t(cis))

#add x coordinates for species names from fitted values
df$fit = 10^(log10(qlnorm(df$frac, meanlog = fit2$estimate[1], sdlog = fit2$estimate[2])) - 0.4)

#plot
ggplot()+
  geom_line(data = bootdat, aes(x = newxs, y = value, group = variable), col = 'steelblue', alpha = 0.05) + 
  geom_point(data = df, aes(x = val, y = frac)) +
  geom_line(data = pdat, aes(x = newxs, y = py), col = 'red') +   
  geom_line(data = pdat, aes(x = newxs, y = lwr), linetype = 'dashed') + 
  geom_line(data = pdat, aes(x = newxs, y = upr), linetype = 'dashed') + 
  geom_text(data = df, aes(x = fit, y = frac, label = species), hjust = 1, size = 4) +
  theme_bw() +
  scale_x_log10(breaks = c(0.1, 1, 10, 100, 1000), limits = c(0.003, max(df$val))) +
  labs(x = expression(paste('Concentration of Chlorpyrifos [ ', mu, 'g ', L^-1, ' ]')), y = 'Fraction of species affected')

#CREATE A PLOT OF THE SSD

chlorpyrifos$Conc<-chlorpyrifos[order(chlorpyrifos$Conc),]

chlorpyrifos$frac<-ppoints(chlorpyrifos$Conc,0.5)

ggplot(data = chlorpyrifos) +
  geom_point(aes(x = Conc, y = frac), size = 2) +
  theme_bw() +
  scale_x_log10(limits = c(0.001, max(chlorpyrifos$Conc))) +
  labs(x = expression(paste('Concentration of Chlorpyrifos [ ', n, 'g ', L^-1, ' ]')), y = 'Percent Rank')

fit2<-fitdist(chlorpyrifos$Conc,'lnorm')
fit2

hc95_2<-quantile(fit2,probs=0.95)
hc95_2

fit2_boot <- bootdist(fit2, bootmethod = 'param', niter = 1000)
quantile(fit2_boot, probs = 0.95)

fit2_boot <- bootdist(fit2, bootmethod = 'nonparam', niter = 1000)
quantile(fit2_boot, probs = 0.95)

newxs <- 10^(seq(log10(0.001), log10(max(chlorpyrifos$Conc)), length.out = 1000))
pp <- apply(fit2_boot$estim, 1, function(x) plnorm(newxs, x[1], x[2]))
bootdat <- data.frame(pp)
bootdat$newxs <- newxs
bootdat <- melt(bootdat, id.vars = 'newxs')
cis <- apply(pp, 1, quantile, c(0.025, 0.975))
rownames(cis) <- c('lwr' ,'upr')
pdat <- data.frame(newxs, py = plnorm(newxs, meanlog = fit2$estimate[1], sdlog = fit2$estimate[2]))
pdat <- cbind(pdat, t(cis))
chlorpyrifos$fit <- 10^(log10(qlnorm(chlorpyrifos$frac, meanlog = fit2$estimate[1], sdlog = fit2$estimate[2])) - 0.4)

#plot
ggplot()+
  geom_line(data = bootdat, aes(x = newxs, y = value, group = variable), col = 'steelblue', alpha = 0.05) + 
  geom_point(data = chlorpyrifos, aes(x = Conc, y = frac)) +
  geom_line(data = pdat, aes(x = newxs, y = py), col = 'red') +   
  geom_line(data = pdat, aes(x = newxs, y = lwr), linetype = 'dashed') + 
  geom_line(data = pdat, aes(x = newxs, y = upr), linetype = 'dashed') + 
  geom_text(data = df, aes(x = fit, y = frac, label = species), hjust = 1, size = 4) +
  theme_bw() +
  scale_x_log10(breaks = c(0.1, 1, 10, 100, 1000), limits = c(0.003, max(df$val))) +
  labs(x = expression(paste('Concentration of Chlorpyrifos [ ', mu, 'g ', L^-1, ' ]')), y = 'Fraction of species affected')

ggplot()+
  geom_line(data = bootdat, aes(x = newxs, y = value, group = variable), col = 'steelblue', alpha = 0.05) + 
  geom_point(data = chlorpyrifos, aes(x = Conc, y = frac)) +
  geom_line(data = pdat, aes(x = newxs, y = py), col = 'red') +   
  geom_line(data = pdat, aes(x = newxs, y = lwr), linetype = 'dashed') + 
  geom_line(data = pdat, aes(x = newxs, y = upr), linetype = 'dashed') + 
  theme_bw() +
  scale_x_log10(breaks = c(0.001,0.01,0.1, 1, 10, 100), limits = c(0.001, max(chlorpyrifos$Conc)))+ 
  labs(x = expression(paste('Concentration of Chlorpyrifos [ ', n, 'g ', L^-1, ' ]')), y = 'Percent Rank')

