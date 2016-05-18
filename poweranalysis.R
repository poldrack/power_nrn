# code to create figure 1 from Poldrack et al., Nature Reviews Neuroscience

library(dplyr)

tal_data=read.table('tal_data.txt',col.names=c('year','n'))
david_data=read.table('david_data.txt',col.names=c('year','n'))
# drop years with small samples
david_data=david_data[david_data$year<2015,]
tal_data=tal_data[tal_data$year>1997,]
library(pwr)


# compute effect size at 80% power for each sample size, using 
# the uncorrected p thresh specified below

pthresh=0.005

tal_power=data.frame(year=NA,n=NA)
for (i in 1:nrow(tal_data)) {
  if (tal_data[i,2]>2) {
    p=pwr.t.test(n=tal_data[i,2],sig.level=pthresh,power=0.8)
    tal_power[i,]=c(tal_data[i,1],p$d)
  }
}


david_power=data.frame(year=NA,n=NA)
for (i in 1:nrow(david_data)) {
  if (david_data[i,2]>2) {
    p=pwr.t.test(n=david_data[i,2],sig.level=pthresh,power=0.8)
    david_power[i,]=c(david_data[i,1],p$d)
  }
}


tal_data_by_year=group_by(tal_data,year)
tal_data_medians=summarize(tal_data_by_year,medians=median(n))
david_data_by_year=group_by(david_data,year)
david_data_medians=summarize(david_data_by_year,medians=median(n))
tal_power_by_year=group_by(tal_power,year)
tal_power_medians=summarize(tal_power_by_year,medians=median(n))
david_power_by_year=group_by(david_power,year)
david_power_medians=summarize(david_power_by_year,medians=median(n))

# David only has one data point beyond 2011; drop 2012 and beyond
david_data_medians=david_data_medians[david_data_medians$year<=2012,]
david_power_medians=david_power_medians[david_power_medians$year<=2012,]

years=c(1995,2015)+c(-1,1)/2
cex_points=0.2
rand_jitter=0.2
offset=0.1
symbol_alpha=0.4
par(mfrow=c(1,2))
plot(tal_data_medians,type='l',col='red',xlim=years,ylim=c(0,200),xlab='Years',
     ylab='Median sample size',lwd=4)
points(tal_data$year-offset-rand_jitter*runif(dim(tal_data)[1]),tal_data$n,pch=20,cex=cex_points,col=rgb(red=1., green=0, blue=0, alpha=symbol_alpha))
lines(david_data_medians,type='l',col='blue',xlim=years,ylim=c(0,200),xlab='Years',
     ylab='Median sample size',lwd=4)
points(david_data$year+offset+rand_jitter*runif(dim(david_data)[1]),david_data$n,pch=20,col=rgb(red=0., green=0, blue=1., alpha=symbol_alpha),cex=cex_points)
legend('topleft',legend=c('David et al.','Neurosynth'),col=c('blue','red'),lwd=c(4,4))

plot(tal_power_medians,type='l',xlim=years,ylim=c(0,3.2),xlab='Years',
     ylab='Effect size with 80% power',col='red',lwd=4)
lines(david_power_medians,col='blue',lwd=4)
legend('topright',legend=c('David et al.','Neurosynth'),col=c('blue','red'),lwd=c(4,4))
dev.copy2pdf(file='power_and_effectsize.pdf',width=16,height=6)

