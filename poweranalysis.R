# code to create figure 1 from Poldrack et al., Nature Reviews Neuroscience

library(dplyr)

tal_data=read.table('tal_data.txt',col.names=c('year','n'))
david_data=read.table('david_data.txt',col.names=c('year','n'))

library(pwr)


# compute effect size at 80% power for each sample size, using 
# the uncorrected p thresh specified below

pthresh=0.005

for (i in 1:nrow(tal_data)) {
  if (tal_data[i,2]>2) {
    p=pwr.t.test(n=tal_data[i,2],sig.level=pthresh,power=0.8)
    if (i==1) {tal_power=c(tal_data[i,1],p$d)} else {tal_power=rbind(tal_power,c(tal_data[i,1],p$d))}
  }
}


for (i in 1:nrow(david_data)) {
  if (david_data[i,2]>2) {
    p=pwr.t.test(n=david_data[i,2],sig.level=pthresh,power=0.8)
    if (i==1) {david_power=c(david_data[i,1],p$d)} else {david_power=rbind(david_power,c(david_data[i,1],p$d))}
  }
}


tal_power_df=as.data.frame(tal_power,row.names=FALSE)
names(tal_power_df)=c('year','n')
david_power_df=as.data.frame(david_power,row.names=FALSE)
names(david_power_df)=c('year','n')

tal_data_by_year=group_by(tal_data,year)
tal_data_medians=summarize(tal_data_by_year,medians=median(n))
david_data_by_year=group_by(david_data,year)
david_data_medians=summarize(david_data_by_year,medians=median(n))
tal_power_by_year=group_by(tal_power_df,year)
tal_power_medians=summarize(tal_power_by_year,medians=median(n))
david_power_by_year=group_by(david_power_df,year)
david_power_medians=summarize(david_power_by_year,medians=median(n))

years=c(1995,2016)
png(file='power_and_effectsize.png',width=2400,height=1200,pointsize=32)
# note - pdf creation did not work properly in RStudio - exported manually to pdf

par(mfrow=c(1,2))
plot(tal_data_medians,type='l',col='red',xlim=years,ylim=c(0,200),xlab='Years',
     ylab='Median sample size',lwd=4)
points(tal_data$year-0.1,tal_data$n,pch=20,cex=0.2,col='red')
lines(david_data_medians,type='l',col='blue',xlim=years,ylim=c(0,200),xlab='Years',
     ylab='Median sample size',lwd=4)
points(david_data$year+0.1,david_data$n,pch=20,cex=0.2,col='blue')
legend('topleft',legend=c('David et al.','Neurosynth'),col=c('blue','red'),lwd=c(4,4))

plot(tal_power_medians,type='l',xlim=years,ylim=c(0,3.2),xlab='Years',
     ylab='Effect size with 80% power',col='red',lwd=4)
lines(david_power_medians,col='blue',lwd=4)
legend('topright',legend=c('David et al.','Neurosynth'),col=c('blue','red'),lwd=c(4,4))
dev.off()
