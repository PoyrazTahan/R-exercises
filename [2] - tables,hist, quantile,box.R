#Q1

#Q2
summary(daily)
#Q3
summary(daily$hum)
#Q4
summary(daily$hum,digits=2)
#Q5
table(daily$season)
#Q6
options(digits=2)
prop.table(table(daily$season))
#Q7
table(daily$season,daily$yr)
#Q8
mean(daily$cnt)
median(daily$cnt)
sd(daily$cnt)
#Q9
quantile(daily$cnt, probs = c(0.05,0.95))
#Q10
hist(daily$cnt)
#Q11
hist(daily$cnt,breaks=30)
#Q12
hist(daily$cnt,breaks=30,freq = FALSE)
#Q13
hist(daily$cnt,breaks=30,freq = FALSE) 
lines(density(daily$cnt,bw= 500, type="1", col="darkblue",lwd=2))
#Q14
boxplot(daily$hum)
#Q15
boxplot(daily$hum~daily$season)
#Q16
qplot(x=hum, data=daily, bins = 30) + facet_wrap(~season)
#Q17
options(digits=4)
by(daily$hum,daily$season, mean)
#Q18
aggregate(daily$hum,
          by=list(daily=daily$season), mean)
