install.packages("ggplot2")
library(ggplot2)

set.seed(1)
xlist=c()
ylist=c()
for (i in 1:1000) {
  x=log(1/(log(1/runif(1,0,1))))
  y=log(1/log((exp(exp(-x)))/runif(1,0,1)))
  xlist=append(xlist,x)
  ylist=append(ylist,y)
}

df=data.frame(x=xlist,y=ylist)
ggplot(df,aes(x=x,y=y)) + geom_point(color='blue') + 
  geom_point(aes(x=mean(xlist),y=mean(ylist)),pch=16,col='red')+
  theme_bw()+coord_fixed(ratio=1)

#Medidas sumárias:
mean(xlist)
mean(ylist)
sd(xlist)
sd(ylist)
median(xlist)
median(ylist)
cov(xlist,ylist)
