library(ggplot2)
set.seed(1)
gerapoisson <- function(lambda) {u=runif(1,0,1)
  p=exp(-lambda)
  f=p
  i=0
  x=i
  while (u>=f) {p=(lambda*p)/(i+1) 
    f=f+p 
    i=i+1 
    x=i}
  return(x)}
BinNeg <- function(m,r,p) {l=replicate(m,gerapoisson(sum(sapply(runif(r,0,1),function(u) -(p*log(u))/(1-p)))))
return(l)}
system.time(BinNeg(10000,6,0.7))
system.time(rnbinom(10000,6,0.3))
bn=BinNeg(10000,6,0.7)
bn_r=rnbinom(10000,6,0.3)
BN <- data.frame(count=bn)
BN_r <- data.frame(count=bn_r)
cA <- rgb(255,0,0,max=255,alpha=100)
cB <- rgb(0,0,100,max=255,alpha=100)
ggplot(data=BN,aes(x=count)) +
  geom_histogram(aes(y = stat(count) / sum(count), color="Implementado",fill="Implementado"),binwidth = 2) +
  geom_histogram(data=BN_r, aes(y = stat(count) / sum(count),color="R", fill="R"),binwidth = 2) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))+
  scale_color_manual("",  values=c("Implementado"=cA,"R"=cB)) +
  scale_fill_manual("",  values=c("Implementado"=cA,"R"=cB)) +
  labs(title="Binomial Negativa (6,0.3)", subtitle = "Comparação entre método do R e método implementado",x="Valores Gerados", y="Densidade")+
  scale_x_continuous(breaks=seq(0,56,4))+
  scale_y_continuous(breaks =seq(0,0.15,0.025))

mean(bn)
mean(bn_r)
var(bn)
var(bn_r)
median(bn)
median(bn_r)
