set.seed(1)
library(ggplot2)
amostra<-list()
sinal=1
contador1=0
contador2=0
df<-data.frame(
  x=c(),
  y=c(),
  cor=c()
)
while (length(amostra)<10000) {
  a=runif(1,0,1)
  u=runif(1,0,1)
  b=-log(u) 
  if(a<(1/(1.5336263*sqrt(pi)) *exp(-(b^2)/4+b))){sinal=-sinal 
  amostra=append(amostra,sinal*b+3)
  contador2=contador2+1
  dfadd<-data.frame(
    x=b,
    y=1.5336263*a*exp(-b),
    cor=0
    )
  } else{  
    dfadd<-data.frame(
    x=b,
    y=1.5336263*a*exp(-b),
    cor=1
    )}

  df <-rbind(df, dfadd)
  contador1=contador1+1
}
ggplot(df,aes(color=as.factor(cor)))+geom_point(aes(x,y),size=0.8)+theme_bw()+
  labs(x="b",y="a*c*F(b)")+scale_color_manual("",values=c("slateblue4","indianred3"), labels=c("Aceites","Rejeitados"))
ggsave("fig4.png",width=10,height=5)

#Verificar média e desvio padrão:
mean(as.numeric(amostra))
sd(as.numeric(amostra))
                                              
