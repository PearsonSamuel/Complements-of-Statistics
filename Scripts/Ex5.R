set.seed(1)
library(ggplot2)
#parte1
amostra1=sapply(c(1:200),function(n) list(replicate(10000,mean(sapply(runif(n,0,1),function(u) -log(1-u))))))
minimos1=sapply(amostra1,min)
maximos1=sapply(amostra1,max)
media1=sapply(amostra1,mean)
#parte 2
amostra2=sapply(c(1:200),function(n) list(replicate(10000,mean(sapply(runif(n,0,1),function(u) (-log(1-u)-log(u))/2)))))
minimos2=sapply(amostra2,min)
maximos2=sapply(amostra2,max)
media2=sapply(amostra2,mean)
#gráfico
cA <- rgb(255,0,0,max=255,alpha=100)
cB <- rgb(0,0,100,max=255,alpha=100)
df=data.frame(X=c(1:200),minimos1,maximos1,minimos2,maximos2)
ggplot(df,aes(x=X)) +
  geom_ribbon(aes(ymin=minimos1, ymax=maximos1, fill="Sem redução de Variância")) +
  geom_ribbon(aes(ymin=minimos2, ymax=maximos2, fill="Com redução de Variância")) +
  geom_line(aes(y=media1))+
  geom_line(aes(y=media2))+
  scale_fill_manual("", breaks=c("Sem redução de Variância","Com redução de Variância"), 
                     values=c("Sem redução de Variância"=cA,"Com redução de Variância"=cB)) +
  theme_bw() +
  scale_y_continuous(breaks= append(c(qexp(0.025),qexp(0.975)),seq(0.5,3.5,0.5)))+
  scale_x_continuous(breaks = seq(0,200,20) ,limits=c(0,200))+
  coord_cartesian(ylim=c(qexp(0.025),qexp(0.975)))+
  labs(title="Simulação do valor esperado de uma Exponencial(1)", subtitle = "Com e sem redução de variância", x="Dimensão n", y="Ê(X)")



