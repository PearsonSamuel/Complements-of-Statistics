library(ggplot2)
n<-10
p<-c(0.2,0.3,0.5)
m<-10000
#Algoritmo 1 - Soma de v.a's com W ~ Ber(p)

binAlg1 <- function(n,p){
  xbin<-0
    u<-runif(n,0,1)
    for (ui in u){
      if (ui<=p){
        xbin<-xbin+1
      }
    }
  return(xbin)
}


#Algoritmo 2 - Duas v.a's i.i.d. U|U<=p e U|U>p onde U~U(0,1)

binAlg2 <- function(n,p){
  u<-runif(1,0,1)
  xbin<-0
  k<-0
  while (k<n){
    k<-k+1
    if (u<=p){
      xbin<-xbin+1
      u<-u/p
      } else{
      u<-(u-p)/(1-p)
    }
  }
  return(xbin)
}


#Algoritmo 3 - Se {Yi} é uma sucessão de v.a's i.i.d.com Y~Geo(p) e X é o menor inteiro tal que sum^[X+1}_{i=1}>n, então X~Bin(n,p)
binAlg3 <- function(n,p){
  xbin<-0
  sumyi<-0
  i<-1
  u<-runif(n+2,0,1)
  while (sumyi<=n){
    xgeo<-trunc(log(1-u[i])/log(1-p))
    sumyi<-sumyi+xgeo+1
    i<-i+1
  }
  xbin<-i-2
  return(xbin)
}

#A função recebe o algoritmo de simulação de números binomiais e devolve os números com distribuição multinomial
multinomAlg <- function(nums,n,p,binfunc){
  X1<-replicate(nums,binfunc(n,p[1]))
  X2<-replicate(nums,0)
  i<-1
  while (i<=nums){
    X2[i]<-binfunc(n-X1[i],p[2]/(1-p[1]))
    i<-i+1
  }
  X3<-replicate(nums,0)
  j<-1
  while (j<=nums){
    X3[j]<-n-X1[j]-X2[j]
    j<-j+1
  }
  return(cbind(X1,X2,X3))
}

#Simulação 
set.seed(1)
start_time<-Sys.time()
xalg1<-data.frame(multinomAlg(m,n,p,binAlg1))
end_time<-Sys.time()
end_time-start_time

start_time<-Sys.time()
xalg2<-data.frame(multinomAlg(m,n,p,binAlg2))
end_time<-Sys.time()
end_time-start_time

start_time<-Sys.time()
xalg3<-data.frame(multinomAlg(m,n,p,binAlg3))
end_time<-Sys.time()
end_time-start_time

start_time<-Sys.time()
xalg4<-data.frame(t(rmultinom(m,n,p)))
end_time<-Sys.time()
end_time-start_time


#Médias
mediaalg1<-apply(xalg1,2,mean)
mediaalg2<-apply(xalg2,2,mean)
mediaalg3<-apply(xalg3,2,mean)
mediaalg4<-apply(xalg4,2,mean)
media_teor<-n*p
print(c(mediaalg1,mediaalg2,mediaalg3,mediaalg4, media_teor))

#Matrizes de Covariância
matrcovalg1<-cov(xalg1)
matrcovalg2<-cov(xalg2)
matrcovalg3<-cov(xalg3)
matrcovalg4<-cov(xalg4)
cov_teor<-matrix(0,3,3)
for (i in 1:3){
  for (j in 1:3){
    if (i==j){
      cov_teor[i,i]<- n*p[i]*(1-p[i])
    }else{
      cov_teor[i,j]<- -n*p[i]*p[j]
    }
  }
}

#Estimativa de Probabilidade
p_alg1<-sum(apply(xalg1,1, function(x) x[1]==0 && x[2]==4 && x[3]==6))/m
p_alg2<-sum(apply(xalg2,1, function(x) x[1]==0 && x[2]==4 && x[3]==6))/m
p_alg3<-sum(apply(xalg3,1, function(x) x[1]==0 && x[2]==4 && x[3]==6))/m
p_alg4<-sum(apply(xalg4,1, function(x) x[1]==0 && x[2]==4 && x[3]==6))/m
p_teor<-dmultinom(c(0,4,6),n,p)
print(c(p_alg1,p_alg2,p_alg3,p_alg4, p_teor))



dif_abs_freq <- function(xbin,p){
  freqalg<-data.frame(table(xbin))
  freqalg[,1]<-as.numeric(freqalg[,1])-1
  i<-nrow(freqalg)
  while (i<11){
    freqalg[i+1,]<-c(i,0)
    i<-i+1
  }
  for (i in seq(1,11,1)){
    freqalg[i,2]<-abs(freqalg[i,2]/m-dbinom(i-1,n,p))
  }
  return(freqalg)
}

freqalg1_1<-dif_abs_freq(xalg1$X1,p[1])
freqalg2_1<-dif_abs_freq(xalg2$X1,p[1])
freqalg3_1<-dif_abs_freq(xalg3$X1,p[1])
freqalg4_1<-dif_abs_freq(xalg4$X1,p[1])


xbin1<-rbind(cbind(freqalg1_1,alg=replicate(11,1)),cbind(freqalg2_1,alg=replicate(11,2)),cbind(freqalg3_1,alg=replicate(11,3)),cbind(freqalg4_1,alg=replicate(11,4)))
ggplot(xbin1, aes(x=xbin,y=Freq, fill=as.factor(alg)))+geom_bar(stat = "identity", width = 0.6,position=position_dodge(width = 0.65))+scale_x_continuous(breaks=seq(0,10,1))+theme_bw()+
  xlab(label="X")+ylab(label="")+labs(title = "Diferença Absoluta entre a Frequência Relativa e o valor da Função de Probabilidade de X1")+
  scale_fill_manual("Algoritmo",breaks=c("1","2","3","4"), values=c("#355070","#6D597A","#B56576","#E56B6F")) + scale_y_continuous(limits=c(0,0.015))
ggsave("fig1_1.png",width=10,height=5)

freqalg1_2<-dif_abs_freq(xalg1$X2,p[2])
freqalg2_2<-dif_abs_freq(xalg2$X2,p[2])
freqalg3_2<-dif_abs_freq(xalg3$X2,p[2])
freqalg4_2<-dif_abs_freq(xalg4$X2,p[2])


xbin2<-rbind(cbind(freqalg1_2,alg=replicate(11,1)),cbind(freqalg2_2,alg=replicate(11,2)),cbind(freqalg3_2,alg=replicate(11,3)),cbind(freqalg4_2,alg=replicate(11,4)))
ggplot(xbin2, aes(x=xbin,y=Freq, fill=as.factor(alg)))+geom_bar(stat = "identity", width = 0.6,position=position_dodge(width = 0.65))+scale_x_continuous(breaks=seq(0,10,1))+theme_bw()+
  xlab(label="X")+ylab(label="")+labs(title = "Diferença Absoluta entre a Frequência Relativa e o valor da Função de Probabilidade de X2")+
  scale_fill_manual("Algoritmo",breaks=c("1","2","3","4"), values=c("#355070","#6D597A","#B56576","#E56B6F")) + scale_y_continuous(limits=c(0,0.015))
ggsave("fig2_1.png",width=10,height=5)

freqalg1_3<-dif_abs_freq(xalg1$X3,p[3])
freqalg2_3<-dif_abs_freq(xalg2$X3,p[3])
freqalg3_3<-dif_abs_freq(xalg3$X3,p[3])
freqalg4_3<-dif_abs_freq(xalg4$X3,p[3])

xbin3<-rbind(cbind(freqalg1_3,alg=replicate(11,1)),cbind(freqalg2_3,alg=replicate(11,2)),cbind(freqalg3_3,alg=replicate(11,3)),cbind(freqalg4_3,alg=replicate(11,4)))
ggplot(xbin3, aes(x=xbin,y=Freq, fill=as.factor(alg)))+geom_bar(stat = "identity", width = 0.6,position=position_dodge(width = 0.65))+scale_x_continuous(breaks=seq(0,10,1))+theme_bw()+
  xlab(label="X")+ylab(label="")+labs(title = "Diferença Absoluta entre a Frequência Relativa e o valor da Função de Probabilidade de X3")+
  scale_fill_manual("Algoritmo",breaks=c("1","2","3","4"), values=c("#355070","#6D597A","#B56576","#E56B6F")) + scale_y_continuous(limits=c(0,0.015))
ggsave("fig3_1.png",width=10,height=5)

soma_dif_alg1<-sum(freqalg1_1$Freq)+sum(freqalg1_2$Freq)+sum(freqalg1_3$Freq)
soma_dif_alg2<-sum(freqalg2_1$Freq)+sum(freqalg2_2$Freq)+sum(freqalg2_3$Freq)
soma_dif_alg3<-sum(freqalg3_1$Freq)+sum(freqalg3_2$Freq)+sum(freqalg3_3$Freq)
soma_dif_alg4<-sum(freqalg4_1$Freq)+sum(freqalg4_2$Freq)+sum(freqalg4_3$Freq)

print(c(soma_dif_alg1,soma_dif_alg2,soma_dif_alg3,soma_dif_alg4))
