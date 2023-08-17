
# Algoritmo (i) da alinea anterior (gera as amostras com menor variancias)

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

##############

set.seed(1)

#(i)

k <- 1
Y <- c()

while(k <= 10000){
  
  X <- mean(replicate(100,binAlg1(10,0.2)))
  
  if(X > 2){
    Y <- c(Y,1)
  }else{
    Y <- c(Y,0)  
  }
  
  k <- k+1
}

# Pontual

mean(Y)

# Intervalar
# Aproxima??o ? Normal(0,1) pelo TLC

a <- qnorm(0.025,0,1)
b <- qnorm(0.975,0,1)
s <- sd(Y)

IC <- c(mean(Y)+a*s/sqrt(length(Y)),mean(Y)+b*s/sqrt(length(Y)))
IC

########

# (ii)

# Estudo Piloto

# Gerar as duas amostras, Y, resultante do m?todo de Monte-Carlo, e Z~U(0,1)

k <- 1
Y <- c()

while(k <= 10000){
  
  X <- mean(replicate(100,binAlg1(10,0.2)))
  
  if(X > 2){
    Y <- c(Y,1)
  }else{
    Y <- c(Y,0)  
  }
  
  k <- k+1
}

Z <- runif(10000,0,1)

# Calcular c*

c <- -cov(Y,Z)/(1/12)

# Simula??o Principal

# Voltar a gerar as amostras

k <- 1
Y <- c()

while(k <= 10000){
  
  X <- mean(replicate(100,binAlg1(10,0.2)))
  
  if(X > 2){
    Y <- c(Y,1)
  }else{
    Y <- c(Y,0)  
  }
  
  k <- k+1
}

Z <- runif(10000,0,1)

# Calcular a amostra Tc

EZ <- 0.5
i <- 1
Tc <- c()

while(i <= 10000){
  ti <- Y[[i]] + c*(Z[[i]] - EZ)
  Tc <- c(Tc, ti)
  i <- i+1
}

# Pontual

mean(Tc)

# Intervalar

# Aproxima??o ? normal pelo TLC

a <- qnorm(0.025,0,1)
b <- qnorm(0.975,0,1)
s <- sd(Tc)

IC <- c(mean(Tc)+a*s/sqrt(length(Tc)),mean(Tc)+b*s/sqrt(length(Tc)))
IC

var(Tc)/var(Y)

