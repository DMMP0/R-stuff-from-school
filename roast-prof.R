# Title     : TODO
# Objective : TODO
# Created by: Maur
# Created on: 14/10/2020

ptm <- proc.time()
boi <-ifelse(runif(1000000, 0, 10) <= 5, 'm', 'f') #ifelse è come condizione?true:false
boi2 <-ifelse(runif(1000000, 0, 10) <= 5, 'm', 'f')
table(boi,boi2)
me <- proc.time() - ptm

#summary() riassume il contenuto di strutture di dati di tipo numerico
ptm <- proc.time()
boi <- rnorm(1000000,0.5,0.05)
boi <- round(boi)
boi2 <- rnorm(1000000,0.5,0.05)
boi2 <- round(boi2)
boi <- as.factor(boi)
boi2 <- as.factor(boi2)
table(boi,boi2)
prof <- proc.time() - ptm
me
prof
writeLines("-----------------------------------------")
#6 lvl

ptm <- proc.time()
boi <-as.factor(sample(-3:2,1000000, replace = TRUE))
boi2 <-as.factor(sample(-3:2,1000000, replace = TRUE))
table(boi,boi2)
me2 <- proc.time() - ptm

#summary() riassume il contenuto di strutture di dati di tipo numerico
ptm <- proc.time()
boi <- rnorm(1000000,0.5,0.5)
boi <- round(boi)
boi2 <- rnorm(1000000,0.5,0.5)
boi2 <- round(boi2)
boi <- as.factor(boi)
boi2 <- as.factor(boi2)
table(boi,boi2)
prof2 <- proc.time() - ptm
me2
prof2

rm(list = setdiff(ls(), lsf.str()))

#roast BarreResidui --------------------------------------------*

BarreResidui2 <- function(X,Y,s=1.7,nameX = "Variabile indipendente",nameY="Variabile dipendente") {

  scaleX <- s*(max(X)-min(X))
  scaleY <- s*(max(Y)-min(Y))
  plot(X,Y,pch=16,cex=1.5,xlim=c(min(X)-scaleX,max(X)+scaleX),ylim=c(min(Y)-scaleY,max(Y)+scaleY),xlab=nameX,ylab=nameY)
  LM <- lm(Y ~ X)
  abline(LM,lwd=2)

  N <- seq_along(X)

  for (i in N) {

    ystar <- predict(LM)[i]
    points(c(X[i],X[i]),c(Y[i],ystar),type="l",lty=2)

  }

}

boi <-sample(4:14,100, replace = TRUE)
boi2 <-sample(4:14,100, replace = TRUE)

ptm <- proc.time()
BarreResidui(boi,boi2,1.7,"X1","Y1")
prof <- proc.time() - ptm

ptm <- proc.time()
BarreResidui2(boi,boi2)
me <- proc.time() - ptm

me
prof


