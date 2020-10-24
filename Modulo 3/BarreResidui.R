##################################################################
# Funzione per la rappresentazione grafica, plot, di una 
# struttura dati bivariata (X,Y) con visualizzazione della
# retta di regressione tra X ed Y e associati residui
# del modello lineare
##################################################################

BarreResidui <- function(x,y,s,nameX,nameY) {
  
  X <- x
  Y <- y
  scaleX <- s*(max(X)-min(X))
  scaleY <- s*(max(Y)-min(Y))
  plot(X,Y,pch=16,cex=1.5,xlim=c(min(X)-scaleX,max(X)+scaleX),ylim=c(min(Y)-scaleY,max(Y)+scaleY),xlab=nameX,ylab=nameY)
  LM <- lm(Y ~ X)
  abline(LM,lwd=2)
  
  N <- length(X)
  
  for (i in 1:N) {
    
    ystar <- predict(LM)[i]
    points(c(X[i],X[i]),c(Y[i],ystar),type="l",lty=2)
    
  }
  
}

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
