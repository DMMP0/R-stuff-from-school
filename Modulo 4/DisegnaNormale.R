############################################################
############################################################
# Distribuzione di densità Normale
# Integrali su intervalli di quantili
############################################################
############################################################


# Grafico generale #########################################

disegnaNorm <- function(limIX,limSX,media,ds,quantX) {
  
  curve(dnorm(x,media,ds),limIX,limSX,axes=F,xlab="",ylab="",main=paste("Distribuzione Normale: dev.st = ",ds, sep = ""))

  
  abline(h=0)
  
  axis(1,c(limIX,media,quantX,limSX),c("",media,quantX,""))
  
  points(quantX,dnorm(quantX,media,ds),cex=2)
  points(c(media,media),c(0,dnorm(media,media,ds)),type="l",lty=4,lwd=2)
  
}


# Grafico generale cumulata #################################

disegnaNormC <- function(limIX,limSX,media,ds,quantX) {
  
  curve(pnorm(x,media,ds),limIX,limSX,axes=F,xlab="",ylab="",main=paste("Distribuzione Normale: dev.st = ",ds, sep = ""))
  
  abline(h=0)
  
  axis(1,c(limIX,media,quantX,limSX),c("",media,quantX,""))
  
  points(quantX,pnorm(quantX,media,ds),cex=2)
  points(c(media,media),c(0,pnorm(media,media,ds)),type="l",lty=4,lwd=2)
  
}


# Integrale parte DX #######################################

disegnaNormDX <- function(limIX,limSX,media,ds,quantX) {
  
  curve(dnorm(x,media,ds),limIX,limSX,axes=F,xlab="",ylab="",main=paste("Distribuzione Normale: dev.st = ",ds, sep = ""))
  
  abline(h=0)
  
  axis(1,c(limIX,media,quantX,limSX),c("",media,quantX,""))
  
  vals <- seq(quantX,limSX,length=100)
  x <- c(quantX,vals,limSX,quantX)
  y <- c(0,dnorm(vals,media,ds),0,0)
  polygon(x,y,density=20,angle=45)
  points(c(media,media),c(0,dnorm(media,media,ds)),type="l",lty=4,lwd=2)
  
}


# Integrale parte SX #######################################

disegnaNormSX <- function(limIX,limSX,media,ds,quantX) {
  
  curve(dnorm(x,media,ds),limIX,limSX,axes=F,xlab="",ylab="",main=paste("Distribuzione Normale: dev.st = ",ds, sep = ""))
  
  abline(h=0)
  
  axis(1,c(limIX,media,quantX,limSX),c("",media,quantX,""))
  
  vals <- seq(limIX,quantX,length=100)
  x <- c(limIX,vals,quantX,limIX)
  y <- c(0,dnorm(vals,media,ds),0,0)
  polygon(x,y,density=20,angle=45)
  points(c(media,media),c(0,dnorm(media,media,ds)),type="l",lty=4,lwd=2)
  
}


# Integrale parte Esterna ##################################

disegnaNormEst <- function(limIX,limSX,media,ds,quantX1,quantX2) {
  
  curve(dnorm(x,media,ds),limIX,limSX,axes=F,xlab="",ylab="",main=paste("Distribuzione Normale: dev.st = ",ds, sep = ""))
  
  abline(h=0)
  
  axis(1,c(limIX,media,quantX1,quantX2,limSX),c("",media,quantX1,quantX2,""))
  
  vals <- seq(limIX,quantX1,length=100)
  x <- c(limIX,vals,quantX1,limIX)
  y <- c(0,dnorm(vals,media,ds),0,0)
  polygon(x,y,density=20,angle=45)
  
  vals <- seq(quantX2,limSX,length=100)
  x <- c(quantX2,vals,limSX,quantX2)
  y <- c(0,dnorm(vals,media,ds),0,0)
  polygon(x,y,density=20,angle=45)
  
  points(c(media,media),c(0,dnorm(media,media,ds)),type="l",lty=4,lwd=2)
  
}


# Integrale parte Interna ##################################

disegnaNormInt <- function(limIX,limSX,media,ds,quantX1,quantX2) {
  
  curve(dnorm(x,media,ds),limIX,limSX,axes=F,xlab="",ylab="",main=paste("Distribuzione Normale: dev.st = ",ds, sep = ""))
  
  abline(h=0)
  
  axis(1,c(limIX,media,quantX1,quantX2,limSX),c("",media,quantX1,quantX2,""))
  
  vals <- seq(quantX1,quantX2,length=100)
  x <- c(quantX1,vals,quantX2,quantX1)
  y <- c(0,dnorm(vals,media,ds),0,0)
  polygon(x,y,density=20,angle=45)
  
  points(c(media,media),c(0,dnorm(media,media,ds)),type="l",lty=4,lwd=2)
  
}


###########################################################
# Esempi 

Media <- 10
Ds <- 5
limInfX <- qnorm(0.001,Media,Ds)
limSupX <- qnorm(0.999,Media,Ds)
Y <- c(3,8,12,19,14)
quantileX1 <- 2
quantileX2 <- 8

disegnaNorm(limInfX,limSupX,Media,Ds,X)
disegnaNormC(limInfX,limSupX,Media,Ds,X)
disegnaNormDX(limInfX,limSupX,Media,Ds,quantileX1)
disegnaNormSX(limInfX,limSupX,Media,Ds,quantileX1)
disegnaNormEst(limInfX,limSupX,Media,Ds,quantileX1,quantileX2)
disegnaNormInt(limInfX,limSupX,Media,Ds,quantileX1,quantileX2)

