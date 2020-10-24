# Title     : TODO
# Objective : TODO
# Created by: Maur
# Created on: 23/10/2020

disegnaChiQ <- function(limIX,limSX,gdl,quantX,Sx){

  curve(dchisq(x,df=gdl),limIX,limSX,axes=F,xlab="",ylab="",main= paste0("Distribuzione Chi2: g.d.l. = ", gdl))
  abline(h=0)
  axis(1,c(limIX,quantX,limSX),c("0",quantX,""))
  vals <- seq(quantX,limSX,length=100)
  if(Sx)
  {
    x <- c(quantX,vals,limSX,quantX)
  } else {
    x <- c(limIX,vals,quantX,limIX)
  }
  y <- c(0,dchisq(vals,df=gdl),0,0)
  polygon(x,y,density=20,angle=45)

}

disegnaNorm <- function(limIX,limSX,media,ds,quantX1,quantX2=0,type = "") {

  if(type=="C"){
    curve(pnorm(x,media,ds),limIX,limSX,axes=F,xlab="",ylab="",main= paste0("Distribuzione Normale: dev.st = ", ds))
  } else{
    curve(dnorm(x,media,ds),limIX,limSX,axes=F,xlab="",ylab="",main= paste0("Distribuzione Normale: dev.st = ", ds))
  }
  abline(h=0)
  axis(1,c(limIX,media,quantX1,limSX),c("",media,quantX1,""))

  #casi
  if(type=="C")
  {  points(quantX1,pnorm(quantX1,media,ds),cex=2)
     points(c(media,media),c(0,pnorm(media,media,ds)),type="l",lty=4,lwd=2)
     return("")
  }else{
    if(type=="DX") {
      vals <- seq(quantX1,limSX,length=100)
      x <- c(quantX1,vals,limSX,quantX1)
      y <- c(0,dnorm(vals,media,ds),0,0)
      polygon(x,y,density=20,angle=45)
    }else{
      if(type=="SX")
      {
        vals <- seq(limIX,quantX1,length=100)
        x <- c(limIX,vals,quantX1,limIX)
        y <- c(0,dnorm(vals,media,ds),0,0)
        polygon(x,y,density=20,angle=45)
      }else{
        if(type=="EST")
        {
          if(quantX2 == 0){
            simpleError("Inserire il secondo quantile")
          }
          vals <- seq(limIX,quantX1,length=100)
          x <- c(limIX,vals,quantX1,limIX)
          y <- c(0,dnorm(vals,media,ds),0,0)
          polygon(x,y,density=20,angle=45)

          vals <- seq(quantX2,limSX,length=100)
          x <- c(quantX2,vals,limSX,quantX2)
          y <- c(0,dnorm(vals,media,ds),0,0)
          polygon(x,y,density=20,angle=45)
        }else{
          if(type=="INT")
          {
            if(quantX2 == 0){
              simpleError("Inserire il secondo quantile")
            }

            vals <- seq(quantX1,quantX2,length=100)
            x <- c(quantX1,vals,quantX2,quantX1)
            y <- c(0,dnorm(vals,media,ds),0,0)
            polygon(x,y,density=20,angle=45)
          }else{
              points(quantX1,dnorm(quantX1,media,ds),cex=2)
              }
          }
        }
      }
    }
  points(c(media,media),c(0,dnorm(media,media,ds)),type="l",lty=4,lwd=2)
}

disegnat <- function (limIX,limSX,gdl,quantX1,quantX2=0,type = "SX" )
{
  curve(dt(x,df=gdl),limIX,limSX,axes=F,xlab="",ylab="",main= paste0("Distribuzione t di Student: g.d.l. = ", gdl))

  lines(c(0,0),c(0,dt(0,df=gdl)),lty=4)
  abline(h=0)

  if(type == "SX"){
    axis(1,c(limIX,quantX1,0,limSX),c("",quantX1,0,""))

    vals <- seq(limIX,quantX1,length=100)
    x <- c(limIX,vals,quantX1,limIX)
    y <- c(0,dt(vals,df=gdl),0,0)
    polygon(x,y,density=20,angle=45)
  } else {
    if(type == "DX") {
      axis(1,c(limIX,quantX1,0,limSX),c("",quantX1,0,""))

      vals <- seq(quantX1,limSX,length=100)
      x <- c(quantX1,vals,limSX,quantX1)
      y <- c(0,dt(vals,df=gdl),0,0)
      polygon(x,y,density=20,angle=45)
    } else {
      if(type == "EST") {
        axis(1,c(limIX,quantX1,0,quantX2,limSX),c("",quantX1,0,quantX2,""))

        vals <- seq(limIX,quantX1,length=100)
        x <- c(limIX,vals,quantX1,limIX)
        y <- c(0,dt(vals,df=gdl),0,0)
        polygon(x,y,density=20,angle=45)

        vals <- seq(quantX2,limSX,length=100)
        x <- c(quantX2,vals,limSX,quantX2)
        y <- c(0,dt(vals,df=gdl),0,0)
        polygon(x,y,density=20,angle=45)
      } else {
        if(type == "INT"){
          axis(1,c(limIX,quantX1,0,quantX2,limSX),c("",quantX1,0,quantX2,""))

          vals <- seq(quantX1,quantX2,length=100)
          x <- c(quantX1,vals,quantX2,quantX1)
          y <- c(0,dt(vals,df=gdl),0,0)
          polygon(x,y,density=20,angle=45)
        } }
    }
  }
}

GDL <- 19
limInfX <- qt(0.001,GDL)
limSupX <- qt(0.999,GDL)
quantileX1 <- -0.7822
quantileX2 <- 0.7822

disegnat(limInfX,limSupX,GDL,quantileX1,type="DX")
disegnat(limInfX,limSupX,GDL,quantileX1,type="SX")
disegnat(limInfX,limSupX,GDL,quantileX1,quantileX2,type="EST")
disegnat(limInfX,limSupX,GDL,quantileX1,quantileX2,type="INT")

