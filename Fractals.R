# Title     : TODO
# Objective : TODO
# Created by: Maur
# Created on: 15/10/2020

Serp <- function (A,B,C,name="Serpinsky Triange",points = 1000,result = FALSE){
 #es
 # A <- c(0,0)
 # B <- c(300,0)
 # C <- c(150,300)

  P <- sample(min(A[1],B[1],C[1]):max(A[2],B[2],C[2]),2,TRUE)

  nex <- 1:3

  risX <- P[1]
  risY <- P[2]

  for (i in 0:points)
  {
    n <- sample(nex, 1)
    ifelse(n == 1,{P[1] <- (A[1]+P[1])/2;P[2] <- (A[2]+P[2])/2 },
           ifelse(n == 2,{P[1] <- (B[1]+P[1])/2;P[2] <- (B[2]+P[2])/2 },
                  {P[1] <- (C[1]+P[1])/2;P[2] <- (C[2]+P[2])/2 }))
    risX<-append(risX,P[1])
    risY<-append(risY,P[2])

  }


  plot(risX,risY,main = name,xlim = c(0,max(A[1],B[1],C[1])),ylim = c(0,max(A[2],B[2],C[2])))
  points(A[1],A[2],pch=15,cex = 2)
  points(B[1],B[2],pch=15,cex = 2)
  points(C[1],C[2],pch=15,cex = 2)
  points(mean(risX),mean(risY),pch=3,cex=5)
  if(result)
  {return(data.frame(X = risX,Y=risY))}
}

pn2 <- function(A,B,C,D,E,name="???",points=3000,result = FALSE) {
  #pentagono, non lo stesso 2 volte
  P <- sample(min(A[1],B[1],C[1],D[1],E[1]):max(A[2],B[2],C[2],D[2],E[2]),2,TRUE)
  nex <- 1:5
  risX <- P[1]
  risY <- P[2]

  prev <- 0

  for (i in 0:points)
  {
    n <- sample(nex, 1)
    while(n == prev)
    {
      n <- sample(nex, 1)
    }
    prev <- n
    ifelse(n == 1,{P[1] <- (A[1]+P[1])/2;P[2] <- (A[2]+P[2])/2 },
           ifelse(n == 2,{P[1] <- (B[1]+P[1])/2;P[2] <- (B[2]+P[2])/2 },
                  ifelse(n == 3,{P[1] <- (C[1]+P[1])/2;P[2] <- (C[2]+P[2])/2 },
                         ifelse(4 == 2,{P[1] <- (D[1]+P[1])/2;P[2] <- (D[2]+P[2])/2 },
                  {P[1] <- (E[1]+P[1])/2;P[2] <- (E[2]+P[2])/2 }))))
    risX<-append(risX,P[1])
    risY<-append(risY,P[2])


  }

  plot(risX,risY,main = name,xlim = c(0,max(A[1],B[1],C[1],D[1],E[1])),ylim = c(0,max(A[2],B[2],C[2],D[2],E[2])))
  points(A[1],A[2],pch=15,cex = 2)
  points(B[1],B[2],pch=15,cex = 2)
  points(C[1],C[2],pch=15,cex = 2)
  points(D[1],D[2],pch=15,cex = 2)
  points(E[1],E[2],pch=15,cex = 2)
  points(mean(risX),mean(risY),pch=3,cex=5)

  if(result)
  {return(data.frame(X = risX,Y=risY))}

  }

q2 <- function(A,B,C,D,name="???",points=3000,result = FALSE) {
  #pentagono, non lo stesso 2 volte
  P <- sample(min(A[1],B[1],C[1],D[1]):max(A[2],B[2],C[2],D[2]),2,TRUE)
  nex <- 1:4
  risX <- P[1]
  risY <- P[2]

  prev <- 0

  for (i in 0:points)
  {
    n <- sample(nex, 1)
    while(n == prev)
    {
      n <- sample(nex, 1)
    }
    prev <- n
    ifelse(n == 1,{P[1] <- (A[1]+P[1])/2;P[2] <- (A[2]+P[2])/2 },
           ifelse(n == 2,{P[1] <- (B[1]+P[1])/2;P[2] <- (B[2]+P[2])/2 },
                  ifelse(n == 3,{P[1] <- (C[1]+P[1])/2;P[2] <- (C[2]+P[2])/2 },
                         {P[1] <- (D[1]+P[1])/2;P[2] <- (D[2]+P[2])/2 })))
    risX<-append(risX,P[1])
    risY<-append(risY,P[2])


  }

  plot(risX,risY,main = name,xlim = c(0,max(A[1],B[1],C[1],D[1])),ylim = c(0,max(A[2],B[2],C[2],D[2])))
  points(A[1],A[2],pch=15,cex = 2)
  points(B[1],B[2],pch=15,cex = 2)
  points(C[1],C[2],pch=15,cex = 2)
  points(D[1],D[2],pch=15,cex = 2)
  points(mean(risX),mean(risY),pch=3,cex=5)

  if(result)
    {return(data.frame(X = risX,Y=risY))}

}

arrow <- function(A,B,C,D,E,name="Arrow",points=3000,result = FALSE) {
  #pentagono, non a distanza di 1 dai 2 precedenti
  P <- sample(min(A[1],B[1],C[1],D[1],E[1]):max(A[2],B[2],C[2],D[2],E[2]),2,TRUE)
  nex <- 1:5
  risX <- P[1]
  risY <- P[2]

  prev1 <- 0
  prev2 <- 0

  time <- FALSE

  for (i in 0:points)
  {
    n <- sample(nex, 1)
    while(abs(n - prev1) == 1 | abs(n - prev1) == 4| abs(n - prev2) == 1 | abs(n - prev2) == 4 )
    {
      n <- sample(nex, 1)
    }
    if(time)
    {prev1 <- n} else {
      prev2 <- n
    }
    time <- !time
    ifelse(n == 1,{P[1] <- (A[1]+P[1])/2;P[2] <- (A[2]+P[2])/2 },
           ifelse(n == 2,{P[1] <- (B[1]+P[1])/2;P[2] <- (B[2]+P[2])/2 },
                  ifelse(n == 3,{P[1] <- (C[1]+P[1])/2;P[2] <- (C[2]+P[2])/2 },
                         ifelse(4 == 2,{P[1] <- (D[1]+P[1])/2;P[2] <- (D[2]+P[2])/2 },
                                {P[1] <- (E[1]+P[1])/2;P[2] <- (E[2]+P[2])/2 }))))
    risX<-append(risX,P[1])
    risY<-append(risY,P[2])


  }

  plot(risX,risY,main = name,xlim = c(0,max(A[1],B[1],C[1],D[1],E[1])),ylim = c(0,max(A[2],B[2],C[2],D[2],E[2])))
  points(A[1],A[2],pch=15,cex = 2)
  points(B[1],B[2],pch=15,cex = 2)
  points(C[1],C[2],pch=15,cex = 2)
  points(D[1],D[2],pch=15,cex = 2)
  points(E[1],E[2],pch=15,cex = 2)
  points(mean(risX),mean(risY),pch=3,cex=5)

  if(result)
  {return(data.frame(X = risX,Y=risY))}

}

ps <- function(A,B,C,D,E,name="Arrow",points=3000,result = FALSE) {
  #pentagono, non a distanza di 1
  P <- sample(min(A[1],B[1],C[1],D[1],E[1]):max(A[2],B[2],C[2],D[2],E[2]),2,TRUE)
  nex <- 1:5
  risX <- P[1]
  risY <- P[2]

  prev <- 0

  for (i in 0:points)
  {
    n <- sample(nex, 1)
    while(abs(n - prev) == 1 )
    {
      n <- sample(nex, 1)
    }

    prev <- n

    ifelse(n == 1,{P[1] <- (A[1]+P[1])/2;P[2] <- (A[2]+P[2])/2 },
           ifelse(n == 2,{P[1] <- (B[1]+P[1])/2;P[2] <- (B[2]+P[2])/2 },
                  ifelse(n == 3,{P[1] <- (C[1]+P[1])/2;P[2] <- (C[2]+P[2])/2 },
                         ifelse(4 == 2,{P[1] <- (D[1]+P[1])/2;P[2] <- (D[2]+P[2])/2 },
                                {P[1] <- (E[1]+P[1])/2;P[2] <- (E[2]+P[2])/2 }))))
    risX<-append(risX,P[1])
    risY<-append(risY,P[2])


  }

  plot(risX,risY,main = name,xlim = c(0,max(A[1],B[1],C[1],D[1],E[1])),ylim = c(0,max(A[2],B[2],C[2],D[2],E[2])))
  points(A[1],A[2],pch=15,cex = 2)
  points(B[1],B[2],pch=15,cex = 2)
  points(C[1],C[2],pch=15,cex = 2)
  points(D[1],D[2],pch=15,cex = 2)
  points(E[1],E[2],pch=15,cex = 2)
  points(mean(risX),mean(risY),pch=3,cex=5)

  if(result)
  {return(data.frame(X = risX,Y=risY))}

}


#Serp(c(0,0),c(300,0),c(150,300))
#pn2(c(400,0),c(0,300),c(180,660),c(620,660),c(800,300))
#arrow(c(400,0),c(0,300),c(180,660),c(620,660),c(800,300))
#ps(c(400,0),c(0,300),c(180,660),c(620,660),c(800,300))
q2(c(0,0),c(800,0),c(800,800),c(0,800))