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