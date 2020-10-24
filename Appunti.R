# Title     : Main
# Objective : test vari
# Created by: DMMP0
# Created on: 24/09/2020

#esempi fatti a lezione o a caso
#e pezzi di codice utili

#visualizza il contenuto del workspace
ls.str()

#rimuove tutte le variabili tranne le funzioni
rm(list = setdiff(ls(), lsf.str()))

#pulisce la console (solo su Rstudio)
cat("\014")

Y2 <- runif(11,0.0,20.0)  #sceglie 11 numeri a caso tra 0 e 20




boi <-ifelse(runif(50, 0, 10) <= 5, 'm', 'f') #ifelse è come condizione?true:false

rm(list = setdiff(ls(), lsf.str()))