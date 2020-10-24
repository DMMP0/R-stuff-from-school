# Title     : TODO
# Objective : TODO
# Created by: Maur
# Created on: 22/10/2020
writeLines("\n\n 1. Si costruisca un dataframe, D, che contenga le sei variabili del set di dati di Anscombe.\nRimuovere dalla memoria tutte le variabili escluso il dataframe D.\n\n")

X1 <- c(10,8,13,9,11,14,6,4,12,7,5)
Y1 <- c(8.04,6.95,7.58,8.81,8.33,9.96,7.24,4.26,10.84,4.82,5.68)
Y2 <- c(9.14,8.14,8.74,8.77,9.26,8.10,6.13,3.10,9.13,7.26,4.74)
Y3 <- c(7.46,6.77,12.74,7.11,7.81,8.84,6.08,5.39,8.15,6.42,5.73)
X2 <- c(8,8,8,8,8,8,8,19,8,8,8)
Y4 <- c(6.58,5.76,7.71,8.84,8.47,7.04,5.25,12.50,5.56,7.91,6.89)

D <- data.frame(X1,Y1,Y2,Y3,X2,Y4)

remove(X1,Y1,Y2,Y3,X2,Y4)

writeLines("\n\n 2. Si calcoli il valore della covarianza tra X1 e Y1. Nel calcolo della covarianza si utilizzi la formula matematica descritta a p. 113 del manuale, nello specifico nello svolgimento dell'esercizio nel riprodurre tale formula si sostituisca il fattore moltiplicativo 1/n con il fattore moltiplicativo corretto 1/(n-1).\n\n")

n <- length(D$X1)
cov1 <- (1/(n-1))* sum((D$X1 - mean(D$X1))*(D$Y1 - mean(D$Y1)))

writeLines("\n\n 3. Ripetere l'esercizio 2, questa volta utilizzando la formula predefinita in R, cov(), per il calcolo della covarianza. Verificare che i risultati ottenuti con le due modalità (esercizio 2 e 3) siano coincidenti.\n\n")

cov2 <- cov(D$X1,D$Y1)

if(round(cov1,3) == round(cov2,3))
{
  writeLines("\nSono uguali, la formula dell'esercizio 2 è corretta\n")
} else {
  writeLines("\nI risultati sono diversi, l'esercizio 2 è sbagliato\n")
}

writeLines("\n\n 4. Costruire un nuovo dataframe Dcov che contenga rispettivamente tre variabili (ovvero tre colonne): a) una variabile scartiX1 contenente lo scarto dalla media di X1 dei punteggi di X1, b) una variabile scartiY1 contenente lo scarto dalla media di Y1 dei punteggi di Y1, c) una variabile prodScarti contenente il prodotto degli scarti rispetto a X1 e Y1 (per maggiori dettagli si veda la formula a p. 113).\n\n")

#scartiX1 <- X1 - mean(X1)
Dcov <- data.frame(scartiX1 = D$X1 - mean(D$X1), scartiY1 = D$Y1 - mean(D$Y1),prodScarti = ((D$X1 - mean(D$X1))*(D$Y1 - mean(D$Y1))))

writeLines("\n\n 5. Visualizzare il contenuto di Dcov e concentrarsi sui valori di prodScarti. Quali osservazioni potresti fare circa i valori contenuti in prodScarti? Nello specifico circa il segno di tali scarti (quale è il loro significato e come vanno ad incidere sul risultato finale della misura di covarianza)?\n\n")

Dcov
writeLines("\n\n 6. Calcolare la covarianza della variabile Y1 con se stessa. \nNella procedura di calcolo utilizzare sia la modalità descritta nell'Es. 2 che quella indicata nell'Es. 3.\n Verificare la concordanza dei risultati delle due procedure.\n Infine tabulare i valori del dataframe corrispondente a Dcov per questa nuova coppia di variabili (variabile ripetuta). \nQuali osservazioni puoi fare sui risultati del valore della covarianza di Y1 con se stessa? \nA cosa viene a coincidere tale particolare covarianza? \n\n")

covY1_1 <- (1/(n-1))* sum((D$Y1 - mean(D$Y1))*(D$Y1 - mean(D$Y1)))
covY1_2 <- cov(D$Y1,D$Y1)
if(round(covY1_1,3) == round(covY1_2,3))
{
  writeLines("\nSono uguali, la formula dell'esercizio 6 è corretta\n")
} else {
  writeLines("\nI risultati sono diversi, l'esercizio 6 è sbagliato\n")
}
Dcov <- data.frame(Y1 = D$Y1,Y1 = D$Y1,Cov = covY1_2)
Dcov


writeLines("\n\n7. Calcolare la correlazione lineare tra X1 e Y1. \nPer il calcolo della correlazione utilizzare la formula descritta a p. 114 del manuale e confrontarla con il calcolo della funzione predefinita in R, ovvero la funzione cor(X1,Y1). \n\n")

cor1 <- cov(D$X1,D$Y1)/(sd(D$X1)*sd(D$Y1))
cor2 <- cor(D$X1,D$Y1)

if(round(cor1,3) == round(cor2,3))
{
  writeLines("\nSono uguali, la formula dell'esercizio 7 è corretta\n")
} else {
  writeLines("\nI risultati sono diversi, l'esercizio 7 è sbagliato\n")
}



