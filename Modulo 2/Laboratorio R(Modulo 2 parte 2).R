# Title     : TODO
# Objective : TODO
# Created by: DMMP0
# Created on: 12/10/2020



writeLines("1.\n\nCostruire un dataframe D riproducente la tabella sopra riportata e rimuovere le singole variabili dal workspace. \nUtilizzare la funzione summary() per visualizzare i risultati delle principali statistiche descrittive per ciascuna delle quattro variabili separatamente. \n")

X1 <- c(41.61, 46.93, 50.33, 53.93, 51.33, 51.55, 52.31, 53.07, 53.00, 51.07, 51.98, 44.96, 46.17, 54.63, 45.53, 51.23, 54.07, 46.25, 51.04, 55.17)
X2 <- c(48.87, 54.81, 38.10, 43.16, 38.52, 40.35, 53.18, 54.50, 55.66, 46.45, 50.53, 58.63, 62.26, 50.17, 57.76, 64.68, 49.38, 42.64, 57.46, 34.93)
X3 <- c(64.50, 51.73, 72.01, 27.77, 64.81, 69.12, 50.44, 44.83, 59.05, 63.12, 65.14, 53.67, 58.58, 54.73, 51.61, 46.07, 52.12, 41.23, 60.84, 56.50)
X4 <- c(56.46, 64.31, 58.61, 59.59, 61.36, 58.10, 56.34, 56.01, 59.54, 57.65, 54.71, 61.24, 56.67, 59.40, 53.78, 63.97, 61.91, 54.86, 62.88, 56.14)

D <- data.frame(X1,X2,X3,X4)
remove(X1,X2,X3,X4)

summary(D$X1)
summary(D$X2)
summary(D$X3)
summary(D$X4)

writeLines("\n\n2.\n\n Costruire un grafico multiplo che contenga i quattro istogrammi associati rispettivamente alle variabili X1, X2, X3 e X4.")

par(mfrow=c(2,2)) #come fare il layout dei grafici, cosi ne crea uno 2x2

hist(D$X1,xlim = c(20,80),breaks = "Sturges")
hist(D$X2,xlim = c(20,80),breaks = "Sturges")
hist(D$X3,xlim = c(20,80),breaks = "Sturges")
hist(D$X4,xlim = c(20,80),breaks = "Sturges")

writeLines("\n\n3.\n\n Per ciascuna variabile separatamente esplorare l'effetto di rappresentazione dell'istogramma in funzione di un differente numero di bin. \nAllo scopo utilizzare il parametro opportuno nella funzione di rappresentazione grafica dell'istogramma per variare il numero di bin dello stesso. \nProvare a commentare qualitativamente il risultato di tali variazioni.")


hist(D$X1,xlim = c(20,80),breaks = 5)
hist(D$X2,xlim = c(20,80),breaks = 5)
hist(D$X3,xlim = c(20,80),breaks = 5)
hist(D$X4,xlim = c(20,80),breaks = 5)
#restano tutti uguali

hist(D$X1,xlim = c(20,80),breaks = 9)
hist(D$X2,xlim = c(20,80),breaks = 9)
hist(D$X3,xlim = c(20,80),breaks = 9)
hist(D$X4,xlim = c(20,80),breaks = 9)
#x3 e x4 cambiano molto, non assomigliano piu a normali, mentre prima c'era una vaga somiglianza

hist(D$X1,xlim = c(20,80),breaks = 20)
hist(D$X2,xlim = c(20,80),breaks = 20)
hist(D$X3,xlim = c(20,80),breaks = 20)
hist(D$X4,xlim = c(20,80),breaks = 20)
#x3 mostra delle differenze tra 50 e 60, x4 invece viene appiattito nel mezzo. I bin di X1 e X2 vengono semplicemente "duplicati"

hist(D$X1,xlim = c(20,80),breaks = 120)
hist(D$X2,xlim = c(20,80),breaks = 120)
hist(D$X3,xlim = c(20,80),breaks = 120)
hist(D$X4,xlim = c(20,80),breaks = 120)
#i grafici sono illeggibili

writeLines("\n\n5.\n\n Utilizzando la funzione quantile(), visualizzare i principali quantili delle quattro variabili di D.\n Per la sola variabile X2 visualizzare i quantili associati ai valori di probabilità 0.15, 0.33, 0.46, 0.89, 0.98, 0.99, 1.0;\n hai alcune osservazioni da fare sui risultati dei quantili di X2?")

quantile(D$X1)
quantile(D$X2, probs = c(0.15, 0.33, 0.46, 0.89, 0.98, 0.99, 1.0))
quantile(D$X3)
quantile(D$X4)

writeLines("\n\n6.\n\n Costruire un grafico unico che contiene i quattro grafici boxplot associati rispettivamente alle variabili X1, X2, X3 e X4.")

boxplot(D$X1,D$X2,D$X3,D$X4,names = c("X1","X2","X3","X4"))

#7.
# Per ciascuna variabile in D verificare se vi siano o meno dei valori anomali (outliers) nella corrispettiva distribuzione.
# Creare un vettore di tipo logico di lunghezza 4 che codifica per ciascuna variabile la presenza o meno di valori anomali
# (secondo i valori TRUE/FALSE).
# Nota: per questo tipo di esercizio l'assegnazione dei valori logici va fatta manualmente.")

#dati i plotbox, solo X3 ha degli outliers
outliers <- c(FALSE,FALSE,TRUE,FALSE)

#8.
# Verificare (separamente per ogni caso, vedi in sequito) attraverso la sola ispezione grafica dei differenti boxplot se
# 1) la distribuzione di X1 sia inclusa in quella di X2
# 2) la mediana di X1 sia superiore alla mediana di X3
# 3) il minimo di X2 sia superiore al minimo di X3
# 4) la differenza interquartilica di X1 sia superiore a quella di X4
# 5) il baffo superiore di X3 coincida con il valore massimo di X3
# 6) il baffo inferiore di X3 coincida con il minimo di X3.
# Infine, individuare quale tra le 4 variabili presenti un qualche livello di asimmetria a destra e quale tra le 4 variabili
# presenti un qualche livello di asimmetria a sinistra.

#Dati i plotbox:
#1) La distribuzione di X1 è inclusa in quella di X2 ;
#2) No, è inferiore
#3) E superiore, dato che X3 ha il minimo più basso di tutti
#4) La differenza tra i quartili di X1 è molto più marcata rispetto a X4
#5) Sì, coincide
#6) Non coincide
#X1 ha asimmetria a sinistra, X3 a desta