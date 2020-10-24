# Title     : TODO
# Objective : TODO
# Created by: Maur
# Created on: 02/10/2020



default <- function ()
{
  X1 <- c(12,13,11,11,13,9,5,11,13,11,6)
  X2 <- c(12.0,15.3,13.3,17.2,13.6,12.4,11.7,10.0,11.0,18.3,15.3)
  X3 <- c(1,1,0,0,1,0,1,0,0,0,1)
  X4 <- c('m','m','m','f','f','m','m','f','m','f','m')
  X5 <- c(1,1,2,1,3,4,4,1,2,1,2)
  TABa <- data.frame(X1,X2,X3,X4,X5)
  return(TABa)
}




#per l'esercizio 1
Y2 <- round(runif(11,0.0,20.0),5 ) #sceglie 11 numeri a caso tra 0 e 20
P5 <- c(sample(5:13,1),round(runif(1,10.0,20.0),1  ),sample(c(0,1),1),sample(c('f','m'),1),sample(1:4,1))

writeLines("Y2: ");Y2
writeLines('P5');P5
#esercizio 1
writeLines("Esercizio 1\n")

TABa <- default()

#c(10,10,1,'f',4)<-c(10,10,1,'f',4)
a <- c(10,10,1,f,4)
a <-TABa[3,]

#q()
writeLines("TABa:")
TABa
#1. Visualizzare la terza osservazione della variabile X2 e la quinta osservazione della variabile
writeLines("\nVisualizzare la terza osservazione della variabile X2 e la quinta osservazione della variabile X4.\n")
TABa$X2[3]
TABa$X4[5]



#X4. Modificare le due osservazioni nella tabella assegnando rispettivamente i valori 25 e m.
TABa$X2[3] <- 25
TABa$X4[5] <-'m'
writeLines("Modificare le due osservazioni nella tabella assegnando rispettivamente i valori 25 e m.")
TABa



#2. Visualizzare solo la variabile X2. TABassegnare questa variabile ad una nuova variabile chiamata Y2.
writeLines("Visualizzare solo la variabile X2. Assegnare questa variabile ad una nuova variabile chiamata Y2.\n")

TABa$X2 <- Y2

# Verificare se nella nuova variabile vi `e qualche valore maggiore di 10.
writeLines("# Verificare se nella nuova variabile vi `e qualche valore maggiore di 10.")

TABa[2,TABa[2,]>10]


#3. Visualizzare il profilo associato alla quinta osservazione. Assegnare tale profilo ad una nuova
#variabile chiamata P5.
writeLines("Visualizzare il profilo associato alla quinta osservazione.")
TABa[5,]
writeLines("\n Assegnare tale profilo ad una nuova variabile chiamata P5.")
TABa[5,] <- P5
#TABa[5,]  debug


#In questa nuova variabile verificare la condizione che essa contenga
#nella opportuna posizione il valore m (maschio).
writeLines("In questa nuova variabile verificare la condizione che essa contenga nella opportuna posizione il valore m (maschio).")
TABa[5,4]
ifelse(TABa[5,4]== 'm',"Continene il valore m","Non contiene il valore m")

rm(list = setdiff(ls(), lsf.str()))




#es 2

writeLines("\n\n\nEsercizio 2\n\n\n")

TABa <- default()
#es1

A2 <- round(runif(11,10.0,20.0),5)
writeLines('A2:');A2
A4 <- sample(c('m','f'),11,TRUE)
writeLines('A4:');A4

writeLines("Tab A:")
TABa

#Per lo svolgimento dei seguenti esercizi si faccia uso dell’operatore di selezione [] per
#filtrare e/o modificare il contenuto del dataframe

writeLines("\nSi assegni la variabile X2 del data frame TABa ad una nuova variabile A2 e su questa venga
effettuato un filtro che seleziona solo quegli elementi superiori a 13.\n Il risultato del filtro
venga assegnato ad una variabile B2. Visualizzare il suo contenuto.")

TABa[,2] <- A2
B2 <- TABa[TABa[,2]>13,]
B2

writeLines("\nSi assegni la variabile X4 del data frame TABa ad una nuova variabile A4. \nSu questa si
applichi il filtro di selezione dei maschi (m). \nSi assegni il risultato alla stessa variabile A4.
\nVisualizzare il suo contenuto.")

TABa[,4] <- A4
A4 <- TABa[TABa[,4] == 'm',]
A4

writeLines("\nVisualizzare il contenuto del workspace\n")

ls.str()

writeLines("Selezionare solo quelle righe di TABa che sono associate al gruppo deli maschi.\n Assegnare il
risultato di tale selezione ad un nuovo oggetto data frame chiamato TABaM.\n Ripetere lo stesso
procedimento per filtrare il gruppo delle femmine e assegnare il risultato ad un nuovo data
frame TABaF.")

TABaM <- data.frame(TABa[TABa[,4] == 'm',])
TABaF <- data.frame(TABa[TABa[,4] == 'f',])
TABaF
TABaM


writeLines("Selezionare i profili (righe) di TABa in maniera tale che le osservazioni per la variabile X1
siano maggiori di 10 e quelle per la variabile X3 siano uguali a 0.\n Assegnare il risultato a
A2.")

A2 <- TABa[TABa[,1]>10 & TABa[,3]==0, ]
A2
writeLines(" \nRipetere la procedura appena descritta, ma in questo caso selezionare quei valori di
X1 che siano compresi tra 9 e 12 (inclusi).")
A2 <- TABa[TABa[,1]>=9 & TABa[,1]<=12,]
A2

writeLines("Sostituire la terza riga di TABa con (10, 10, 1, f, 4). Produrre la sostituzione dell’intero")
TABa[3,] <- c(10,10,1,'f',4)
TABa

rm(list = setdiff(ls(), lsf.str()))


#es 3

writeLines("\n\n\nEsercizio 3\n\n\n")
TABa <- default()
TABa

writeLines("\nSi calcoli la media della variabile X1 limitatamente a quei valori (osservazioni) di X1 associati
al valore 1 di X3\n")

mean(TABa[TABa[,3] == 1,1])

writeLines("\nCalcolare la somma tra\n\t
• la deviazione standard di X1 limitatamente a quei valori (osservazioni) di X1 associati
alla condizione 1 <= X5 <= 2\n\t")

sd(TABa[TABa[,5] >= 1 & TABa[,5] <= 2,1])



writeLines("\n\t• la deviazione standard di X2 limitatamente a quei valori (osservazioni) di X2 associati
alla condizione X3 + X5 >= 4\n")

sd(TABa[TABa[,3] + TABa[,5] >= 4,2])


writeLines("\n Si calcoli la somma dei valori di X2 limitatamente a quei valori (osservazioni) di X2 associati
in modo esclusivo o al valore m di X4 o al valore 1 di X3. Nota: per modo esclusivo si intende
che l’associazione deve avvenire con l’uno o l’altro valore ma non con entrambi (cosiddetta
disgiunzione esclusiva).\n")

sum(TABa[xor(TABa[,4] == 'm',TABa[,3]==1),2])


writeLines("\nVisualizzare la sottotabella di TABa le cui righe sono associate ai valori di X1 strettamente
maggiori di 10 e che soddisfino contemporaneamente anche la condizione (vincolo) descritta
nell’esercizio precedente (esercizio 3)\n")

sum(TABa[xor(TABa[,4] == 'm',TABa[,3]==1) & TABa[,1] > 10,2])

writeLines("Assegnare ad una nuova variabile U la sottotabella risultante dalla selezione della prima ed
ultima colonna di TABa (cioè a dire da X1 e X5) le cui righe sono associate al valore f di
X4 oppure ai valori X2 ≥ 15.
)")

U <- TABa[ TABa[,4] == 'f' | TABa[,2] >= 15,c(1,5)]
U

