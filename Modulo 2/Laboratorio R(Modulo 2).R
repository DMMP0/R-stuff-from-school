# Title     : TODO
# Objective : TODO
# Created by: DMMP0
# Created on: 09/10/2020

debug <- TRUE
wd <- getwd()
file <- paste(wd,"Modulo 2/DATI.csv",sep = "/")
DATI <- read.csv(file ,sep = ",",fileEncoding = "UTF-16")

writeLines("Esercizio 2\n\nFare la somma tra la media dei RT del gruppo delle donne e la media dei TP sempre nel gruppo delle donne.\n")

if(debug)
{
  m2_1 <- mean(DATI[DATI[,1]=="f",2])
  m2_2 <- mean(DATI[DATI[,1]=="f",4])
  somma <- m2_1 + m2_2
  writeLines(sprintf("\nLa media dei RT è %f\nLa media dei TP è %f\n\nSomma:\t",m2_1,m2_2))
} else {
  somma <- mean(DATI[DATI[,1]=="f",2]) + mean(DATI[DATI[,1]=="f",4])
}
somma
writeLines("\n\n\nEserizio 3\n\nControllare (semplicemente visualizzando i rispettivi risultati) se la deviazione standard dei TP del gruppo delle donne appartenenti alla condizione sperimentale (CS) di tipo A sia superiore alla deviazione standard dei TP del gruppo dei maschi appartenenti alla condizione sperimentale (CS) di tipo B.")

if(debug)
{
  sd3_1 <- sd(DATI[DATI[,1] == "f" & DATI[,3] == "A",4])
  sd3_2 <- sd(DATI[DATI[,1] == "m" & DATI[,3] == "B",4])
  check <- sd3_1 == sd3_2
}else{
  check <- sd(DATI[DATI[,1] == "f" & DATI[,3] == "A",4]) == sd(DATI[DATI[,1] == "m" & DATI[,3] == "B",4])
}
if(!check)
{
  if(debug)
  {
    writeLines(sprintf("Sono diversi:\n\tLa SD dei donne del gruppo A è %f\n\tQuella degli uomini del gruppo B è %f",sd3_1,sd3_2))
  } else {
    print("Sono diversi")
  }
} else{
  print("Sono uguali")
}


writeLines("\n\n\nEsercizio 4\n\nCostruire due nuove strutture di dati DATIsub1 e DATIsub2 contenenti rispettivamente: DATIsub1 la porzione di DATI limitata esclusivamente alle variabili quantitative RT e TP; DATIsub2 la porzione di DATI limitata esclusivamente alle variabili qualitative o ordinali GEN, CS e SC. Cancellare dalla memoria del sistema la struttura di dati DATI.")

DATIsub1 <- data.frame(RT = DATI$RT,TP = DATI$TP)
DATIsub2 <- data.frame(GEN = DATI$GEN,CS = DATI$CS,SC = DATI$SC)

remove(DATI)

writeLines("\n\n\nEsercizio 5\n\nVisualizzare la tabella di frequenza relativa alla variabile GEN di DATIsub2. Ripetere la procedura questa volta visualizzando la percentuale di donne e quella degli uomini.")

#senza dplyr
w <- table(DATIsub2$GEN)
t <- as.data.frame(w)
names(t)[1] <- 'GEN'
t


#con dplyr
#library('plyr')
#t <- count(DATIsub2,'GEN')
#t

data.frame(GEN = t[,1],PROP = round(prop.table(as.array(t[,2])),2))

writeLines("\n\n\nEsercizio 6\n\nVisualizzare la tabella di frequenza relativa alla variabile SC di DATIsub2. Ripetere la procedura questa volta visualizzando la tabella con le frequenze relative dei tre livelli di scolarizzazione.")


w <- table(DATIsub2$SC)
t <- as.data.frame(w)
names(t)[1] <- 'SC'
t
data.frame(SC = t[,1],PROP = round(prop.table(as.array(t[,2])),2))

writeLines("\n\n\nEsercizio 7\n\nCostruire una tabella di contingenza TC di dimensione 2 × 3 tra la variabile GEN e SC. \nVisualizzare i valori percentuali di TC")

TC <- table(DATIsub2$GEN,DATIsub2$SC)
round(prop.table(TC),2)

writeLines("\n\n\nEsercizio 8\n\nA partire dai dati contenuti in DATIsub2 e DATIsub1 costruire una tabella di contingenza tra GEN e CS che sia limitata a soli quegli individui che abbiano ottenuto un punteggio di TP strettamente maggiore di 55.\n\n")

#per farlo immagino sia più semplice ricreare DATI
tmpDATI <-data.frame(DATIsub1,DATIsub2)
tmpDATI <- tmpDATI[tmpDATI[,2]>55,]
TC2 <- table(tmpDATI$GEN,tmpDATI$CS)
TC2
remove(tmpDATI)
remove(wd)
remove(debug)
