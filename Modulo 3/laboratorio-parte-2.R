# Title     : TODO
# Objective : TODO
# Created by: Maur
# Created on: 22/10/2020

writeLines("\n\nEsercizio 1.1. Utilizzando il dataset di Anscombe studiare la relazione tra le seguenti coppie di variabili: (X1,Y1), (X1,Y2), (X1,Y3) e (X2,Y4)
utilizzando quattro differenti modelli di regressione MR1, MR2, MR3 e MR4.
Si rammenta che il caso studio di Anscombe è stato utilizzato nella letteratura statistica per mostrare l'importanza della rappresentazione grafica nello studio delle relazioni lineari
tra variabili quantitative.
Di conseguenza, si rappresenti graficamente in un grafico multiplo unico le relazioni tra le quattro coppie di variabili includendo anche la retta di regressione per ciascun grafico.\n\n")

par(mfrow = c(2,2))

X1 <- c(10,8,13,9,11,14,6,4,12,7,5)
Y1 <- c(8.04,6.95,7.58,8.81,8.33,9.96,7.24,4.26,10.84,4.82,5.68)
Y2 <- c(9.14,8.14,8.74,8.77,9.26,8.10,6.13,3.10,9.13,7.26,4.74)
Y3 <- c(7.46,6.77,12.74,7.11,7.81,8.84,6.08,5.39,8.15,6.42,5.73)
X2 <- c(8,8,8,8,8,8,8,19,8,8,8)
Y4 <- c(6.58,5.76,7.71,8.84,8.47,7.04,5.25,12.50,5.56,7.91,6.89)

D <- data.frame(X1,Y1,Y2,Y3,X2,Y4)

remove(X1,Y1,Y2,Y3,X2,Y4)

MR1 <- lm(formula = D$Y1~D$X1)
MR2 <- lm(formula = D$Y2~D$X1)
MR3 <- lm(formula = D$Y3~D$X1)
MR4 <- lm(formula = D$Y4~D$X2)

BarreResidui2(D$X1,D$Y1,nameX = "X1",nameY = "Y1")
BarreResidui2(D$X1,D$Y2,nameX = "X1",nameY = "Y2")
BarreResidui2(D$X1,D$Y3,nameX = "X1",nameY = "Y3")
BarreResidui2(D$X2,D$Y4,nameX = "X2",nameY = "Y4")



writeLines("\n\nEsercizio 1.2. Studia la relazione tra la variabile indipendente ed i residui per ciascun modello utilizzando dei grafici di dispersione.
Che tipo di commenti puoi fare circa ciascun grafico di dispersione?\n\n")

e1 <- residuals(MR1)
e2 <- residuals(MR2)
e3 <- residuals(MR3)
e4 <- residuals(MR4)
plot(D$X1,e1)
plot(D$X1,e2)
plot(D$X1,e3)
plot(D$X2,e4)


writeLines("\n\nEsercizio 1.3. Studia la relazione tra l'ordine di inserimento delle unità statistiche ed i residui per ciascun modello utilizzando dei grafici di dispersione.
Che tipo di commenti puoi fare circa ciascun grafico di dispersione?\n\n")

#par(mfrow = c(2,2))
ordine <- 1:11
plot(ordine,e1)
plot(ordine,e2)
plot(ordine,e3)
plot(ordine,e4)

writeLines("\n\nEsercizio 1.4. Calcola e visualizza la percentuale di varianza spiegata per ciascun modello.\n\n")

R2_1 <- cor(D$X1,D$Y1)^2
R2_2 <- cor(D$X1,D$Y2)^2
R2_3 <- cor(D$X1,D$Y3)^2
R2_4 <- cor(D$X2,D$Y4)^2

R2_1
R2_2
R2_3
R2_4

writeLines("\n\nEsercizio 1.5. Calcola i valori predetti dal primo modello di regressione per i nuovi valori -25, 0.25, 44.9, 100.8 della variabile indipendente X1.\n\n")

summary(MR1)
X1 <- c(-25, 0.25, 44.9, 100.8)

#B0 <- 3.0001
#B <- 0.5001
cof <- coefficients(MR1)
cof

Y1 <- cof[1] + X1 * cof[2]
Y1

#MR1$coef[1] + MR1$coef[2]*c(-25,0.25,44.9,100.8)

rm(list = setdiff(ls(), lsf.str()))

writeLines("\n\nEsercizio 2.1. Si consideri la seguente coppia di variabili categoriali A e B con rispettivamente 3 e 2 livelli:
A=(a1,a2,a2,a1,a3,a3,a3,a1,a2,a1,a2,a1,a3,a2,a2,a1,a3,a2,a1)
B=(b1,b1,b1,b1,b1,b1,b1,b1,b1,b2,b2,b2,b2,b2,b2,b2,b2,b2,b2)
Si costruisca la tabella di contingenza TC associata alle due variabili A e B.\n\n")

A<-as.factor(c('a1','a2','a2','a1','a3','a3','a3','a1','a2','a1','a2','a1','a3','a2','a2','a1','a3','a2','a1'))
B<-as.factor(c('b1','b1','b1','b1','b1','b1','b1','b1','b1','b2','b2','b2','b2','b2','b2','b2','b2','b2','b2'))
TC <- table(A,B)
TC




writeLines("\n\nEsercizio 2.2. Costruire la tabella di contingenza associata alle frequenze teoriche associate al modello di indipendenza tra A e B.\n\n")
mA <- margin.table(TC,1)
mB <- margin.table(TC,2)

n <- sum(TC)

TCI <- matrix(rep(0,6),3,2)
for (i in 1:3) {

   for (j in 1:2) {

       TCI[i,j] <- (mA[i]*mB[j])/n

   }

}

TCI
rownames(TCI) <- c("a1","a2","a3")
colnames(TCI) <- c("b1","b2")

writeLines("\n\nEsercizio 2.3. Calcola e visualizza il valore del Chi-quadrato osservato ottenuto sulla tabella di contingenza TC.\n\n")

summary(TC)
#0.4343

writeLines("\n\nEsercizio 2.4. Calcola l'indice normalizzato del Chi-quadrato osservato utilizzando la formula descritta a p. 85 del manuale.
Quali conclusioni puoi trarre dal risultato di tale indice?\n\n")

MaxChiQ <- n*min(dim(TC)[1]-1,dim(TC)[2]-1)
ChiQOss <- 0.4343
ChiQNorm <- ChiQOss/MaxChiQ
ChiQNorm

writeLines("\n\nEsercizio 2.5. Calcola il valore del Chi-quadrato osservato ottenuto sulla tabella di contingenza TC limitata ai soli valori a1 e a3 della variabile categoriale A.
Su questa nuova tabella di contingenza calcola l'indice normalizzato del Chi-quadrato.\n\n")

TC2 <- TC[c(1,3),]
TC2 <- as.table(TC2)
summary(TC2)
n <- sum(TC2)
ChiQOss <- 0.3429
MaxChiQ <- n*min(dim(TC2)[1]-1,dim(TC2)[2]-1)
ChiQNorm2 <- ChiQOss/MaxChiQ
ChiQNorm2 #0.028575


writeLines("\n\nEsercizio 2.6. Costruisci una nuova tabella di contingenza sommando i valori della seconda e terza riga di TC.
Questa nuova tabella di contingenza esprime l'associazione tra i valori b1 e b2 della variabile categoriale B e i valori a1 e {a2,a3} della nuova variabile categoriale A2.
 Per questa nuova tabella di contingenza
 1) deriva la tabella di contingenza associata alle frequenze teoriche del modello di indipendenza tra A2 e B
 2) visualizza il valore del Chi-quadrato osservato 3) calcola e visualizza il valore del Chi-quadrato normalizzato.\n\n")


A2 <- A
A2[A2=="a2" | A2=="a3"] <- "a23"
TC3 <- table(A2,B)
TC3

mA2 <- margin.table(TC3,1)
mB <- margin.table(TC3,2)
n <- sum(TC3)
TC3I <- matrix(rep(0,4),2,2)
for (i in 1:2) {

  for (j in 1:2) {

    TC3I[i,j] <- (mA[i]*mB[j])/n

  }

}
TC3I

summary(TC3)
MaxChiQ3 <- n*min(dim(TC3)[1]-1,dim(TC3)[2]-1)
ChiQOss3 <- 0.09048
ChiQNorm3 <- ChiQOss3/MaxChiQ3
ChiQNorm3 #0.01292571