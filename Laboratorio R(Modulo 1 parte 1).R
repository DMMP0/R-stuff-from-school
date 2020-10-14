# Title     : TODO
# Objective : TODO
# Created by: DMMP0
# Created on: 25/09/2020

#Esercizio 1 Risolvere le seguenti espressioni:
writeLines("Esercizio 1\n")

#1
3.5+((17*3+4)/(1+1/5))

#2
15^(2*6) + 18/3

#3
15^(2*6)+18/(3^(1/2))

#4
(18-22.5)*sqrt(121+3/25)



#2 Assegnazione di variabili

#esercizio 2
writeLines("\nEsercizio 2\n")

X<-c(32.45,22.16,0.04,21.89,0.00)
Y<-c(3/5,1/32,2/9)
Z<-c("Sergio","Marina","Stefano","Antonio")

X
Y
Z


#Esercizio 3 Costruire le variabili a, b, c e d come di seguito illustrato
writeLines("\nEsercizio 3\n")



a <- (27*2) - (3+12)
b <- (7 * a) + a/3
c <- b^2 + a/b
#d <- sqrt(a+b-(c*d))
#d non è stata inizializzata prima, quindi dà errore

a
b
c
#d

remove(a,b,c)#,d)

#Esercizio 4 Costruire le seguenti variabili:
writeLines("\nEsercizio 4\n")

a <- 24
b <- a
B <- a + 2

a
b
B

remove(a,b,B)

#Esercizio 5 Assegnare le seguenti variabili. Dopo ciascuna assegnazione visualizzare direttamente
#il contenuto della variabile.

writeLines("\nEsercizio 5\n")


a <- "Carlo"
a
b <- "Giovanni"
b
c <- 7
c
c <- 8
c
c <- b
c

#quale osservazione potete fare? Rimuovere le variabili.

remove(a,b,c)


#3 Operazioni relazionali e logiche
#Esercizio 6 Date le variabili assegnate: a = TRUE, b = TRUE, c = FALSE, d = TRUE, verificare le seguenti espressioni logiche (∧, ∨ e ¬ denotano rispettivamente: congiunzione, disgiunzione
#e negazione logica):

writeLines("\nEsercizio 6\n")

a<-TRUE
b<-TRUE
c<-FALSE
d<-TRUE


#(1) ¬a ∨ a
!a | a
#(2) a ∨ (b ∧ c)
a|(b&c)
#(3) a ∧ (b ∨ c) ∧ ¬d
a & (b|c) & !d
#(4) ¬a ∨ (b ∧ c) ∧ (¬d ∨ c)
!a | (b&c) & (!d|c)
#(5) ¬(a ∨ c) ∧ ¬(¬(b ∧ c))
!(a|c) & !(!(b&c))
#rimuovere le variabili dalla memoria.
remove(a,b,c,d)

#Esercizio 7 Si assegni le variabili a = 3 e b = −3. Inoltre vengano assegnate alle variabili X, Y
#e Z i risultati dei seguenti confronti relazionali:

writeLines("\nEsercizio 7\n")
a <- 3
b <- -3
#(1) a > (b + 3)
X <- a>(b+3)
#(2) b ≥ (a − 6)
Y<- b>=(a-6)
#(3) a 6=
Z<- a != sqrt(b^2)

#Infine si verifichi la condizione
#X ∧ (Y ∨ Z).
X & (Y | Z)

#4 Vettori e funzioni in R
writeLines("\nEsercizio 8\n")
#Esercizio 8 In R le funzioni max(), min(), sum() indicano rispettivamente il valore minimo, il
#valore massimo, la somma di un vettore di numeri (x1, x2, . . . , xn). abs() `e la funzione valore
#assoluto che pu`o essere applicata ad un singolo valore o ad un vettore di valori. Inoltre le funzioni
#mean(), sd() e var() denotano rispettivamente: media, deviazione standard e varianza di un
#vettore numerico. Costruire i vettori numerici:
a <- c(1, 7, 8, 12, -12, 1)
b <- c(2/3, 4/5, -7/2, 12/3, -11/8, 1/4)
#Calcolare usando le funzioni sopra elencate le seguenti espressioni:

s1 <- sum(a) + sum(abs(b))

m1 <- min(a) - max(b)
m2 <- min(a) - max(abs(b))
m3 <- min(sum(a),sum(b))
n <- mean(a) + mean(b)
varA <- var(a)
varB <- var(b)

#Visualizzare tutte le variabili create.
a
b
s1
m1
m2
m3
n
varA
varB