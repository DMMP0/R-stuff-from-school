# Title     : TODO
# Objective : TODO
# Created by: Maur
# Created on: 24/10/2020


#Es 1

#Sia X una variabile casuale continua distribuita secondo una funzione di densit`a
#normale N(x|µX, σX) dove µX = 100 e σX = 35. Si considerino i seguenti quattro eventi associati
#ad X:
# E1 = {x ∈ R : x ≤ 75},
# E2 = {x ∈ R : 50 ≤ x ≤ 70},
# E3 = {x ∈ R : x ≥ 110} e
#E4 = {x ∈ R : x ≥ 125}.
# Si calcolino le probabilità dei seguenti eventi combinati:


P1 <- pnorm(75,100,35)
P2 <- pnorm(70,100,35) - pnorm(50,100,35)
P3 <- 1 - pnorm(110,100,35)
P4 <- 1 - pnorm(125,100,35)

#1. P[E1 ∪ E3].

PR1 <- P1 + P3

#2. P[E1 ∪ E4].

PR2 <- P1 + P4
#3. P[E1 ∩ E2]. = E2

PR3 <- P2
#4. P[E1 ∩ E3]. insieme vuoto

PR4 <- 0


