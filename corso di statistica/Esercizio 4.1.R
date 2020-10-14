#Si consideri la prova che consiste nel lancio di tre monete. Costruire la
#distribuzione di probabilità della variabile X = {numero totale delle croci} e
#calcolare la probabilità di avere almeno due croci.

set.seed(203818)

moneta <- c("T", "C")
lanci <- 100000
out <- sample(moneta, size=3*lanci, replace=TRUE)
out <- out=="C"
ris <- matrix(out, ncol=3)

ncr <- apply(ris, 1, sum)  #ris è la matrice, 1 per le righe, sum è la funzione
(frel <- table(ncr)/sum(table(ncr)))

#      0       1       2       3
#	0.12396 0.37515 0.37658 0.12431

#end

