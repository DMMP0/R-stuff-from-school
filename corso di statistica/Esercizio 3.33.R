#La probabilità che un individuo contragga una data malattia è pari a 0.03. 
#È disponibile un test medico per verificare l’effettiva presenza di detta malattia. 
#Se l’individuo è malato, la probabilità che il test fornisca un risultato posi-
#tivo (cioè l’individuo è effettivamente malato) è pari a 0.90. Se la malattia non è
#presente, la probabilità che il test fornisca un risultato positivo è di 0.02. 
#Si supponga che il test abbia fornito un risultato positivo. Quanto vale la probabilità
#che l’individuo sia effettivamente malato?

set.seed(203818)

#pbase = 0.03
#sens = 0.90
#spec = 0.02


persona <- c(rep("NM", 97), rep("M", 3))



prove <- 100000
num <- 0; den <- 0

for(i in 1:prove)
{
	out <- sample(persona, 1) #persona a caso
	if(out=="M" )#l'individuo è malato
		ris <- sample(c(1, 2), 1, prob=c(0.9, 0.3)) #1 = malato, 2 = non malto
	if(out == "NM")
		ris <- sample(c(1, 2), 1, prob=c(0.02, 0.98))
	if(ris == 1) #è stato dato per malato
		{
			den <- den + 1
			if(out == "M")
				num <- num + 1  # e lo era veramente
		}
	}


cat(num,den,num/den,"\n")


#0.54, why?