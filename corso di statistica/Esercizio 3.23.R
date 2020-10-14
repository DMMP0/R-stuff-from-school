#Presso il check-in di un aeroporto sono in fila in attesa di imbarcarsi 40
#persone delle quali 15 in partenza per motivi di vacanza e 25 per motivi di lavoro. 
#Supponendo una distribuzione casuale degli individui all’interno della fila,
#determinare la probabilità che la seconda persona presente in essa stia recandosi
#in vacanza sotto le seguenti ipotesi:
#a) la prima persona in coda parte per motivi di lavoro;
#b) la prima persona in coda si reca in vacanza;
#c) non si hanno informazioni sulla prima persona della fila.

set.seed(203818)

#a
fila <- c(rep("VACANZA", 15), rep("LAVORO", 25))
prove <- 100000
num <- 0; den <- 0
for (i in 1:prove) 
{
	out <- sample(fila, 2, FALSE)
	if (out[1]=="LAVORO") 
	{
		den <- den + 1
		if (out[2]=="VACANZA") 
			num <- num+1
	}
}
cat(num,den,num/den,"\n")
#24119 62714 0.3845872


#b
num <- 0; den <- 0
for (i in 1:prove) 
{
	out <- sample(fila, 2, FALSE)
	if (out[1]=="VACANZA") 
	{
		den <- den + 1
		if (out[2]=="VACANZA") 
			num <- num+1
	}
}
cat(num,den,num/den,"\n")
#13466 37222 0.3617753



#c
num <- 0; den <- 0
for (i in 1:prove) 
{
	out <- sample(fila, 2, FALSE)
	den <- den + 1
	if (out[2]=="VACANZA") 
		num <- num+1
	
}
cat(num,den,num/den,"\n")
#37681 1e+05 0.37681





