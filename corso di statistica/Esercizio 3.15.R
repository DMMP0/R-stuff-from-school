set.seed(203818)


#Si lancia per tre volte una moneta non truccata. Calcolare la probabilità
#a) di ottenere tre volte testa;
#b) di non ottenere tre volte testa;
#c) di ottenere almeno una volte croce;
#d) di ottenere almeno una volta testa.

money = c("TESTA","CROCE")
volte = 10000
num = 0; den = 0

#a
for (i in 1:volte) 
{
	out = sample(money,3,TRUE)
	den = den + 1
	if (out[1]=="TESTA" & out[2]=="TESTA" & out[3]=="TESTA") 
		num = num+1
}
cat(num,den,num/den,"\n")
#1183 10000 0.1183


#b
num = 0; den = 0
for (i in 1:volte) 
{
	out = sample(money,3,TRUE)
	den = den + 1
	if (!(out[1]=="TESTA" & out[2]=="TESTA" & out[3]=="TESTA"))
		num = num+1
}
cat(num,den,num/den,"\n")
#8801 10000 0.8801 


#c
num = 0; den = 0
for (i in 1:volte) 
{
	out = sample(money,3,TRUE)
	den = den + 1
	if ((out[1]=="CROCE" | out[2]=="CROCE" | out[3]=="CROCE"))
		num = num+1
}
cat(num,den,num/den,"\n")
#8742 10000 0.8742



#d
num = 0; den = 0
for (i in 1:volte) 
{
	out = sample(money,3,TRUE)
	den = den + 1
	if ((out[1]=="TESTA" | out[2]=="TESTA" | out[3]=="TESTA"))
		num = num+1
}
cat(num,den,num/den,"\n")

#8721 10000 0.8721



#end