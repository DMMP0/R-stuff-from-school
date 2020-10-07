
mazzo= c(1:52)
set.seed(203818)
#funzione calcolo carte
calcoloCarte <- function(mazzo,ripetizioni,sovrapposizioni=0)
{
	num=0
	den=0
	for(i in 1:ripetizioni)
	{
		y=sample(mazzo,52,FALSE)   #shuffle del secondo mazzo
		dif=table((x-y)==sovrapposizioni)       #trasformo in tabella con true e false
		if(dif[1]==52)			  #dif[1]=  FALSE
		{
			num = num+1
		}
		den = den+1
	}
	return (num/den)
}


set.seed(203818)

#mazzo1, non deve essere più mischiato
x= sample(mazzo,52,FALSE)  #false, così è senza svrapposizioni

stat1=calcoloCarte(x,100)    #a 0.37
stat1
stat1=calcoloCarte(x,1000)	 #0.379
stat1
stat1=calcoloCarte(x,10000)  # 0.3737
stat1
stat1=calcoloCarte(x,100000) #0.36628
stat1

#end



