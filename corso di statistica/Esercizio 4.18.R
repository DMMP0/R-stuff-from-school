#Il 5% dei CD prodotti da una certa ditta è difettoso. Supponendo di estrarre a caso e con reimmissione 8 CD dalla produzione, si calcoli la probabilità che i CD difettosi siano:
#1. uno;
#2. al massimo 2;
#3. nessuno.

set.seed(203818)
library(rmf)

#1
Binomiale(n=8,p=0.05,da=1,a=1,grafico=F)
#0.2793349

#2
Binomiale(n=8,p=0.05,da=0,a=2,grafico=F)
#0.9942118

#3
Binomiale(n=8,p=0.05,da=0,a=0,grafico=F)
#0.6634204




#end