# Title     : TODO
# Objective : TODO
# Created by: DMMP0
# Created on: 09/10/2020

#rm(list = setdiff(ls(), lsf.str()))
set.seed(99)

z1 <- rnorm(150,100,50)
z2 <- rnorm(220,82,65)
boxplot(z1,z2,names=c("Z1","Z2"))


