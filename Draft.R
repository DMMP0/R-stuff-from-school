# Title     : TODO
# Objective : TODO
# Created by: DMMP0
# Created on: 09/10/2020

#source("Fractals.R")

prof <- c(0.25,0.30,0.28,0.28,0.28,0.97,0.22,0.18,0.21,0.22,0.19,0.21,0.17,0.24)

me <-   c(0.18,0.20,0.22,0.22,0.16,0.20,0.22,0.22,0.30,0.22,0.19,0.20,0.22,0.23)

par(mfrow=c(2,2))
hist(prof,xlim = c(0.00,1.00),ylim = c(0,10),breaks = 7)
hist(me,xlim = c(0.00,1.00),ylim = c(0,10),breaks = 7)
plot(density(prof),xlim = c(0.00,1.00),ylim = c(0,10))
plot(density(me),xlim = c(0.00,1.00),ylim = c(0,10))

boxplot(prof,me,names = c("Prof","Me"))

writeLines("Prof:\n\tMedia:")
mean(prof)
writeLines("\n\tMin:")
min(prof)
writeLines("\n\tMax:")
max(prof)
writeLines("\n\tDeviazione Stamdard:")
sd(prof)
writeLines("*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-")

writeLines("Me:\n\tMedia:")
mean(me)
writeLines("\n\tMin:")
min(me)
writeLines("\n\tMax:")
max(me)
writeLines("\n\tDeviazione Stamdard:")
sd(me)

#writeLines("*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-")
#rm(list = setdiff(ls(), lsf.str()))