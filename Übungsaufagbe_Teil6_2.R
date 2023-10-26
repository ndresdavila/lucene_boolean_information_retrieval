library(rio)
library(vcd)

#Aufgabe 2

a_ndf <- import("a_ndf_2010.rds")
#a)
#2 Nominalskala
ta <- table(a_ndf$konfession,a_ndf$famstand)
vcd::assocstats(ta) #Chi-Quadrat Koeffizient und Cramer's V (Masszahlen fuer Nominalskala) sind NaN

#b)
#2 Nominalskala
tb <- table(a_ndf$famstand,a_ndf$geschlecht)
vcd::assocstats(tb) #Chi-Quadrat Koeffizient und Cramer's V (Masszahlen fuer Nominalskala) sind NaN

#c)
#2 Nominalskala
tc <- table(a_ndf$geschlecht,a_ndf$schulabschl)
vcd::assocstats(tc) #Schwacher Zusammenhang: 0 < Cramer's V <= 0.3

#d)
#2 Ordinalskala
cor.test(c(a_ndf$lebenzufr),c(a_ndf$nettoeink.k), method = "spearman") #Schwacher Zusammenhang: rho := 0.151368
