library(rio)
library(haven)
library(tidyverse) 
library(Hmisc) 
library(psych) 
library(lmtest)

# Aufgabe 1
kino <- read_sav("Kino.sav")
#a)
plot(kino$kino ~ kino$alter, xlim = c(0,80), ylim = c(0,15)) # Streudiagramm
abline(lm(kino$kino ~ kino$alter)) # Regressionsgerade
#b)
model1 <- lm(kino$kino ~ kino$alter);model1
res1 <- model1[2];res1 # Residuen der Regression
res1_ueberpruefung <- residuals(model1);res1_ueberpruefung
#c)
RSS1 <- deviance(model1);RSS1
TSS1 <- sum((kino$kino - mean(kino$kino))^2); TSS1
R2.1 <- 1 - RSS1/TSS1; R2.1 # Bestimmtheitsmass
#d)
model1 # 1.66409 + 0.05062 * x
prognose1 <- 1.66409 + 0.05062 * 30;prognose1 # Alter von 30 
prognose2 <- 1.66409 + 0.05062 * 45;prognose2 # Alter von 45


#Aufgabe 2

einkommen <- c(1100,2400,2100,2250,1700,1200,2000,1500,1200,1800,1750,1300,2300,2200,2100,2250,1370,2310,1200,1310) #unabhaengig
ausgaben <- c(960,2070,1780,2120,1250,890,1600,1400,1110,1540,1470,1180,1900,1830,1690,1610,1170,1750,960,1170) ##abhaengig

#a)
plot(ausgaben ~ einkommen, xlim = c(0,2500), ylim = c(0,2100)) # Streudiagramm
abline(lm(ausgaben ~ einkommen)) # Regressionsgerade
#b)
model2 <- lm(ausgaben ~ einkommen);model2
res2 <- model2[2];res2 # Residuen der Regression
res2_ueberpruefung <- residuals(model2);res2_ueberpruefung
#c)
RSS2 <- deviance(model2);RSS2
TSS2 <- sum((ausgaben - mean(ausgaben))^2); TSS2
R2.2 <- 1 - RSS2/TSS2; R2.2 # Bestimmtheitsmass
#d)
model2 # 98.7186 + 0.7775 * x 
prognose3 <- 98.7186 + 0.7775 * 1325;prognose3 # Einkommen von 1325 
prognose4 <- 98.7186 + 0.7775 * 1475;prognose4 # Einkommen von 1475
prognose5 <- 98.7186 + 0.7775 * 2005;prognose5 # Einkommen von 2005
