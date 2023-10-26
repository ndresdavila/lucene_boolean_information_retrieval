library(moments)
library(QuantPsyc)
library(vcd)
library(rio)
library(tidyverse)

#Aufgabe 1

daten1 <- c(-0.1845, -0.5819, 0.0806, -0.4913, -0.1643, 0.9504, -0.7489, -1.3823, 0.4783, -1.1963, 0.6096, -0.4479, 0.1517, -0.9726, 0.4836)

#Schiefe und Exzess = Woelbung-3
moments::skewness(daten1) # -0.01295026
moments::kurtosis(daten1)-3 # -0.9780772
hist(daten1) #entspricht in etwa der Form einer Normalverteilung, die Daten sind normalverteilt

#Aufgabe 2

koerpergroesse <- c(175,175,184,180,173,173,184,179,168,183)
schuhgroesse <- c(40,42,43,42,38,39,44,41,37,43)

#Streudiagramm
plot(koerpergroesse ~ schuhgroesse)
abline(lm(koerpergroesse ~ schuhgroesse)) # eine Anpassungslinie (Regressionsgerade) hinzufügen

# Masszahlen
cor.test(koerpergroesse,schuhgroesse,method = "kendall", exact = FALSE) # 0.8941795 
cor.test(koerpergroesse,schuhgroesse,method = "spearman", exact = FALSE) # 0.9538507 

#Aufgabe 3

mathematik <- c(1,2,3,2,1,2,3,1,3,2,3,2,1,2,3)
informatik <- c(1,3,2,1,2,3,1,3,2,3,2,1,2,3,3)

#Achtung! Overplotting: viele aehnliche und wiederholte Werte
plot(mathematik ~ informatik) 

#Alternative:
#Die runif-Funktion erzeugt für jeden Datenpunkt einen zufälligen Offset zwischen -0,1 und 0,1.
#Diese Offsets werden in der Plot-Funktion zu 'mathematik' und 'informatik' hinzugefügt, um Overplotting zu vermeiden.
jitter_x <- runif(length(mathematik), min = -0.1, max = 0.1)
jitter_y <- runif(length(informatik), min = -0.1, max = 0.1)
plot(mathematik + jitter_x, informatik + jitter_y)

cor.test(mathematik, informatik, method = "spearman", exact = FALSE) # -0.02828283

#Aufgabe 4
#a)
Staat <- c("A","B","C","D","E","F","G","H","I","J","K","L","M","N","O","P","Q","R","S","T")
IR <- c(1.9,3.2,4.5,5.8,2.3,3.5,5.1,2.2,3.4,4.7,2.7,3.8,4.2,2.1,3.5,5,4.1,3.3,2.4,3.1)
StSchulden <- c(53.1,55.7,74.4,76.6,54.7,62.8,71.2,52.4,64.5,76.4,55.2,64.3,76.2,50.0,68.6,72.4,67.3,61.2,57.2,61.7)
df <- data.frame(Staat,IR,StSchulden)

#b)
rownames(df) <- df$Staat # Variable Staat in die Zeilennamen des Data Frames konvertieren
df <- df[,-1] # df soll nur zwei Variablen enthalten
df # Ueberpruefung

#c)
cor.test(df$IR,df$StSchulden,method = "spearman", exact = FALSE) # 0.9462205

#d)
plot(df$IR ~ df$StSchulden)
abline(lm(df$IR ~ df$StSchulden)) # eine Anpassungslinie (Regressionsgerade) hinzufügen

#Aufgabe 5

wf2018 <- import("WorldFacts2018.xlsx")

# Auf 5 Variablen beschraenken
var1 <- wf2018$EG.ELC.ACCS.RU.ZS
var2 <- wf2018$NY.ADJ.NNTY.CD
var3 <- wf2018$SH.HIV.INCD
var4 <- wf2018$IS.AIR.PSGR
var5 <- wf2018$SE.PRM.UNER

aufgabe5 <- cbind(var1,var2,var3,var4,var5)
#a) Korrelationsmatrix
cor(aufgabe5, use = "complete.obs")
#Interpretierung von Korrelationskoeffizienten:
#Es besteht eine starke positive Korrelation (0.99577360) zwischen var2 (NY.ADJ.NNTY.CD) und var4 (IS.AIR.PSGR).
#Es besteht eine schwache negative Korrelation (-0.38426027) zwischen var1 (EG.ELC.ACCS.RU.ZS) und var5 (SE.PRM.UNER).

# b) Streudiagramm-Matrix
pairs(~ var1 + var2 + var3 + var4 + var5, data = aufgabe5)
      