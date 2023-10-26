library(rio)
library(tidyverse)
library(moments)

#Aufgabe 1
europa<-import("Europa_2017.xls")
sum(is.na(europa))#0 NAs

#1 Standariesierung von Variable bip
europa$bip.z<-scale(europa$bip)

#2 Zentrierung von Variable energie
europa$energie.z<-scale(europa$energie, scale = FALSE)

#3 Ueberpruefung

#Standarisierte Variable hat einen Mittelwert von 0 (nicht genau 0 wegen Genauigkeit von Rechner)
round(mean(europa$bip.z,na.rm = TRUE),2)
#Standarisierte Variable hat eine Standardabweichung von 1
sd(europa$bip.z)
#Zentrierte Variable hat einen Mittelwert von 0 (nicht genau 0 wegen Genauigkeit von Rechner)
round(mean(europa$energie.z,na.rm = TRUE),2)

#Aufgabe 2
gewicht<-import("gewicht.rds")
sum(is.na(gewicht))#0 NAs 

#Zeigt eine gleichmäßige Verteilung der Werte , es soll (201-156)/3 = 15 sein
breaks<-round(((max(gewicht$gr)-min(gewicht$gr))/3),0);breaks

#Also:
#klein: ab 156cm bis 170cm
#mittelgross: ab 171cm bis 185cm
#gross: ab 186cm
gewicht$grK<-ifelse( (gewicht$gr<=170), "klein",
                     ifelse( (gewicht$gr<=185), "mittelgross", "gross"))


#Aufgabe 3
wahl<-import("Wahl.rds")
sum(is.na(wahl))#2 NAs
#Loeschung von der Zeile mit NA in Variable "alter" (Variable "partei" hat auch ein NA aber es ist irrelevant fuer die Aufgabe)
wahl_ohneNA<-wahl[!is.na(wahl$alter),]

#1a) 48.28% aller Befragten sind unter 40 Jahre alt
unter_40<-round(sum(wahl_ohneNA$alter<40)/nrow(wahl_ohneNA)*100,2);unter_40

#1b) 27.59% aller Befragten sind alter als 60 Jahre alt
alter_als_60<-round(sum(wahl_ohneNA$alter>60)/nrow(wahl_ohneNA)*100,2);alter_als_60

#2a) 50% aller maennnlichen Befragten sind unter 40 Jahre alt
unter_40_man<-round(sum( (wahl_ohneNA$alter<40) & (wahl_ohneNA$sex=="männlich") )/length(which(wahl_ohneNA$sex=="männlich"))*100,2);unter_40_man

#2b) 35.71% aller maennnlichen Befragten sind alter als 60 Jahre alt
alter_als_60_man<-round((sum( (wahl_ohneNA$alter>60) & (wahl_ohneNA$sex=="männlich") )/length(which(wahl_ohneNA$sex=="männlich")))*100,2);alter_als_60_man

#Aufgabe 4
gw<-import("GW.rds")
sum(is.na(gw))#0 NAs

#1
colnames(gw)[2]<-"Geschlecht"
colnames(gw)[3]<-"Alter"
colnames(gw)[4]<-"GemWoerter"

#2
#Mittelwert: 12
mean(gw$GemWoerter, na.rm = TRUE)

#Median: 12
median(gw$GemWoerter, na.rm = TRUE)

#Funktion fuer Berechnung des Modus/Moden
modes <- function(x) {
  ux <- unique(x)
  tab <- tabulate(match(x, ux))
  ux[tab == max(tab)]
}

#Modus: 11 und 12 mit Haeufigkeit von 9 (bimodal)
modes(gw$GemWoerter)

#Boxplot von GemWoerter
boxplot(gw$GemWoerter)

#Interpretation von Ergebnissen: die Streuung der Daten ist nicht zu groß (Standardabweichung ist ∼3), aber es gibt 1 potentieller Outlier (21)

sd(gw$GemWoerter) #3.487587
quantile(gw$GemWoerter, na.rm = TRUE) #1. Quartile (Q1): 10, 2. Quartile (Q2=Median): 12, 3. Quartile (Q3): 14
IQR(gw$GemWoerter, na.rm = TRUE)#Quartilabstand von Q1 und Q3: 4 (von 10 bis 14)
min(gw$GemWoerter, na.rm = TRUE)#Minimum: 5
max(gw$GemWoerter, na.rm = TRUE)#Maximum: 21 (Outlier: groesser als Q3+1.5*IQR)
range(gw$GemWoerter, na.rm = TRUE) #Wertebereich
diff(range(gw$GemWoerter, na.rm = TRUE))#Spannweite des Wertebereichs

#3
hist(gw$Alter)

#Schiefe
moments::skewness(gw$Alter)#Schiefe>0, also rechtsschiefe oder linkssteile Verteilung

#4
gw_4<-subset(gw, subset = ((gw$Alter>=20 & gw$Alter<=30) & (gw$Geschlecht=="männlich")))
mean(gw_4$GemWoerter, na.rm = TRUE) #15.1

#5
gw_5<-subset(gw, subset = ( (gw$Geschlecht=="männlich" & gw$Alter>50) | (gw$Geschlecht=="weiblich" & gw$Alter>60) ))
mean(gw_5$GemWoerter, na.rm = TRUE) #8.2

#6
#"5-Zahlen-Zusammenfassung" von Variable "Alter"
quantile(gw$Alter, na.rm = TRUE)
summary(gw$Alter, na.rm = TRUE)
#Welcher Diagrammtyp eignet sich hier für die graphische Darstellung? Boxplot
boxplot(gw$Alter)


#7
quantile(gw$GemWoerter,probs = c(.05,.33,.9)) #Anzahl von gemerkten Worten, die 5%, 33% und 90% der Befragten haben

#8
gw_weiblich<-subset(gw,subset = gw$Geschlecht=="weiblich") #Nur weibliche Befragten betrachten
quantile(gw_weiblich$GemWoerter,probs = c(.05,.33,.9)) #Anzahl von gemerkten Worten, die 5%, 33% und 90% der weiblichen Befragten haben

#9
#Fuer welche Variable(n) ist es sinnvoll Streuungsmaße zu berechnen? Alter und GemWoerter

#Fuenf unterschiedliche Streuungsmasse fuer Variable "Alter":

#1. Standardabweichung 14.56672
sd(gw$Alter,na.rm = TRUE)
#2. Varianz 212.1894
var(gw$Alter,na.rm = TRUE)
#3. Spannweite 56
max(gw$Alter,na.rm = TRUE) - min(gw$Alter,na.rm = TRUE)
#4. Kurtosis 2.882177
kurtosis(gw$Alter,na.rm = TRUE)
#5. Interquartilsabstand 20.75
IQR(gw$Alter,na.rm = TRUE)

#Interpretation: es gibt eine relativ grosse Streuung (Standardabweichung ist ∼14) der Werte vom Mittelwert weg (aber keine Outliers)
boxplot(gw$Alter) #Ueberpruefung
 