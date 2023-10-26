library(rio)
library(tidyverse)
library(psych)
library(moments)
library(pastecs)

allbus<-import("allbus2010_ndf.rds")

# Einekleine Funktion um sich die Schreibarbeit bei Haeufigkeitstabellen
create_freqtable <- function(x, round = FALSE) {
  AH <- table(x) # absolute Häufigkeiten
  RH <- prop.table(AH) # relative Häufigkeiten
  RHP <- 100 * RH # relative Häufigkeiten im Prozent
  KP <- cumsum(RHP) # kumulierte Prozente
  res <- cbind(AH, RH, RHP, KP)
  res_round <- round(res, digits = 2)
  if(round == FALSE) {
    return(res)
  } else {
    return(res_round)
  }
}

# Aufgabe 1.1

# Welcher Diagrammtyp eignet sich für die graphische Darstellung des Nettoeinkommens (neink)?
#Antwort: Boxplot
#Begründung: Viele numerische Beobachtungen, die wiederholt werden können

# Überprüfen ob es NAs in Variable neink.K gibt (189 NAs)
sum(is.na(allbus$neink))

# Löschung von NAs in Variable neink.K
allbus_ohneNA_netto<-allbus[!is.na(allbus$neink),]

# Überprüfen ob alle NAs in Variable neink.K gelöscht sind (soll 0 sein)
sum(is.na(allbus_ohneNA_netto$neink)) 

boxplot(allbus_ohneNA_netto$neink)

# Aufgabe 1.2

# Welcher Diagrammtyp eignet sich für die graphische Darstellung des kategorisierten Nettoeinkommens (neink.K)?
# Antwort: Barplot
# Begründung: die Variable ist in nicht-numerische Kategorien unterteilt (z.B: "UNTER 200 EURO","7500 EURO UND MEHR",...),
#             daher kann es sich nicht um ein Histogramm handeln, also ein Barplot passt.

# Überprüfen ob es NAs in Variable neink.K gibt (189 NAs)
sum(is.na(allbus$neink.K))

# Löschung von NAs in Variable neink.K
allbus_ohneNA<-allbus[!is.na(allbus$neink.K),]

# Überprüfen ob alle NAs in Variable neink.K gelöscht sind (soll 0 sein)
sum(is.na(allbus_ohneNA$neink.K)) 

# Frequency table für Variable neinnk.K
freq_table_allbus<-create_freqtable(allbus_ohneNA$neink.K,2);freq_table_allbus

# Achtung: Zeile "KEIN EINKOMMEN" Zeile in Häufigkeitstabelle soll gelöscht werden, da die NAs auch gelösch sind
freq_table_allbus<-freq_table_allbus[-c(1),];freq_table_allbus

# Überprüfen ob alle relative Häufigkeiten im Prozent 100 summieren 
sum(freq_table_allbus[,"RHP"]) 

# In Faktor umwandeln und levels bestimmen 
allbus_neink.K_RHP_faktor<-factor(freq_table_allbus[,"RHP"], levels = sort(unique(allbus_ohneNA$neink.K))) 

# Diagramm erstellen
par(mar = c(13.1, 4.1, 4.1, 2.1)) # Anpassung der Margen
barplot(freq_table_allbus[,"RHP"], las=2, main = "Kategorisierten Nettoeinkommens", ylab = "Relative Häufigkeit im Prozent")

# Aufgabe 2

# Variable Alter:
# Mittelwert
mean(allbus$alter, na.rm = TRUE)
# Median
median(allbus$alter, na.rm = TRUE) 
#Funktion fuer Berechnung des Modus / der Modi
modes <- function(x) {
  ux <- unique(x)
  tab <- tabulate(match(x, ux))
  ux[tab == max(tab)]
}
# Modus
allbus_ohneNA_alter<-allbus[!is.na(allbus$alter),]# Löschung von NAs
modes(allbus_ohneNA_alter$alter)
# Quartilen
quantile(allbus$alter, na.rm = TRUE)
# Geometrisches Mittel irrelevant: Beobachtungen darstellen kein Fluktuation/Veränderung
# Harmonisches Mittel irrelevant: Beobachtungen sind keine Quotienten

# Variable Geschlecht
modes(allbus$sex) #Modus
#Aritmetisches, geometrisches und harmonisches Mitte, Quartile, Perzentile, Quantile und Median können nicht definiert sein: nicht numerisch

# Variable kategorisiertes Alter
modes(allbus$alter.K) # Modus
#Aritmetisches, geometrisches und harmonisches Mitte, Quartile, Perzentile, Quantile und Median können nicht definiert sein: nicht numerisch

# Aufgabe 3
#Mittelwert für Nettoeinkommen
mean(allbus$neink, na.rm = TRUE) #1419.243
#Median für Nettoeinkommen
median(allbus$neink, na.rm = TRUE) #1150
#Diagramm
par(mar = c(5.1, 4.1, 4.1, 2.1)) # Anpassung der Margen
boxplot(allbus$neink)
#Warum unterscheiden sich beide Größen stark?
#Der Median ist im Diagramm deutlich zu erkennen,
# aber das arithmetische Mittel ist höher, da es viele (potenzielle) Ausreißer gibt.

# Aufgabe 4
allbus_männlich<-subset(allbus,subset = (allbus$sex == "männlich"))
mean(allbus_männlich$neink, na.rm = TRUE)#1805.784
allbus_weiblich<-subset(allbus,subset = (allbus$sex == "weiblich"))
mean(allbus_weiblich$neink, na.rm = TRUE)#1069.56
#Durschnittliche Nettoeinkommen bei männlichen Befragten (1805.784) ist größer als weiblichen Befragten (1069.56)

# Aufgabe 5

# Variable Alter
#Varianz
var(allbus$alter, na.rm = TRUE)#327.8497
#Standardabweichung
sd(allbus$alter, na.rm = TRUE)#18.10662
#Minimum
min(allbus$alter, na.rm = TRUE)#18
#Maximum
max(allbus$alter, na.rm = TRUE)#98
#Spannweite
max(allbus$alter, na.rm = TRUE)-min(allbus$alter, na.rm = TRUE)#80
#Interquartilabstand
IQR(allbus$alter, na.rm = TRUE)#30
#Standardfehler des Mittelwert
psych::describe(allbus$alter, na.rm = TRUE)[13]#0.58
#Variationskoeffizient
pastecs::stat.desc(allbus$alter)[14,]#0.3596725

# Variablen kategorisiertes Alter und Geschlecht
#Varianz,Standardabweichung, Min, Max, Sappweite, IQR, Standardfehler des Mittelwert
# und Variationskoeffizient können nicht deffiniert sein: beide sind nicht numerisch und sind Faktoren

# Aufgabe 6
#Standardabweichung
sd(allbus$neink, na.rm = TRUE)
#Min
min(allbus$neink, na.rm = TRUE)
#Max
max(allbus$neink, na.rm = TRUE)
#Spannweite
range(allbus$neink, na.rm = TRUE)
max(allbus$neink, na.rm = TRUE)-min(allbus$neink, na.rm = TRUE)
#Interquartilabstand
IQR(allbus$neink, na.rm = TRUE)
#Histogramm: (sieht wie eine Pareto-Verteilung an: die überwiegende Mehrheit der Befragten verdient ein Gehalt zwischen 0 und 2000 EUR, eine Minderheit verdient ein sehr hohes Gehalt.)
hist(allbus$neink, xlab = "Nettoeinkommen" )

# Aufgabe 7
# Löschung von NAs
allbus_weiblich_ohneNAs<-allbus_weiblich[!is.na(allbus_weiblich$neink),]
# Subset von weiblichen Befragten mit <3000 Nettoeinkommen
unter3000<-subset(allbus_weiblich_ohneNAs,subset = allbus_weiblich_ohneNAs$neink<3000)
#Prozent aller weiblichen Befragten (ohne NAs) mit Einkommen unter 3000 EUR 
round(nrow(unter3000)/nrow(allbus_weiblich_ohneNAs)*100,2)

# Aufgabe 8
#Schiefe: rechtsschiefe oder linkssteile Verteilung
moments::skewness(allbus$neink, na.rm = TRUE) #2.614159
#Excess: die Verteilung ist zu spitz
moments::kurtosis(allbus$neink, na.rm = TRUE) #14.21134
