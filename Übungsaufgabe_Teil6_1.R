library(rio)
library(vcd)
library(dplyr)
allbus<-import("allbus2010.sav")

#Um die Bedeutung der Werte in der Datei "allbus2010.sav" zu ermitteln,
#ging ich auf die Website "https://access.gesis.org/dbk/19379"
#und lud den Fragebogen der Version mit der Studiennummer ZA4611 herunter.

#Ich nehme an, dass die Reihenfolge "1,2,3,..." in der Datei allbus2010.sav im Fragebogen mit "A,B,C,..." übersetzt wird.

# Geschlecht (v298):
#1: Maennlich
allbus$v298[allbus$v298 == "1"] <- "Maennlich"
#2: Weiblich
allbus$v298[allbus$v298 == "2"] <- "Weiblich"

# Familienstand (V405):
#1: Verheiratet und leben mit Ihrem Ehepartner zusammen
allbus$v405[allbus$v405 == "1"] <- "Verheiratet zusammen"
#2: Verheiratet und leben getrennt
allbus$v405[allbus$v405 == "2"] <- "Verheiratet getrennt"
#3: Verwitwet
allbus$v405[allbus$v405 == "3"] <- "Verwitwet"
#4: Geschieden
allbus$v405[allbus$v405 == "4"] <- "Geschieden"
#5: Ledig
allbus$v405[allbus$v405 == "5"] <- "Ledig"

# Kofession (v730):
#1: Der römisch-katholischen Kirche
allbus$v730[allbus$v730 == "1"] <- "Roemisch-Katholisch"
#2: Der evangelischen Kirche (ohne Freikirchen)
allbus$v730[allbus$v730 == "2"] <- "Ev. ohne Freikirchen"
#3: Einer evangelischen Freikirche
allbus$v730[allbus$v730 == "3"] <- "Ev. mit Freikirchen"
#4: Einer anderen christlichen Religionsgemeinschaft
allbus$v730[allbus$v730 == "4"] <- "Andere (christl.)"
#5: Einer anderen nicht-christlichen Religionsgemeinschaft
allbus$v730[allbus$v730 == "5"] <- "Andere (nicht christ.)"
#6: KeinerReligionsgemeinschaft
allbus$v730[allbus$v730 == "6"] <- "Keine"

#Aufgabe 1.1

#a) Kreuztabelle mit den absoluten Haeufigkeiten zwischen den Variablen Konfession (v730) und Familienstand (v405)

ta <- addmargins(table(allbus$v730,allbus$v405));ta

barplot(table(allbus$v405,allbus$v730),
        xlab = "Arten von Konfessionen",
        col = c("blue","lightblue","lightgreen","green","yellow"),
        beside = TRUE)
legend(x = "topleft",
       legend = c("Geschieden","Ledig","Verheiratet getrennt","Verheiratet zusammen","Verwitwet"),
       fill = c("blue","lightblue","lightgreen","green","yellow") )

#b) Kreuztabelle mit den relativen Haeufigkeiten zwischen den Variablen Familienstand (v405) und Geschlecht (v298)

tb <- round(addmargins(prop.table(table(allbus$v405,allbus$v298))),2);tb
barplot(prop.table(table(allbus$v298,allbus$v405)),
        beside = TRUE,
        legend = TRUE,
        col = c("blue","red"),
        xlab = "Arten von Familientstaende")

#c)  Kreuztabelle mit den relativen Haeufigkeiten im Prozent zwischen den Variablen Allgemeiner Schulabschluss (v327)
#    und Geschlecht(v298)

tc <- round(addmargins(prop.table(table(allbus$v327,allbus$v298))*100),2);tc
barplot(prop.table(table(allbus$v298,allbus$v327))*100,
        beside = TRUE,
        col = c("blue","red"),
        xlab = "Arten von allgemeinen Schulabschluessen")
legend(x = "topright",
       legend = c("Maennlich", "Weiblich"),
       fill = c("blue","red"))

#d) Kreuztabelle mit den relativen Haeufigkeiten in Bezug zur Zeilen- bzw. Spaltensummen fuer die Variablen
#   Allgemeine Lebenszufriedenheit (v749) und Nettoeinkommen (v615)

#Allgemeine Lebenszufriedenheit (v749)
#10: Ganz und gar zufrieden
#...
#0: Ganz und gar unzufrieden

td_zeilen <- addmargins(prop.table(table(allbus$v749,allbus$v615), margin = 1));td_zeilen # Zeilensummen
td_spalten <- addmargins(prop.table(table(allbus$v749,allbus$v615), margin = 2));td_spalten # Spaltensummen

#Barplot ohne Legende fuer bessere Visualisierung des Graphes
barplot(prop.table(table(allbus$v749,allbus$v615), margin = 2),
        xlab = "Kategorien von Nettoeinkommen",
        legend = FALSE,
        col = c("#fafa6e","#c9ee73","#9cdf7c","#72cf85","#4abd8c","#23aa8f","#00968e","#008288","#106e7c","#225b6c","#2a4858"))

#Mit Legende: 10: Ganz und gar zufrieden , ... , 0: Ganz und gar unzufriedens
barplot(prop.table(table(allbus$v749,allbus$v615), margin = 2),
        xlab = "Kategorien von Nettoeinkommen",
        legend = TRUE,
        col = c("#fafa6e","#c9ee73","#9cdf7c","#72cf85","#4abd8c","#23aa8f","#00968e","#008288","#106e7c","#225b6c","#2a4858"))


# Ist einen Zusammenhang zwischen diesen Variablen erkennbar?
# Es besteht ein schwacher Zusammenhang (0 < Cramer's V <= 0.3)
vcd::assocstats(table(allbus$v749,allbus$v615))
vcd::assocstats(table(allbus$v615,allbus$v749))

#Aufgabe 1.2
#Wie viele Männer (Maennlich in v298) sind evangelisch ("Ev. ohne Freikirchen" und "Ev. mit Freikirchen" in v730)
# und verheiratet ("Verheiratet zusammen" und "Verheiratet getrennt" in v405)

#Tabelle mit nur maennliche Befragten (1)
allbus_maennlich <- subset(allbus, subset = allbus$v298 == "Maennlich")
#Kreuztabelle fuer Variablen Familienstand und Konfession
t12 <- addmargins(table(allbus_maennlich$v405,allbus_maennlich$v730));t12

#Antwort: Summe der Werte der jeweiligen Zeilen und Spalten
antwort <- (t12["Verheiratet getrennt","Ev. mit Freikirchen"]
          +t12["Verheiratet getrennt","Ev. ohne Freikirchen"]
          +t12["Verheiratet zusammen","Ev. mit Freikirchen"]
          +t12["Verheiratet zusammen","Ev. ohne Freikirchen"]); antwort #86
