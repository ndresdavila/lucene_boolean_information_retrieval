library(rio)

#Aufgabe 1
ideal<-import("Gewicht.rds")
ideal$normalgewicht<-ideal$gr-100;ideal$normalgewicht
ideal$idealgewicht<-ideal$normalgewicht*0.85;ideal$idealgewicht

#Ueberpruefung
head(ideal)
tail(ideal)

#Aufgabe 2

#2.1
wb<-import("Wohnungen_Berlin_2021.xlsx")

#2.2
names(wb)[4]<-"Groesse"

#2.3
wb$Stadtteil<-factor(wb$Stadtteil)
attributes(wb$Stadtteil)#Ueberpruefen ob Stadtteil ein Faktor ist

#2.4
AH_S <-table(wb$Stadtteil); AH_S   #Absolute Haeufigkeiten
RH_S <-prop.table(AH_S); RH_S      #Relative Haeufigkeiten
RHP_S <-100*RH_S; RHP_S            #Relative Haeufigkeiten in Prozent
KP_S <-cumsum(RHP_S); KP_S         #Kumulierte Haeufigkeiten

haeufigkeit_stadtteil.tabelle <-cbind(AH_S, RH_S, RHP_S, KP_S);
round(haeufigkeit_stadtteil.tabelle,2)#mit Rundung von 2 Nachkommastellen

#2.5

#Grenzen der Kategorien:
#sehr klein:  ab 13m^2   bis 35m^2
#klein:       ab 36m^2   bis 57m^2
#mittelgross: ab 58m^2   bis 79m^2
#gross:       ab 80m^2   bis 101m^2
#sehr gross:  ab 102m^2  bis 123m^2
#riesig:      ab 124m^2 

Groesse.breaks<-round(((146-13)/6),0);Groesse.breaks #Es soll 22 sein
wb$Groesse_Kat<-ifelse( (wb$Groesse <= 35), "sehr klein",
                     ifelse( (wb$Groesse <= 57), "klein",
                             ifelse((wb$Groesse <= 79) ,"mittelgross",
                                     ifelse( (wb$Groesse <= 101),"gross",
                                             ifelse( (wb$Groesse <= 123),"sehr gross","riesig")))))
#2.6

AH_G <-table(wb$Groesse); AH_G     #Absolute Haeufigkeiten
RH_G <-prop.table(AH_G); RH_G      #Relative Haeufigkeiten
RHP_G <-100*RH_G; RHP_G            #Relative Haeufigkeiten in Prozent
KP_G <-cumsum(RHP_G); KP_G         #Kumulierte Haeufigkeiten

haeufigkeit_groesse.tabelle <-cbind(AH_G, RH_G, RHP_G, KP_G);
round(haeufigkeit_groesse.tabelle,2)#mit Rundung von 2 Nachkommastellen

#Wie viel Prozenten aller Wohnungen haben die Fläche (Groesse) kleiner als 75m^2?
round(haeufigkeit_groesse.tabelle[max(which(haeufigkeit_groesse.tabelle[,"KP_G"]<75)),"KP_G"],2)#Es soll 73.57% aller Wohnungen sein

#2.7

#Stadtteil:
par(mar= c(5, 12, 4, 2))#Anpassung der Margen
barplot(AH_S, main = "Absolute Haeufigkeit von Stadtteilen", horiz = TRUE, las=1, xlab = "Anzahl von Wohnungen")

#Groesse:
par(mar = c(5.1, 4.1, 4.1, 2.1))#Anpassung der Margen
plot(wb$Groesse, main = "Groesse von Wohnungen in Berlin", xlab = "Index der Wohnungen", ylab = "Groesse in Quadratmeter")

#Groesse_Kat
par(mar = c(5.1, 4.1, 4.1, 4))#Anpassung der Margen
barplot(table(wb$Groesse_Kat), main = "Kategorien von Wohnungsgroesse", ylab = "Anzahl von Wohnungen", xlab = "Kategorien", xlim = c(0,6))

#2.8

#Wie viele Dreizimmerwohnungen sind in Neukoelln verfuegbar? 13
dreizimmerwohnungen_neukoelln<-subset(wb, subset = (Stadtteil=="Neukölln" & Zimmer==3));nrow(dreizimmerwohnungen_neukoelln)

#2.9

#Anzahl 4-Zimmer-Wohnungen in Mitte: 4
vierzimmerwohnungen_mitte<-subset(wb, subset = (Stadtteil=="Mitte" & Zimmer==4));nrow(vierzimmerwohnungen_mitte)

#Anzahl 4-Zimmer-Wohnungen in Pankow: 2
vierzimmerwohnungen_pankow<-subset(wb, subset = (Stadtteil=="Pankow" & Zimmer==4));nrow(vierzimmerwohnungen_pankow)

#Durschnittlicher Mietpreis von 4-Zimmer-Wohnungen in Mitte: 1618 Euro
summary(vierzimmerwohnungen_mitte[3])[4] 

#Durschnittlicher Mietpreis von 4-Zimmer-Wohnungen in Pankow: 1958 Euro
summary(vierzimmerwohnungen_pankow[3])[4]