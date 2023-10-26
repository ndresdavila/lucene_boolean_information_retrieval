library(psych)
library(rio)

#1. Veraenderungsraten des Kontostandes

kontostand<-c(1500,2000,1700,1500,1800,2000,2500)

veraenderungsrate<-c()

#Berechnung von Veraenderungsraten:
for(index in seq(length(kontostand))) {
  #1. Element weg lassen, weil es kein vorheriges Jahr hat
  if(index==1){next}
  #Veraenderungsrat := (aktuelles Jahr)/(vorheriges Jahr)
  veraenderungsrate[index] <- kontostand[index]/kontostand[index-1]
  #Ueberpruefung
  print(veraenderungsrate[index])
}

#Loeschung von NAs
veraenderungsrate<-veraenderungsrate[!is.na(veraenderungsrate)]
#Ueberpruefung
veraenderungsrate

#2. Geometrisches Mittel

#1.088867 mit R funktionen
exp(mean(log(veraenderungsrate)))
#1.088867 mit psych library
geometric.mean(veraenderungsrate, na.rm = TRUE)
#1.088867 manuell mit Formel berechnet
prod(veraenderungsrate)^(1/(length(kontostand)-1))

#3. Erstellen Sie eine geeignete Grafik.

jahren<-c(2010:2016)

#Grafik
plot(jahren,kontostand, xlab = "Jahre", ylab = "Kontostand", main = "Entwicklung des Kontostand")

#Bessere Visualisierung 
for(i in seq(length(jahren)-1)){
  segments(x0=jahren[i],y0=kontostand[i],x1=jahren[i+1],y1=kontostand[i+1],col = "blue")
}
