library(lubridate)
# Aufgabe 1

#1.
dt1 <- as.Date("2022-04-20")
#a)
format(dt1,"%Y/%B") # "2022/April"
format(dt1 %m+% years(1),"%Y/%B") # "2023/April"
#b)
paste("KW:",week("2022-04-20")) # "KW: 16"

#2.
ISOdate(2022, 2, 29) # NA, also kein Schaltjahr

#3.
as.numeric(format(dt1,"%d"))%%2 # 20 modulo 2 ist 0 also ja, 20 ist eine gerade Zahl

#4.
OlsonNames()[169] # "America/New_York"
OlsonNames()[86] # "America/Buenos_Aires"
now(OlsonNames()[169]) # Datum und Uhrzeit in New York
now(OlsonNames()[86]) # Datum und Uhrzeit in Buenos Aires

#5.
a <- "01 September 2020"
b <- "11-07-2021 07:20:30"
dt_a <- as.Date(a, format = "%d %B %Y");dt_a
dt_b <- as.Date(b, format = "%d-%m-%Y");dt_b
difftime(dt_b,dt_a, units = "weeks") # Ohne Rundung: 44.71429 Wochen
round(difftime(dt_b,dt_a, units = "weeks"),0) # Mit Rundung: 45 Wochen

#6
#a) Heute + taeglich bis in fünf Tagen in der Zukunft (6 Elemente insgesamt)
seq_a <- seq(Sys.Date(),Sys.Date()+5, "day");seq_a
#b) Heute + woechentlich bis in sechs Wochen in der Vergangenheit (7 Elemente insgesamt)
seq_b <- rev(seq(Sys.Date() %m-% weeks(6),Sys.Date(), "week"));seq_b
#c) Heute + jaehrlich bis in vier Jahre in der Zukunft (5 Elemente insgesamt)
seq_c <- seq(Sys.Date(),Sys.Date() %m+% years(4), "year");seq_c

#7
#Alle Tagen in 2023
tagen <- seq.Date(as.Date("2023-01-01"),as.Date("2023-12-31"),by="day")
#Alle Freitagen in 2023
freitags <- subset(tagen,subset= (weekdays(tagen)=="Friday"));freitags

counter<-0
for(i in seq(1:length(freitags))){ # Durch jeden Freitag iterieren
  
  if(as.numeric(format(freitags[i], "%d")) == 13){ # Ueberpruefen ob der Tag 13 ist
    counter <- counter+1 # Falls ja, Zaehler erhoehen
  }
}
print(counter) # 2 Freitagen insgesamt

#8
#1. Woche in May
woche1_may <- seq(as.Date("2023-05-01"),(as.Date("2023-05-01") %m+% weeks(1)-1),by="day");woche1_may

for(j in seq(1:length(woche1_may))){ # Durch alle Tagen der Woche iterieren
  if(weekdays(woche1_may[j]) == "Sunday"){ # Falls Tag "Sonntag" ist...
    print(woche1_may[j] %m+% weeks(1))     # ...1 Woche addieren
  }
}
# Muttertag: "2023-05-14"