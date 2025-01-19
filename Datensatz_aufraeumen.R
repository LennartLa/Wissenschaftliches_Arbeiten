#Daten einlesen
data_1 = read.csv('titanic.csv')

#Anrede extrahieren

#Fehlen Namen? -> Nein
anyNA(data_1$Name)

i = 1
Anrede = 1

for (i in 1:891) {
  temp_1 = strsplit(data_1$Name[i], ", ")
  temp_1 = strsplit(temp_1[[1]][2], ". ")
  temp_1 = temp_1[[1]][1]
  Anrede[i] = temp_1
}

#Anrede als neue Variable zu dem Datensatz hinzufügen
data_1$Anrede = Anrede


#Codiert die Variablen „Survived“, „Sex“, „Embarked“ als factor um.

#Survived
data_1$Survived = factor(data_1$Survived)

#Sex
data_1$Sex = factor(data_1$Sex)

#Embarked (nach der Reihenfolge der Anläufe)
data_1$Embarked = factor(data_1$Embarked)


#Überführen der Variable „Pclass“ in einen ordered-factor
data_1$Pclass = factor(data_1$Pclass, ordered = TRUE, levels = c("1", "2", "3"))


#Imputiert fehlende Werte in der Variable „Age“ mithilfe der erzeugten Variable „Anrede“ 
i = 1
for (i in 1:891) {
  if(is.na(data_1$Age[i])){
    data_1$Age[i] = ceiling(mean(data_1$Age[data_1$Anrede == data_1$Anrede[i]], na.rm = TRUE))
  }
}

#Extrahiert aus der Variable „Cabin“
##Backbord oder Steuerbord? 
data_1$Cabin_Side = NA
data_1$Cabin_Side = ifelse(grepl("[13579]$", data_1$Cabin), "Steuerbord", 
                         ifelse(grepl("[02468]$", data_1$Cabin), "Backbord", NA))

##Deck
data_1$Deck = NA
data_1$Deck = ifelse(grepl("^[A-Z]", data_1$Cabin), substr(data_1$Cabin, 1, 1), NA)

##Einträge mit unbekannter Kabinennummer, d.h. „“ setzt ihr auf NA.
data_1$Deck[is.na(data_1$Cabin) | data_1$Cabin == ""] = NA
data_1$Cabin_Side[is.na(data_1$Cabin) | data_1$Cabin == ""] = NA

#Entfern Variablen
data_1 = subset(data_1, select = -c(PassengerId, Name, Ticket, Cabin))

#Speichern des neuen Datensatzes
write.csv2(data_1, file = "Datensatz_neu.csv")







