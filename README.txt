In diesem Projekt wird ein Datensatz, der Informationen über die Passagiere auf der Titanic beinhaltet,
analysiert. 
Der zurverfügung gestellte Datensatz ("titanic.csv") enthält folgende Informationen:
- PassengerID: ID-Variable
- Survived: Hat den Untergang der Titanic überlebt? Ja (1), Nein (0)
- Pclass: Klasse des Reisenden (ordinal mit 1 > 2 > 3)
- Name: Name des Reisenden
- Sex: Geschlecht (male/female)
- Age: Alter in Jahren beim Untergang (Für Kleinkinder auch in Dezimalzahlen)
- SibSp: Anzahl an Geschwistern und Ehefrauen an Bord
- Parch: Anzahl an Eltern und Kinder an Bord
- Ticket: Ticketnummer
- Fare: Ticketpreis
- Cabin: Kabinennummer
- Embarked: Zustiegshafen (C = Cherbourg; Q = Queenstown; S = Southampton)

Mit "Datensatz-aufraeumen.R" wird der orginale "titanic.csv" Inhalt überarbeitet, sodass folgende 
Variablen hinzugefügt werden: 
- Anrede: "Mr.", "Mrs.","Miss","Master" 
- Cabine_Side: Steuerbord, Backbord, "NA" für fehlende Werte
- Deck: A-G, "NA" für fehlende Werte
Dieser überarbeitete Datensatz ist unter "Datensatz_neu.csv" aufzufinden. 
"Funktionen-R-Skript 1.R" sowie "Funktionen-R-Skript 2.R" liefert Helferfunktionen die beispielsweile 
prüfen ob eine Variable Numerisch ist oder als Factor umgewandelt wurde, da dies wichtig für die Analyse Funktionen ist.


