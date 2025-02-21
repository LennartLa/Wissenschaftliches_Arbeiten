#' Diese Datei enthält die Funktionen (i)-(iv), die aber jetzt die Helper-Funktionen
#' aus Funktionen-R-Skript_2.R beinhalten
#' 
#' Unter jeder Funktion wurden zu gewählten Variablen eine Analyse durchgeführt

library(ggplot2)

metr_desk <- function(data, colum, table_if = FALSE) {
  # Einlesen der Daten in die Funktion
  temp_1 <- data[[colum]]
  
  # Berechnung der Statistiken mit der Helper-Funktion
  statistik <- metr_statistiken(data, colum)
  
  # Falls table_if = TRUE, auch Häufigkeitstabelle speichern
  if (table_if) {
    tabelle <- table(temp_1)
    statistik <- c(list(Tabelle = tabelle), statistik)
  }
  if (table_if && is.factor(temp_1)) {
    tabelle <- table(temp_1)
    statistik <- c(list(Tabelle = tabelle), statistik)
  }
  
  
  # Daten ausgeben
  return(statistik)
}

metr_desk(data_1, "Age", table_if = FALSE)
# Der älteste Passagier war 80 Jahre alt, was darauf hinweist, dass auch ältere Menschen auf der 
# Titanic reisten, vermutlich in den höheren Klassen.
# Der jüngste Passagier war ein Neugeborenes (0,42 Jahre alt), was zeigt, dass auch Kleinkinder an Bord waren.
# Das Medianalter lag bei 30 Jahren, was darauf hindeutet, dass die Mehrheit der Passagiere eher jung war.

#------------------------------------------------------------------------------#

metr_desk(data_1, "Fare", table_if = FALSE)
# Günstigster Ticketpreis: 0 EUR
# => Höchstwahrscheinlich wurden neugeborene Babys kostenlos mitgenommen, da sie 
#    keinen eigenen Sitzplatz benötigten oder als Begleitpersonen mitreisten.
# 
# Teuerster Ticketpreis: 512,33 EUR
# => Dieser extrem hohe Preis deutet darauf hin, dass es sich um ein Luxusticket für die 
#    1. Klasse handelte, vermutlich mit exklusiven Annehmlichkeiten und einer großen Kabine.
# 
# Durchschnittlicher Ticketpreis: 32,20 EUR
# => Der durchschnittliche Preis zeigt, dass die meisten Passagiere moderat teure Tickets kauften, 
#    was auf eine Mischung aus 2. und 3. Klasse hinweist.
# => Die große Preisspanne von 0 bis 512 EUR verdeutlicht die starke soziale Schichtung auf der Titanic.





#ii

# Funktion zur Berechnung von deskriptiven Statistiken für kategoriale Variablen
analyse_kategorial <- function(data, var) {
  # Berechnung mit Helfer-Funktion
  statistik <- haeufigkeiten_berechnen(data, var)
  
  cat("\nAnalyse der Variable:", var, "\n")
  cat("\nHäufigkeiten:\n")
  print(statistik$Absolute_Häufigkeiten)
  
  cat("\nRelative Häufigkeiten:\n")
  print(statistik$Relative_Häufigkeiten)
  
  cat("\nModalwert:\n")
  print(statistik$Modalwert)
}

# Analyse für die Variablen "Survived", "Sex", "Embarked" und "Pclass"
analyse_kategorial(data_1, "Survived")
# Von insgesamt 888 Passagieren überlebten nur 342 Personen (38,38%), während 546 Passagiere (61,61%) starben.
# Dies zeigt, dass die Mehrheit der Passagiere den Untergang der Titanic nicht überlebte. 
# Die Überlebensrate lag somit unter 40%, was die hohe Sterblichkeit der Katastrophe verdeutlicht.

#------------------------------------------------------------------------------#

analyse_kategorial(data_1, "Sex")
# Auf der Titanic befanden sich insgesamt 891 Passagiere (in diesem Datenausschnitt). 
# Die Verteilung der Geschlechter zeigt ein deutliches Ungleichgewicht:
#   
# Männer: 577 (64.75%) – fast zwei Drittel der Passagiere waren männlich.
# Frauen: 314 (35.24%) – nur etwa ein Drittel der Passagiere waren weiblich.





#iii. Zusammenhang zwischen zwei kategorialen Variablen

# Funktion: Berechnung von deskriptiven bivariaten Statistiken
analyse_bivariat <- function(data, var1, var2) {
  # Automatische Konvertierung der Variablen zu factor, falls notwendig
  data[[var1]] <- as.factor(data[[var1]])
  data[[var2]] <- as.factor(data[[var2]])
  
  # Nutzung der Helfer-Funktion für die Kreuztabelle
  ergebnisse <- kreuztabelle_erstellen(data, var1, var2)
  
  # Ausgabe der Ergebnisse
  cat("\nAnalyse zwischen", var1, "und", var2, ":\n")
  cat("\nKreuztabelle:\n")
  print(ergebnisse$Kreuztabelle)
  
  cat("\nRelative Häufigkeiten:\n")
  print(ergebnisse$Relative_Häufigkeiten)
  
  cat("\nErgebnisse des Chi-Quadrat-Tests:\n")
  print(ergebnisse$Chi_Quadrat_Test)
}


# Analyse aller Kombinationen von "Survived", "Pclass", "Sex" und "Embarked"
analyse_bivariat(data_1, "Survived", "Sex")
# Die Überlebensrate auf der Titanic zeigt einen starken Zusammenhang zwischen Geschlecht und Überlebenschance. 
# Von insgesamt 342 Überlebenden waren 109 Männer und 233 Frauen. Das bedeutet, dass deutlich mehr Frauen als Männer überlebt haben.
# 
# Der Chi-Quadrat-Test bestätigt diesen Zusammenhang mit einem extrem niedrigen p-Wert (< 2.2e-16), 
# was darauf hindeutet, dass die Überlebenswahrscheinlichkeit stark vom Geschlecht beeinflusst wurde.
# 
# - Von den insgesamt 38.38% Überlebenden waren 26.15% Frauen.
# - Die "Frauen und Kinder zuerst"-Regel könnte hierbei eine zentrale Rolle gespielt haben.
# - Männer hatten eine wesentlich geringere Überlebenschance als Frauen.

#------------------------------------------------------------------------------#

analyse_bivariat(data_1, "Pclass", "Survived")
# Die Überlebensrate auf der Titanic war stark von der Passagierklasse (Pclass) abhängig. 
# Die 1. Klasse hatte die meisten Überlebenden, während die 3. Klasse die höchste Anzahl an Todesfällen verzeichnete. 
# Dies deutet darauf hin, dass Passagiere in der 1. Klasse bevorzugten Zugang zu Rettungsbooten hatten, 
# während viele Passagiere der 3. Klasse nicht rechtzeitig gerettet wurden.
# 
# Ein Chi-Quadrat-Test bestätigt diesen Zusammenhang statistisch:
# Der p-Wert ist extrem klein (< 0.05), was bedeutet, dass die Klassenzugehörigkeit einen signifikanten Einfluss 
# auf die Überlebenschancen hatte. Mit anderen Worten, es machte einen erheblichen Unterschied, 
# in welcher Klasse sich ein Passagier befand – Reisende der 1. Klasse hatten deutlich bessere Chancen 
# zu überleben als jene in der 3. Klasse.


analyse_bivariat_metrisch_dichotom(data_1, "Fare", "Survived")
ggplot(data_1, aes(x = factor(Survived), y = Fare, fill = factor(Survived))) +
  geom_boxplot() +
  labs(
    title = "Ticketpreisverteilung nach Überlebensstatus",
    x = "Überlebensstatus (0 = Nein, 1 = Ja)",
    y = "Ticketpreis (Fare)"
  ) +
  theme_minimal()
## Obiege Analyse untersucht ob ein Zusammenhang zwischen dem Ticketpreis
## und dem Ueberleben der Gaeste bestehen koennte. Dieser Zusammenang wuerde die vorherige Analyse
## von Passagierklasse und ueberlebt weiter untermauern. Ein hoeher Ticketpreis 
## deutet auf hoeheren Wohlstand des Passagiers hin. Die sich daraus ergebende
## Folgerung ist: Wohlhabende Passagiere wurden bei der Evakuierung bevorzugt. 
## Dabei ist davon auszugehen, dass ein hoeher durschnittlicher Ticketpreis in Verbindug
## mit Survived = "ja" diesen Zusammenhang bestaetigt.
## Das Ergebnis liefert: Die Gaeste, die nicht ueberlebten haben einen durchschnittlich
## geringeren Ticketpreis gezahlt: 22.12 Euro. Im Gegensatz dazu betraegt der 
## Durchschnitt bei den Uberlebenden 48.39 Euro. Somit liegt ein Zusammenhang 
## zwischen Ueberleben und dem Ticketpreis nah, sodass man schlussfolgern kann, dass 
## die wohlhabenden Passagiere bevorzug worden sind. 




#iv. Zusammengang zwischen einer metrischen und einer dichotomen Variablen 

metr_desk1 = function(data, colum){
  statistik <- metr_statistiken(data, colum)
  return(statistik)
}


# Funktion: Analyse von metrischen und dichotomen Variablen
analyse_bivariat_metrisch_dichotom <- function(data, metr_var, dichot_var) {
  data[[dichot_var]] <- as.factor(data[[dichot_var]])
  
  gruppe_1 <- data[data[[dichot_var]] == levels(data[[dichot_var]])[1], ]
  gruppe_2 <- data[data[[dichot_var]] == levels(data[[dichot_var]])[2], ]
  
  statistik <- list(
    Gruppe_1 = metr_statistiken(gruppe_1, metr_var),
    Gruppe_2 = metr_statistiken(gruppe_2, metr_var)
  )
  
  cat("\nStatistiken für", levels(data[[dichot_var]])[1], ":\n")
  print(statistik$Gruppe_1)
  
  cat("\nStatistiken für", levels(data[[dichot_var]])[2], ":\n")
  print(statistik$Gruppe_2)
}


# Beispiel: Analyse von "Age" (metrisch) und "Survived" (dichotom)
analyse_bivariat_metrisch_dichotom(data, "Age", "Survived")
# Die durchschnittliche Altersdifferenz zwischen Überlebenden (28,3 Jahre) und Nicht-Überlebenden (30,8 Jahre) beträgt etwa 2-3 Jahre. 
# Dieser Unterschied ist relativ gering und allein nicht aussagekräftig genug, um zu behaupten, 
# dass jüngere Passagiere grundsätzlich eine höhere Überlebenschance hatten.
# 
# Es könnte verschiedene Gründe geben, warum Überlebende im Schnitt etwas jünger waren:
# - Jüngere Passagiere könnten körperlich fitter gewesen sein.
# - Die Evakuierungsstrategie („Frauen und Kinder zuerst“) könnte jüngeren Menschen zugutekommen.
# - Obwohl das Alter keine ausschlaggebende Variable für das Überleben war, zeigt die Analyse 
#   dennoch einen leichten Trend: Überlebende waren im Durchschnitt etwas jünger.

#------------------------------------------------------------------------------#

analyse_bivariat_metrisch_dichotom(data, "SibSp", "Survived")
# Die durchschnittliche Anzahl an Überlebenden (0,47) und nicht Überlebenden (0,55) mit Familienangehörigen
# an Bord ist nahezu gleich. Dies deutet darauf hin, dass das Vorhandensein von Familienmitgliedern 
# keinen starken Einfluss auf die Überlebenschancen hatte und zwischen diesen beiden Variablen keine signifikante Korrelation besteht.

## Gibt es einen zusammenhang zwischen der Position der Kabine und der 
## Ueberlebenschance ? 

data$cabin_side_num <- ifelse(data$Cabin_Side == "Backbord", 1, 0)
## Auspraegung der Kabinenseite wird vorher in Numeric umgewandelt
## um weitere Analyse zu ermoeglichen (0 == Backbord und 1 == Steuerbord). 

analyse_bivariat_metrisch_dichotom(data, "Cabin_Side", "Survived")

ggplot(data, aes(x = Cabin_Side, fill = as.factor(Survived))) +
  geom_bar(position = "dodge") +  # Position "dodge" zeigt Balken nebeneinander
  labs(title = "Überlebensanzahl nach Kabinenseite", x = "Kabinenseite", 
       y = "Anzahl", fill = "Überlebt") +
  theme_minimal()

## Das erstellte Balkendiagramm zeigt auf der y-Achse die absolute Haeufigkeit
## der Passagiere. Auf der x-Achse befindet sich die Lage der Kabinen. 
## Ein Vergleich der Position mit der Auspraegung ueberlebt ja(1) und 
## nicht ueberlebt(0) kann aufschluss darueber geben ob welche Seite des Schiffs
## leichter zu evakuieren war oder auch von dem Unfall schwerer betroffen. 
## Das Diagramm liefert jedoch keine aussagekraeftige Antwort. Die fehlenden 
## Daten sind ueberwiegen hier zudem sind die Anteile der vorhanden Kabinenseiten 
## recht ausgeglichen. 

