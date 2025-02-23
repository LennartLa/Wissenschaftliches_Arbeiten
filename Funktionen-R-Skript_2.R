# (1)
# Die Kreuztabelle und ihre Berechnungen werden nicht mehr direkt in analyse_bivariat berechnet.
# Stattdessen ruft die Funktion nun 'kreuztabelle_erstellen(data, var1, var2)' auf und speichert die Ergebnisse in 'ergebnisse'.

kreuztabelle_erstellen <- function(data, var1, var2) {
  kreuztabelle <- table(data[[var1]], data[[var2]])
  list(
    Kreuztabelle = kreuztabelle,
    Relative_Haeufigkeiten = prop.table(kreuztabelle),
    Chi_Quadrat_Test = chisq.test(kreuztabelle)
  )
}


# (2)
# Helper-Funktion für die "analyse_kategorial" Funktion
# Anstatt die haeufigkeiten direkt in der Funktion zu berechnen, kann man diese Helper-Funktion
# benutzen um den Code einfacher und kürzer zu halten

haeufigkeiten_berechnen <- function(data, var) {
  var <- as.factor(data[[var]])
  haeufigkeiten <- table(var)
  list(
    Absolute_Haeufigkeiten = haeufigkeiten,
    Relative_Haeufigkeiten = prop.table(haeufigkeiten),
    Modalwert = names(which.max(haeufigkeiten))
  )
}


# (3)
# somit kürzen wir die 'metr_desk' und 'analyse_bivariat_metrisch_dichotom' funktionen, da in den beiden Funktionen,
# die gleichen metrischen Daten benötigt werden. Viel effizienter und sauberer.

metr_statistiken <- function(data, colum) {
  temp_1 <- data[[colum]]
  list(
    Mittelwert = mean(temp_1, na.rm = TRUE),
    Median = median(temp_1, na.rm = TRUE),
    Standardabweichung = sd(temp_1, na.rm = TRUE),
    Minimum = min(temp_1, na.rm = TRUE),
    Maximum = max(temp_1, na.rm = TRUE),
    Spannweite = range(temp_1, na.rm = TRUE),
    Varianz = var(temp_1, na.rm = TRUE)
  )
}
