#Datensatz einlesen
data_1 = read.csv2('Datensatz_neu.csv')
data_1$X = NULL

#2,i)
metr_desk = function(colum, table_if = FALSE){
  #einlesen der Daten in die Funktion
  temp_1 = data_1[[which(colnames(data_1) == colum)]]
  
  #Verarbeiten der Daten
  tabelle = table(temp_1)
  arith_Mittel = mean(temp_1, na.rm = TRUE)
  Median = median(temp_1, na.rm = TRUE)
  minimum = min(temp_1, na.rm = TRUE)
  maximum = max(temp_1, na.rm = TRUE)
  spannweite = range(temp_1, na.rm = TRUE)
  Quantile = quantile(temp_1, na.rm = TRUE)
  standartabweichung = sd(temp_1, na.rm = TRUE)
  varianz = var(temp_1)
  
  #Daten Speichern
  if(table_if){
  temp_df_1 = c(Tabelle = tabelle,Arithmetisches_Mittel = arith_Mittel,
                         Median = Median, Minimum = minimum, Maximum = maximum,
                         Spannweite = spannweite, Quantile = Quantile, 
                         standartabweichung = standartabweichung, 
                         Varianz = varianz)
  }else{
    temp_df_1 = c(Arithmetisches_Mittel = arith_Mittel,
                  Median = Median, Minimum = minimum, Maximum = maximum,
                  Spannweite = spannweite, Quantile = Quantile, 
                  standartabweichung = standartabweichung, 
                  Varianz = varianz)
  }
  #Daten ausgeben
  return(temp_df_1)
}

metr_desk("Survived", table_if = TRUE)
metr_desk("Age", table_if = FALSE)
metr_desk("SibSp", table_if = FALSE)
metr_desk("Parch", table_if = FALSE)
metr_desk("Fare", table_if = FALSE)

#ii
# Funktion zur Berechnung von deskriptiven Statistiken für kategoriale Variablen

data_1 <- read.csv("titanic.csv")

# Funktion: Berechnung von deskriptiven Statistiken für eine kategoriale Variable
analyse_kategorial <- function(data, var) {
  # Automatische Konvertierung der Variablen zu factor, falls notwendig
  data[[var]] <- as.factor(data[[var]])
  
  haeufigkeiten <- table(data[[var]])
  relative_haeufigkeiten <- prop.table(haeufigkeiten)
  modalwert <- names(which.max(haeufigkeiten))
  
  cat("\nAnalyse der Variable:", var, "\n")
  cat("\nHäufigkeiten:\n")
  print(haeufigkeiten)
  
  cat("\nRelative Häufigkeiten:\n")
  print(relative_haeufigkeiten)
  
  cat("\nModalwert:\n")
  print(modalwert)
}

# Analyse für die Variablen "Survived", "Sex", "Embarked" und "Pclass"
analyse_kategorial(data_1, "Survived")
analyse_kategorial(data_1, "Sex")
analyse_kategorial(data_1, "Embarked")
analyse_kategorial(data_1, "Pclass")


#iii. Zusammenhang zwischen zwei kategorialen Variablen
data_1 <- read.csv("titanic.csv")

# Funktion: Berechnung von deskriptiven bivariaten Statistiken
analyse_bivariat <- function(data, var1, var2) {
  # Automatische Konvertierung der Variablen zu factor, falls notwendig
  data[[var1]] <- as.factor(data[[var1]])
  data[[var2]] <- as.factor(data[[var2]])
  
  kreuztabelle <- table(data[[var1]], data[[var2]])
  relative_haeufigkeiten <- prop.table(kreuztabelle)
  chi_quadrat_test <- chisq.test(kreuztabelle)
  
  cat("\nAnalyse zwischen", var1, "und", var2, ":\n")
  cat("\nKreuztabelle:\n")
  print(kreuztabelle)
  
  cat("\nRelative Häufigkeiten:\n")
  print(relative_haeufigkeiten)
  
  cat("\nErgebnisse des Chi-Quadrat-Tests:\n")
  print(chi_quadrat_test)
}

# Analyse aller Kombinationen von "Survived", "Pclass", "Sex" und "Embarked"
analyse_bivariat(data_1, "Survived", "Pclass")
analyse_bivariat(data_1, "Survived", "Sex")
analyse_bivariat(data_1, "Survived", "Embarked")
analyse_bivariat(data_1, "Pclass", "Sex")
analyse_bivariat(data_1, "Pclass", "Embarked")
analyse_bivariat(data_1, "Sex", "Embarked")

#iv. Zusammengang zwischen einer metrischen und einer dichotomen Variablen 

metr_desk <- function(temp_1) {
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

# Funktion: Analyse von metrischen und dichotomen Variablen
analyse_bivariat_metrisch_dichotom <- function(data, metr_var, dichot_var) {
  # Automatische Konvertierung der dichotomen Variable zu factor
  data[[dichot_var]] <- as.factor(data[[dichot_var]])
  
  # Daten splitten basierend auf der dichotomen Variable
  gruppe_1 <- data[[metr_var]][data[[dichot_var]] == levels(data[[dichot_var]])[1]]
  gruppe_2 <- data[[metr_var]][data[[dichot_var]] == levels(data[[dichot_var]])[2]]
  
  # Berechnung der deskriptiven Statistiken mit metr_desk
  statistik <- list(
    Gruppe_1 = metr_desk(gruppe_1),
    Gruppe_2 = metr_desk(gruppe_2)
  )
  
  # Ausgabe der Ergebnisse
  cat("\nAnalyse zwischen", metr_var, "und", dichot_var, ":\n")
  cat("\nStatistiken für", levels(data[[dichot_var]])[1], ":\n")
  print(statistik$Gruppe_1)
  
  cat("\nStatistiken für", levels(data[[dichot_var]])[2], ":\n")
  print(statistik$Gruppe_2)
}

# Beispiel: Analyse von "Age" (metrisch) und "Survived" (dichotom)
analyse_bivariat_metrisch_dichotom(data_1, "Age", "Survived")



#v
# Funktion, dass anhand 3 Variablen einen Mosaikplot erstellt. Durch die verwendung d. Biblothek "ggmosaic"
mosaic_plot <- function(data, var1, var2, var3) {
  library(ggmosaic) # Biblothek einladen
  #`geom_mosaic` wird verwendet, um ein Mosaikdiagramm zu erstellen, bei dem die Fläche
  # jedes Rechtecks die Häufigkeit oder das Gewicht einer Kombination von kategorialen Variablen darstellt.
  ggplot(data) +
  # x: erste kategoriale Variable entlang der x-Achse
  # fill: weist Farben zu, um die zweite kategoriale Variable darzustellen
  # cond: stratifiziert die Daten nach der dritten kategorialen Variable
    geom_mosaic(aes(weight = 1, x = product(!!sym(var1)), fill = !!sym(var2), cond = !!sym(var3))) +
    theme_minimal() + # 
    labs(title = "Mosaic Plot", x = var1, fill = var2)
} 

mosaic_plot(data_1, "Pclass", "Sex", "Embarked")
#vi
