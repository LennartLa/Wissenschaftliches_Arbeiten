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

#iii

#iv

#v

#vi
