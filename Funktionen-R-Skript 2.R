# Laden einer CSV-Datei und Kodierung bestimmter Variablen als Faktor oder geordneter Faktor
load_factorized_data = function(file, factors = NULL, ordered_factors = NULL) {
    # Überprüfen, ob die Datei existiert
    if (!file.exists(file)) {
        stop("Die Datei existiert nicht: ", file)
    }

    # Daten laden
    data = read.csv(file, stringsAsFactors = FALSE)

    # Überprüfen, ob die Spalten in den Daten existieren und sie als Faktor kodieren
    if (!is.null(factors)) {
        for (var in factors) {
            if (var %in% colnames(data)) {
                data[[var]] = factor(data[[var]])
            } else {
                warning(paste("Fehlende Spalte:", var))
            }
        }
    }

    # Überprüfen, ob die Spalten in den Daten existieren und sie als geordnete Faktoren kodieren
    if (!is.null(ordered_factors)) {
        for (var in ordered_factors) {
            if (var %in% colnames(data)) {
                data[[var]] = factor(data[[var]], ordered = TRUE)
            } else {
                warning(paste("Fehlende Spalte:", var))
            }
        }
    }

    return(data)
}

######### Überprüfungen (Sanity Checks) #########

#  Überprüfen, ob eine Variable numerisch ist
is_numeric = function(x) {
    if (!is.numeric(x)) {
        stop("Die Eingabe muss eine numerische Variable sein.")
    }
}

# Überprüfen, ob eine Variable ein Faktor ist
is_factor = function(x) {
    if (!is.factor(x)) {
        stop("Die Eingabe muss ein Faktor sein.")
    }
}

#  Überprüfen, ob ein Faktor dichotom (binär) ist
is_dichotomous = function(x) {
    if (!is.factor(x)) {
        stop("Die Eingabe muss ein Faktor sein.")
    }
    # Entfernt NA-Werte, bevor die Anzahl der Levels überprüft wird
    if (length(unique(na.omit(x))) != 2) {
        stop("Der Faktor muss genau zwei nicht fehlende Stufen haben.")
    }
}

# Überprüfen, ob zwei Variablen die gleiche Länge haben
have_same_length = function(x, y) {
    if (length(x) != length(y)) {
        stop("Beide Variablen müssen die gleiche Länge haben.")
    }
}
