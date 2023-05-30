
getValue_ts <- function(dataTS, date, nb = 1L) {

    # Check de l'objet dataTS
    if  (!isGoodTS(dataTS, warn = FALSE)) {
        stop("L'objet `dataTS` doit \u00eatre un ts unidimensionnel de fr\u00e9quence mensuelle ou trimestrielle.")
    }

    # Check l'argument nb
    if (is_single_integer(nb)) {
        stop("L'argument nb doit \u00eatre un entier (vecteur de longueur 1).")
    }

    if (is.double(nb)) warning("L'argument nb est de type double. Il faut privil\u00e9gier le format integer.")

    return(dataTS |>
               stats::window(start = date, end = next_date_ts(date, frequency = 12L, lag = nb - 1L)) |>
               as.numeric())
}
