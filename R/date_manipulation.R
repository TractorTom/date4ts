
#' Obtenir la date précédente
#'
#' @param date_ts un vecteur numérique, de préférence integer au format AAAA, c(AAAA, MM) ou c(AAAA, TT)
#' @param frequency un entier qui vaut 4L (ou 4.) pour les séries trimestrielles et 12L (ou 12.) pour les séries mensuelles.
#' @param lag un entier
#'
#' @return En sortie, la fonction retourne un vecteur d'entier qui représente la date à la période passée.
#'
#' @details Lorsqu'on parle de date précédente, on parle de date passée.
#' L'argument `lag` est entier et désigne le nombre de décalage que l'on affecte à notre date.
#' Par exemple pour des lag positif (1L, 2L, 10L) on désigne le décalage de la période précédente, celle d'avant et celle d'il y a 10 périodes.
#' Cependant, lorsque l'argument `lag` vaut zéro, la fonction retourne la `date` inchangée. Aussi lorsque l'argument `lag` est négatif, la fonction se comporte comme la fonction `next_date_ts` et retourne les périodes futures et non passées.
#' @export
#'
#' @seealso `next_date_ts`
#'
#' @examples
#'
#' previous_date_ts(c(2020L, 4L), frequency = 4L, lag = 2L)
#' previous_date_ts(c(2021L, 1L), frequency = 4L, lag = -2L)
#'
#' previous_date_ts(c(2020L, 4L), frequency = 12L, lag = 2L)
#' previous_date_ts(c(2022L, 6L), frequency = 12L, lag = 12L)
previous_date_ts <- function(date_ts, frequency, lag = 1L) {

    # Check de la fréquence
    if (!is_good_frequency(frequency)) {
        stop("La fr\u00e9quence doit \u00eatre trimestrielle ou mensuelle.")
    }

    # Check du format date_ts
    if (!is_date_ts(date_ts, frequency = frequency)) {
        stop("La date est au mauvais format.")
    }

    # Check l'argument lag
    if (is_single_integer(lag)) {
        stop("L'argument lag doit \u00eatre un entier (vecteur de longueur 1).")
    }

    if (is.double(lag)) warning("L'argument lag est de type double. Il faut privil\u00e9gier le format integer.")

    year <- date_ts[1L]
    month <- date_ts[2L]
    return(c(year + ((month - 1L - lag) %/% frequency),
             1L + ((month - 1L - lag) %% frequency)))
}

#' Obtenir la date suivante
#'
#' @param date_ts un vecteur numérique, de préférence integer au format AAAA, c(AAAA, MM) ou c(AAAA, TT)
#' @param frequency un entier qui vaut 4L (ou 4.) pour les séries trimestrielles et 12L (ou 12.) pour les séries mensuelles.
#' @param lag un entier
#'
#' @return En sortie, la fonction retourne un vecteur d'entier qui représente la date à la période future.
#'
#' @details Lorsqu'on parle de date suivante, on parle de date future.
#' L'argument `lag` est entier et désigne le nombre de décalage que l'on affecte à notre date.
#' Par exemple pour des lag positif (1L, 2L, 10L) on désigne le décalage de la période suivante, celle d'après et celle dans 10 périodes.
#' Cependant, lorsque l'argument `lag` vaut zéro, la fonction retourne la `date` inchangée. Aussi lorsque l'argument `lag` est négatif, la fonction se comporte comme la fonction `previous_date_ts` et retourne les périodes passées et non futures.
#' @export
#'
#' @seealso `previous_date_ts`
#'
#' @examples
#'
#' next_date_ts(c(2020L, 4L), frequency = 4L, lag = 2L)
#' next_date_ts(c(2021L, 1L), frequency = 4L, lag = -2L)
#'
#' next_date_ts(c(2020L, 4L), frequency = 12L, lag = 2L)
#' next_date_ts(c(2022L, 6L), frequency = 12L, lag = 12L)
next_date_ts <- function(date_ts, frequency, lag = 1L) {

    # Check de la fréquence
    if (!is_good_frequency(frequency)) {
        stop("La fr\u00e9quence doit \u00eatre trimestrielle ou mensuelle.")
    }

    # Check du format date_ts
    if (!is_date_ts(date_ts, frequency = frequency)) {
        stop("La date est au mauvais format.")
    }

    # Check l'argument lag
    if (is_single_integer(lag)) {
        stop("L'argument lag doit \u00eatre un entier (vecteur de longueur 1).")
    }

    if (is.double(lag)) {
        warning("L'argument lag est de type double. Il faut privil\u00e9gier le format integer.")
    }

    if (length(date_ts) == 2L) {
        year <- date_ts[1L]
        month <- date_ts[2L]
        return(c(year + ((month - 1L + lag) %/% frequency),
                 1L + ((month - 1L + lag) %% frequency)))
    } else return(date_ts + lag / frequency)
}


firstDate <- function(dataTS) {

    # Check de l'objet dataTS
    if  (!isGoodTS(dataTS, warn = FALSE)) {
        stop("L'objet `dataTS` doit \u00eatre un ts unidimensionnel de fr\u00e9quence mensuelle ou trimestrielle.")
    }

    timeTS <- stats::time(na_trim(dataTS))
    firstTime <- timeTS[1L]
    return(c(firstTime %/% 1L, (firstTime %% 1L) * stats::frequency(dataTS) + 1L))
}

lastDate <- function(dataTS) {

    # Check de l'objet dataTS
    if  (!isGoodTS(dataTS, warn = FALSE)) {
        stop("L'objet `dataTS` doit \u00eatre un ts unidimensionnel de fr\u00e9quence mensuelle ou trimestrielle.")
    }

    timeTS <- stats::time(na_trim(dataTS))
    lastTime <- timeTS[length(timeTS)]
    return(c(lastTime %/% 1L, (lastTime %% 1L) * stats::frequency(dataTS) + 1L))
}

is_before <- function(a, b, frequency) {

    # Check de la fréquence
    if (!is_good_frequency(frequency)) {
        stop("La fr\u00e9quence doit \u00eatre trimestrielle ou mensuelle.")
    }

    # Check du format date_ts
    if (!is_date_ts(a, frequency = frequency)) {
        stop("La date a est au mauvais format.")
    }

    # Check du format date_ts
    if (!is_date_ts(b, frequency = frequency)) {
        stop("La date b est au mauvais format.")
    }

    tu_a <- getTimeUnits(a, frequency = frequency)
    tu_b <- getTimeUnits(b, frequency = frequency)
    return(tu_a <= tu_b)
}

diff_periode <- function(a, b, frequency) {

    # Check de la fréquence
    if (!is_good_frequency(frequency)) {
        stop("La fr\u00e9quence doit \u00eatre trimestrielle ou mensuelle.")
    }

    # Check du format date_ts
    if (!is_date_ts(a, frequency = frequency)) {
        stop("La date a est au mauvais format.")
    }

    # Check du format date_ts
    if (!is_date_ts(b, frequency = frequency)) {
        stop("La date b est au mauvais format.")
    }

    if (!is_before(a, b, frequency)) return(diff_periode(b, a, frequency))
    if (length(a) == 1L) a <- c(a, 1L)
    if (length(b) == 1L) b <- c(b, 1L)
    return((b[1L] - a[1L]) * frequency + b[2L] - a[2L] + 1L)
}
