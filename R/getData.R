
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
#' Cependant, lorsque l'argument `lag` vaut zéro, la fonction retourne la `date` inchangée. Aussi lorsque l'argument `lag` est négatif, la fonction se comporte comme la fonction `nextDate` et retourne les périodes futures et non passées.
#' @export
#'
#' @seealso `nextDate`
#'
#' @examples
#'
#' previousDate(c(2020L, 4L), frequency = 4L, lag = 2L)
#' previousDate(c(2021L, 1L), frequency = 4L, lag = -2L)
#'
#' previousDate(c(2020L, 4L), frequency = 12L, lag = 2L)
#' previousDate(c(2022L, 6L), frequency = 12L, lag = 12L)
previousDate <- function(date_ts, frequency, lag = 1L) {
    if (!ts4conj::is.date_ts(date_ts, frequency = frequency)) stop("La date est au mauvais format.")
    if (!is.numeric(frequency) || length(frequency) != 1L || !frequency %in% c(4L, 12L))
        stop("La fr\u00e9quence doit \u00eatre trimestrielle ou mensuelle.")
    if (!is.numeric(lag) || length(lag) != 1 || any(is.na(lag)) || lag != round(lag))
        stop("L'argument lag doit \u00eatre un entier (vecteur de longueur 1).")
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
#' Cependant, lorsque l'argument `lag` vaut zéro, la fonction retourne la `date` inchangée. Aussi lorsque l'argument `lag` est négatif, la fonction se comporte comme la fonction `previousDate` et retourne les périodes passées et non futures.
#' @export
#'
#' @seealso `previousDate`
#'
#' @examples
#'
#' nextDate(c(2020L, 4L), frequency = 4L, lag = 2L)
#' nextDate(c(2021L, 1L), frequency = 4L, lag = -2L)
#'
#' nextDate(c(2020L, 4L), frequency = 12L, lag = 2L)
#' nextDate(c(2022L, 6L), frequency = 12L, lag = 12L)
nextDate <- function(date_ts, frequency, lag = 1L) {
    if (length(date_ts) == 2L) {
        year <- date_ts[1L]
        month <- date_ts[2L]
        return(c(year + ((month - 1L + lag) %/% frequency),
                 1L + ((month - 1L + lag) %% frequency)))
    } else return(date_ts + lag / frequency)
}


firstDate <- function(dataTS) {
    timeTS <- dataTS |> zoo::na.trim() |> stats::time()
    firstTime <- timeTS[1L]
    return(c(firstTime %/% 1L, (firstTime %% 1L) * stats::frequency(dataTS) + 1L))
}

lastDate <- function(dataTS) {
    timeTS <- dataTS |> zoo::na.trim() |> stats::time()
    lastTime <- timeTS[length(timeTS)]
    return(c(lastTime %/% 1L, (lastTime %% 1L) * stats::frequency(dataTS) + 1L))
}

getValue_ts <- function(dataTS, date, nb = 1L) {
    return(dataTS |>
               stats::window(start = date, end = ts4conj::nextDate(date, frequency = 12L, lag = nb - 1L)) |>
               as.numeric())
}
