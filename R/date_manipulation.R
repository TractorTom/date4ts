
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

    coll <- checkmate::makeAssertCollection()

    # Check de la fréquence
    frequency <- assert_frequency(frequency, add = coll, .var.name = "frequency")
    # Check du format date_ts
    date_ts <- assert_date_ts(x = date_ts, frequency, add = coll, .var.name = "date_ts")
    # Check l'argument lag
    lag <- assert_scalar_integer(lag, add = coll, .var.name = "lag")

    checkmate::reportAssertions(coll)

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

    coll <- checkmate::makeAssertCollection()

    # Check de la fréquence
    frequency <- assert_frequency(frequency, add = coll, .var.name = "frequency")
    # Check du format date_ts
    date_ts <- assert_date_ts(x = date_ts, frequency, add = coll, .var.name = "date_ts")
    # Check l'argument lag
    lag <- assert_scalar_integer(lag, add = coll, .var.name = "lag")

    checkmate::reportAssertions(coll)

    if (length(date_ts) == 2L) {
        year <- date_ts[1L]
        month <- date_ts[2L]
        return(c(year + ((month - 1L + lag) %/% frequency),
                 1L + ((month - 1L + lag) %% frequency)))
    } else return(date_ts + lag / frequency)
}

#' @export
firstDate <- function(dataTS) {

    # Check de l'objet dataTS
    assert_ts(dataTS, .var.name = "dataTS")

    timeTS <- stats::time(na_trim(dataTS))
    firstTime <- timeTS[1L]
    frequency <- as.integer(stats::frequency(dataTS))
    return(c(firstTime %/% 1L, (firstTime %% 1L) * frequency + 1L))
}

#' @export
lastDate <- function(dataTS) {

    # Check de l'objet dataTS
    assert_ts(dataTS, .var.name = "dataTS")

    timeTS <- stats::time(na_trim(dataTS))
    lastTime <- timeTS[length(timeTS)]
    frequency <- as.integer(stats::frequency(dataTS))
    return(c(lastTime %/% 1L, (lastTime %% 1L) * frequency + 1L))
}

#' @export
is_before <- function(a, b, frequency) {

    coll <- checkmate::makeAssertCollection()

    # Check de la fréquence
    frequency <- assert_frequency(frequency, add = coll, .var.name = "frequency")
    # Check du format date_ts a
    a <- assert_date_ts(x = a, frequency, add = coll, .var.name = "a")
    # Check du format date_ts b
    b <- assert_date_ts(x = b, frequency, add = coll, .var.name = "b")

    checkmate::reportAssertions(coll)

    tu_a <- date_ts2TimeUnits(a, frequency = frequency)
    tu_b <- date_ts2TimeUnits(b, frequency = frequency)
    return(tu_a <= tu_b)
}


#' Intervalle entre 2 dates
#'
#' @param a un objet date_ts, c'est-à-dire un vecteur numérique, de préférence integer au format AAAA, c(AAAA, MM) ou c(AAAA, TT)
#' @param b un objet date_ts, c'est-à-dire un vecteur numérique, de préférence integer au format AAAA, c(AAAA, MM) ou c(AAAA, TT)
#' @param frequency un entier qui vaut 4L (ou 4.) pour les séries trimestrielles et 12L (ou 12.) pour les séries mensuelles.
#'
#' @return En sortie, la fonction retourne un entier qui désigne le nombre de période (mois ou trimestres) qui sépare les 2 dates `a` et `b`.
#'
#' @details On travaille ici avec des dates au format date_ts, c'est-à-dire qui passe le test de la fonction `assert_date_ts`. Lorsqu'on parle d'intervalle et de nombre de période entre `a` et `b`, les bornes sont incluses. Ainsi `diff_periode(2020L, 2020L, 12L)` retourne bien 1L et non 2L ou 0L.
#'
#' @export
#'
#' @examples
#' # Une seule période
#' diff_periode(a = 2020L, b = 2020L, frequency = 4L)
#'
#' diff_periode(a = c(2000L, 1L), b = c(2020L, 4L), frequency = 4L)
#'
#' # Ordre chronologique respecté
#' diff_periode(a = c(2021L, 5L), b = c(2023L, 8L), frequency = 12L)
#'
#' # Date inversées
#' diff_periode(a = c(2023L, 8L), b = c(2021L, 5L), frequency = 12L)
#'
diff_periode <- function(a, b, frequency) {

    coll <- checkmate::makeAssertCollection()

    # Check de la fréquence
    frequency <- assert_frequency(frequency, add = coll, .var.name = "frequency")
    # Check du format date_ts a
    a <- assert_date_ts(x = a, frequency, add = coll, .var.name = "a")
    # Check du format date_ts b
    b <- assert_date_ts(x = b, frequency, add = coll, .var.name = "b")

    checkmate::reportAssertions(coll)

    if (!is_before(a, b, frequency)) {
        return(diff_periode(b, a, frequency))
    }
    if (length(a) == 1L) {
        a <- c(a, 1L)
    }
    if (length(b) == 1L) {
        b <- c(b, 1L)
    }
    return((b[1L] - a[1L]) * frequency + b[2L] - a[2L] + 1L)
}
