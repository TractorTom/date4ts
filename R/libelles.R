
#' Libelé pour une date
#'
#' @description La fonction `libelles_one_date` créé le libelé pour une date à une fréquence donnée.modifie la ou les valeurs d'un objet ts à une date donnée.
#'
#' @param date_ts un vecteur numérique, de préférence integer au format AAAA, c(AAAA, MM) ou c(AAAA, TT)
#' @param frequency un entier qui vaut 4L (ou 4.) pour les séries trimestrielles et 12L (ou 12.) pour les séries mensuelles.
#'
#' @return En sortie, la fonction retourne une chaine de caractère qui correspond au libelés de la date `date`.
#'
#' @examples
#' ts4conj:::libelles_one_date(date_ts = c(2020L, 4L), frequency = 12L)
#' ts4conj:::libelles_one_date(date_ts = c(2020L, 4L), frequency = 4L)
#'
libelles_one_date <- function(date_ts, frequency) {

    coll <- checkmate::makeAssertCollection()

    # Check de la fréquence
    frequency <- assert_frequency(frequency, add = coll, .var.name = "frequency")
    # Check du format date_ts
    date_ts <- assert_date_ts(x = date_ts, frequency, add = coll, .var.name = "date_ts")

    checkmate::reportAssertions(coll)

    date <- date_ts2date(date_ts, frequency = frequency)

    year <- date_ts[1L]
    if (frequency == 4L) {
        return(paste(quarters(date), year))
    } else if (frequency == 12L) {
        return(paste(months(date, abbreviate = TRUE), year))
    }
}

#' Libelés pour une période
#'
#' @description La fonction `libelles` créé un vecteur de chaines de caractère contenant les libelés de toutes les dates sur une période
#'
#' @param date_ts un vecteur numérique, de préférence integer au format AAAA, c(AAAA, MM) ou c(AAAA, TT)
#' @param frequency un entier qui vaut 4L (ou 4.) pour les séries trimestrielles et 12L (ou 12.) pour les séries mensuelles.
#' @param n un entier
#'
#' @details Pour choisir la période, il faut spécifier une date de début de période et un nombre de valeur.
#'
#' @return En sortie, la fonction retourne un vecteur de char avec les libelés de la période.
#' @export
#'
#' @examples
#' libelles(date_ts = c(2019L, 10L), frequency = 12L, n = 9L)
#' libelles(date_ts = c(2019L, 4L), frequency = 4L, n = 3L)
#'
libelles <- function(date_ts, frequency, n = 1L) {

    coll <- checkmate::makeAssertCollection()

    # Check de la fréquence
    frequency <- assert_frequency(frequency, add = coll, .var.name = "frequency")
    # Check de l'argument n
    n <- assert_scalar_natural(n, add = coll, .var.name = "n")
    # Check du format date_ts
    date_ts <- assert_date_ts(x = date_ts, frequency, add = coll, .var.name = "date_ts")

    checkmate::reportAssertions(coll)

    decale_libele <- function(x) {
        date_temp <- next_date_ts(date_ts = date_ts, frequency = frequency, lag = x)
        return(libelles_one_date(date_temp, frequency = frequency))
    }

    return(sapply(seq_len(n) - 1L, FUN = decale_libele))
}
