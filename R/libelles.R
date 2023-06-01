
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
#' libelles_one_date(date = c(2020L, 4L), frequency = 12L)
#' libelles_one_date(date = c(2020L, 4L), frequency = 4L)
libelles_one_date <- function(date_ts, frequency) {

    # Check de la fréquence
    if (!is_good_frequency(frequency)) {
        stop("La fr\u00e9quence doit \u00eatre trimestrielle ou mensuelle.")
    }

    # Check du format date_ts
    if (!is_date_ts(date_ts, frequency = frequency)) {
        stop("La date est au mauvais format.")
    }

    # Check d'une date après JC
    if (date_ts[1L] <= 0L) stop("La date doit \u00eatre apr\u00e8s JC (ann\u00e9e positive).")

    date_ts <- format_date_ts(date_ts, frequency = frequency)
    year <- date_ts[1L]
    if (frequency == 4L) {
        quarter <- date_ts[2L]
        return(paste0("T", quarter, " ", year))
    } else if (frequency == 12L) {
        month <- date_ts[2L]
        char_date <- paste(year, sprintf("%02.f", month), "01", sep = "-")
        return(format(base::as.Date(char_date), format = "%b %Y"))
    }
}

#' Libelés pour une période
#'
#' @description La fonction `libelles` créé un vecteur de chaines de caractère contenant les libelés de toutes les dates sur une période
#'
#' @param date_ts un vecteur numérique, de préférence integer au format AAAA, c(AAAA, MM) ou c(AAAA, TT)
#' @param frequency un entier qui vaut 4L (ou 4.) pour les séries trimestrielles et 12L (ou 12.) pour les séries mensuelles.
#' @param nb un entier
#'
#' @details Pour choisir la période, il faut spécifier une date de début de période et un nombre de valeur.
#'
#' @return En sortie, la fonction retourne un vecteur de char avec les libelés de la période.
#' @export
#'
#' @examples
#' libelles(date_ts = c(2019L, 10L), frequency = 12L, nb = 9L)
#' libelles(date_ts = c(2019L, 4L), frequency = 4L, nb = 3L)
libelles <- function(date_ts, frequency, nb = 1L) {

    # Check de la fréquence
    if (!is_good_frequency(frequency)) {
        stop("La fr\u00e9quence doit \u00eatre trimestrielle ou mensuelle.")
    }

    # Check du format date_ts
    if (!is_date_ts(date_ts, frequency)) {
        stop("La date est au mauvais format.")
    }

    # Check de l'argument nb
    if (is_single_integer(nb)) {
        stop("L'argument nb doit \u00eatre un entier (vecteur de longueur 1).")
    }

    if (is.double(nb)) {
        warning("L'argument nb est de type double. Il faut privil\u00e9gier le format integer.")
    }

    if (nb <= 0L) {
        stop("Aucun libell\u00e9 n'est s\u00e9lectionn\u00e9.")
    }

    decale_libele <- function(x) {
        date_temp <- next_date_ts(date_ts = date_ts, frequency = frequency, lag = x)
        return(libelles_one_date(date_temp, frequency = frequency))
    }

    return(sapply(seq_len(nb) - 1, FUN = decale_libele))
}
