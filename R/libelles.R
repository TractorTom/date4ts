
#' Libelé pour une date
#'
#' @description La fonction `libelles_one_date` créé le libelé pour une date à une fréquence donnée.modifie la ou les valeurs d'un objet ts à une date donnée.
#'
#' @param date un vecteur numérique, de préférence integer au format AAAA, c(AAAA, MM) ou c(AAAA, TT)
#' @param frequency un entier qui vaut 4L (ou 4.) pour les séries trimestrielles et 12L (ou 12.)
#'
#' @return En sortie, la fonction retourne une chaine de caractère qui correspond au libelés de la date `date`.
#'
#' @examples
#' libelles_one_date(date = c(2020L, 4L), frequency = 12L)
#' libelles_one_date(date = c(2020L, 4L), frequency = 4L)
libelles_one_date <- function(date, frequency){
    if (!ts4conj::isGoodDate(date)) stop("La date est au mauvais format.")
    if (date[1L] <= 0L) stop("La date doit \u00eatre après JC (ann\u00e9e positive).")
    if (!is.numeric(frequency) || length(frequency) != 1L || !frequency %in% c(4L, 12L))
        stop("La fr\u00e9quence doit \u00eatre trimestrielle ou mensuelle.")

    date <- date |> ts4conj::nextDate(frequency = frequency, lag = 0L)
    year <- date[1L]
    if (frequency == 4L){
        quarter <- date[2L]
        return(paste0("T", quarter, " ", year))
    } else if (frequency == 12L){
        month <- date[2L]
        return(paste(year, sprintf("%02.f", month), "01", sep = "-") |>
                   base::as.Date() |>
                   format(format = "%b %Y"))
    }
}

#' Libelés pour une période
#'
#' @description La fonction `libelles` créé un vecteur de chaines de caractère contenant les libelés de toutes les dates sur une période
#'
#' @param date un vecteur numérique, de préférence integer au format AAAA, c(AAAA, MM) ou c(AAAA, TT)
#' @param frequency un entier qui vaut 4L (ou 4.) pour les séries trimestrielles et 12L (ou 12.)
#' @param nb un entier
#'
#' @details Pour choisir la période, il faut spécifier une date de début de période et un nombre de valeur.
#'
#' @return En sortie, la fonction retourne un vecteur de char avec les libelés de la période.
#' @export
#'
#' @examples
#' libelles(date = c(2019L, 10L), frequency = 12L, nb = 9L)
#' libelles(date = c(2019L, 4L), frequency = 4L, nb = 3L)
libelles <- function(date, frequency, nb = 1){
    if (!ts4conj::isGoodDate(date)) stop("La date est au mauvais format.")
    if (!is.numeric(frequency) || length(frequency) != 1L || !frequency %in% c(4L, 12L))
        stop("La fr\u00e9quence doit \u00eatre trimestrielle ou mensuelle.")
    if (!is.numeric(nb) || length(nb) != 1 || any(is.na(nb)) || nb != round(nb))
        stop("L'argument nb doit \u00eatre un entier (vecteur de longueur 1).")
    if (is.double(nb)) warning("L'argument nb est de type double. Il faut privil\u00e9gier le format integer.")

    if (nb <= 0) stop("Aucun libell\u00e9 n'est sélectionn\u00e9.")

    return(sapply(0:(nb - 1), FUN = \(lag) (lag |>
                                                nextDate(date = date, frequency = frequency) |>
                                                ts4conj:::libelles_one_date(frequency = frequency))))
}
