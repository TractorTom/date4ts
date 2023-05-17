
#' Conversion au format c(AAAA, TT)
#'
#' @description La fonction `as.YYYYTT` convertit une date en année en date au format c(AAAA, TT).
#'
#' @param timeUnits une date en année (Par exemple 2015.25 pour le 2ème trimestre 2015)
#'
#' @return En sortie, la fonction retourne la date au format (AAAA, TT)
#' @export
#'
#' @examples
#' as.YYYYTT(2019.75) #4ème trimestre 2019
#' as.YYYYTT(2020) #1er trimestre 2020
#' as.YYYYTT(2022 + 1/4) #2ème trimestre 2022
as.YYYYTT <- function(timeUnits) {
    if (!is.numeric(timeUnits) || length(timeUnits) != 1L || any(is.na(timeUnits)))
        stop("L'input timeUnits est au mauvais format.")
    temporalConsistence <- 4 * timeUnits
    if (!isTRUE(all.equal(temporalConsistence, round(temporalConsistence))))
        stop("L'input timeUnits n'est pas coh\u00e9rent temporellement avec les trimestres classiques.")

    return(c(timeUnits %/% 1, (timeUnits %% 1L) * 4 + 1) |> round() |> as.integer())
}

#' Conversion au format c(AAAA, MM)
#'
#' @description La fonction `as.YYYYMM` convertit une date en année en date au format c(AAAA, MM).
#'
#' @param timeUnits une date en année (Par exemple 2021.83333333333 pour Novembre 2021)
#'
#' @return En sortie, la fonction retourne la date au format c(AAAA, MM)
#' @export
#'
#' @examples
#' as.YYYYMM(2019.75) #Octobre 2019
#' as.YYYYMM(2020) #Janvier 2020
#' as.YYYYMM(2020 + 1/12) #Février 2020
#' as.YYYYMM(2020 + 12/12) #Janvier 2021
as.YYYYMM <- function(timeUnits) {
    if (!is.numeric(timeUnits) || length(timeUnits) != 1L || any(is.na(timeUnits)))
        stop("L'input timeUnits est au mauvais format.")
    temporalConsistence <- 12 * timeUnits
    if (!isTRUE(all.equal(temporalConsistence, round(temporalConsistence))))
        stop("L'input timeUnits n'est pas coh\u00e9rent temporellement avec les trimestres classiques.")

    return(c(timeUnits %/% 1, (timeUnits %% 1L) * 12 + 1) |> round() |> as.integer())
}

#' Correspondance d'une date trimestrielle au premier mois du trimestre
#'
#' @description La fonction `trim2mens` prend en argument une date au format c(AAAA, TT) et la convertit au format c(AAAA, MM) en choisissant le premier mois du trimestre.
#'
#' @param date un vecteur numérique, de préférence `integer` au format c(AAAA, TT)
#'
#' @return En sortie, la fonction retourne la date au format c(AAAA, MM)
#' @export
#'
#' @examples
#' trim2mens(c(2019L, 4L)) #4ème trimestre 2019 --> Octobre 2019
#' trim2mens(c(2020L, 1L)) #1er trimestre 2020 --> Janvier 2020
trim2mens <- function(date) {
    if (!ts4conj::isGoodDate(date)) stop("La date est au mauvais format.")

    year <- date[1L] + (date[2L] - 1L) %/% 4L
    trim <- (date[2L] - 1L) %% 4L + 1L
    return(c(year, trim * 3L - 2L) |> as.integer())
}

#' Correspondance d'une date mensuelle au trimestre courant
#'
#' @description La fonction `mens2trim` prend en argument une date au format c(AAAA, MM) et la convertit au format c(AAAA, TT) avec le trimestre en cours.
#'
#' @param date un vecteur numérique, de préférence `integer` au format c(AAAA, MM)
#'
#' @return En sortie, la fonction retourne la date au format c(AAAA, TT) correspondant au trimestre courant du mois `date`.
#' @export
#'
#' @examples
#' mens2trim(c(2019L, 4L)) #Avril 2019 --> 2ème trimestre 2019
#' mens2trim(c(2020L, 11L)) #Novembre 2020 --> 4ème trimestre 2020
mens2trim <- function(date) {
    if (!ts4conj::isGoodDate(date)) stop("La date est au mauvais format.")

    year <- date[1L] + (date[2L] - 1L) %/% 12L
    month <- (date[2L] - 1L) %% 12L + 1L
    return(c(year, 1L + ((month - 1L) %/% 3L)) |> as.integer())
}

#' Conversion d'une date TS au format TimeUnits
#'
#' @param date un vecteur numérique, de préférence integer au format AAAA, c(AAAA, MM) ou c(AAAA, TT)
#' @param frequency un entier qui vaut 4L (ou 4.) pour les séries trimestrielles et 12L (ou 12.)
#'
#' @return En sortie, la fonction retourne la date au format AAAA + TT/4 ou AAAA + MM/12.
#' @export
#'
#' @examples
getTimeUnits <- function(date, frequency) {
    if (!ts4conj::isGoodDate(date)) stop("La date est au mauvais format.")
    if (date[1L] <= 0L) stop("La date doit \u00eatre après JC (ann\u00e9e positive).")
    if (!is.numeric(frequency) || length(frequency) != 1L || !frequency %in% c(4L, 12L))
        stop("La fr\u00e9quence doit \u00eatre trimestrielle ou mensuelle.")

    if (length(date) == 2L) return(date[1L] + (date[2L] - 1) / frequency)
    return(date[1L])
}
