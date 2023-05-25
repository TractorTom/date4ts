
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
#' @param date_ts un vecteur numérique, de préférence `integer`, au format c(AAAA, TT)
#'
#' @return En sortie, la fonction retourne la date au format c(AAAA, MM)
#' @export
#'
#' @examples
#' trim2mens(c(2019L, 4L)) #4ème trimestre 2019 --> Octobre 2019
#' trim2mens(c(2020L, 1L)) #1er trimestre 2020 --> Janvier 2020
trim2mens <- function(date_ts) {
    if (!ts4conj::isGoodDate(date_ts, frequency = 4L)) stop("La date est au mauvais format.")

    year <- date_ts[1L] + (date_ts[2L] - 1L) %/% 4L
    trim <- (date_ts[2L] - 1L) %% 4L + 1L
    return(c(year, trim * 3L - 2L) |> as.integer())
}

#' Correspondance d'une date mensuelle au trimestre courant
#'
#' @description La fonction `mens2trim` prend en argument une date au format c(AAAA, MM) et la convertit au format c(AAAA, TT) avec le trimestre en cours.
#'
#' @param date_ts un vecteur numérique, de préférence `integer`, au format c(AAAA, MM)
#'
#' @return En sortie, la fonction retourne la date au format c(AAAA, TT) correspondant au trimestre courant du mois `date_ts`.
#' @export
#'
#' @examples
#' mens2trim(c(2019L, 4L)) #Avril 2019 --> 2ème trimestre 2019
#' mens2trim(c(2020L, 11L)) #Novembre 2020 --> 4ème trimestre 2020
mens2trim <- function(date_ts) {
    if (!ts4conj::isGoodDate(date_ts, frequency = 12L)) stop("La date est au mauvais format.")

    year <- date_ts[1L] + (date_ts[2L] - 1L) %/% 12L
    month <- (date_ts[2L] - 1L) %% 12L + 1L
    return(c(year, 1L + ((month - 1L) %/% 3L)) |> as.integer())
}

#' Conversion d'une date du format date_ts au format TimeUnits
#'
#' @param date_ts un vecteur numérique, de préférence integer au format AAAA, c(AAAA, MM) ou c(AAAA, TT)
#' @param frequency un entier qui vaut 4L (ou 4.) pour les séries trimestrielles et 12L (ou 12.) pour les séries mensuelles.
#'
#' @return En sortie, la fonction retourne la date au format AAAA + TT/4 ou AAAA + MM/12.
#'
#' @details
#'  AAAA signifie que l'année est au format numérique avec 4 chiffres (Exemple : l'année deux mille vingt-deux s'écrit 2022 et non 22)
#'  MM signifie que le mois est au format numérique (Exemple : le mois de mai s'écrit 5, le moi de décembre s'écrit 12)
#'  TT signifie que le trimestre est au format numérique (Exemple : le troisième trimestre s'écrit 3)
#' @export
#'
#' @examples
#'
#' getTimeUnits(date_ts = c(2020L, 4L), frequency = 12L) # Avril 2020
#' getTimeUnits(date_ts = c(2022L, 11L), frequency = 12L) # Novembre 2020
#'
#' getTimeUnits(date_ts = c(2022, 4L), frequency = 4L) # 4ème trimestre de 2022
#' getTimeUnits(date_ts = c(1995L, 2L), frequency = 4L) # 2ème trimestre de 1995
#'
getTimeUnits <- function(date_ts, frequency) {
    if (!ts4conj::isGoodDate(date_ts, frequency = frequency)) stop("La date est au mauvais format.")
    if (date_ts[1L] <= 0L) stop("La date doit \u00eatre apr\u00e8s JC (ann\u00e9e positive).")
    if (!is.numeric(frequency) || length(frequency) != 1L || !frequency %in% c(4L, 12L))
        stop("La fr\u00e9quence doit \u00eatre trimestrielle ou mensuelle.")

    if (length(date_ts) == 2L) return(date_ts[1L] + (date_ts[2L] - 1) / frequency)
    return(date_ts[1L])
}

#' Conversion d'une date au format TS
#'
#' @param date un objet de type Date
#' @param frequency un entier qui vaut 4L (ou 4.) pour les séries trimestrielles et 12L (ou 12.) pour les séries mensuelles.
#'
#' @description La fonction `date2date_ts` prend en argument une date au format date (integer avec une class Date) et la convertit au format c(AAAA, MM) ou c(AAAA, TT) avec le mois ou trimestre en cours.
#' @return En sortie, la fonction retourne la date au format c(AAAA, MM) ou c(AAAA, TT) avec le mois ou trimestre en cours selon l'argument `frequency`.
#' @export
#'
#' @examples
#'
#' date2date_ts(as.Date("2000-01-01"))
#' date2date_ts(as.Date("2000-01-01"), frequency = 12L)
#'
#' date2date_ts(as.Date("2021-10-01"), frequency = 12L)
#' date2date_ts(as.Date("2021-10-01"), frequency = 4L)
#'
date2date_ts <- function(date, frequency = 12L) {

    if (!is.numeric(frequency) || length(frequency) != 1L || !frequency %in% c(4L, 12L))
        stop("La fr\u00e9quence doit \u00eatre trimestrielle ou mensuelle.")

    year <- as.numeric(format(date, format = "%Y"))
    month <- as.numeric(format(date, format = "%m"))

    if (frequency == 4L) {
        month <- 1L + ((month - 1L) %/% 3L)
    }

    return(c(year, month))
}

#' Conversion d'une date du format TS au format date
#'
#' @param date_ts un vecteur numérique, de préférence integer, au format AAAA, c(AAAA, MM) ou c(AAAA, TT)
#' @param frequency un entier qui vaut 4L (ou 4.) pour les séries trimestrielles et 12L (ou 12.) pour les séries mensuelles.
#'
#' @return En sortie, la fonction retourne la date au format Date.
#' @export
#'
#' @examples
#'
#' date_ts2date(date_ts = c(2020L, 4L))
#' date_ts2date(date_ts = c(2020L, 11L), frequency = 12L)
#' date_ts2date(date_ts = c(1995L, 2L), frequency = 4L)
#'
date_ts2date <- function(date_ts, frequency = 12L) {
    if (!ts4conj::isGoodDate(date_ts, frequency = frequency)) stop("La date est au mauvais format.")
    if (!is.numeric(frequency) || length(frequency) != 1L || !frequency %in% c(4L, 12L))
        stop("La fr\u00e9quence doit \u00eatre trimestrielle ou mensuelle.")

    year <- date_ts[1]
    month <- "01"

    if (length(date_ts) == 2) {
        if (frequency == 4L) {
            month <- sprintf("%02d", date_ts[2] * 3L - 2L)
        } else if (frequency == 12L) {
            month <- sprintf("%02d", date_ts[2])
        }
    }

    return(as.Date(paste(year, month, "01", sep = "-")))
}
