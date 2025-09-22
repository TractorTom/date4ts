#' @title Libelé pour une date
#'
#' @description La fonction `libelles_one_date` créé le libellé pour une date
#' à une fréquence donnée.modifie la ou les valeurs d'un objet ts à une date
#' donnée.
#'
#' @inheritParams date_ts2timeunits
#' @param warn un booleen
#'
#' @returns En sortie, la fonction retourne une chaîne de caractère de longueur
#' 1 qui correspond au libellé de la date `date_ts`.
#'
#' @details
#' Si l'argument `warn` est `FALSE`, alors la fonction ne retournera pas de
#' warning lors de l'évaluation.
#'
#' @keywords internal
#' @noRd
#'
#' @examples
#'
#' libelles_one_date(2020L, 12L)
#' libelles_one_date(c(2020L, 3L), 4L)
#'
libelles_one_date <- function(date_ts, frequency_ts, warn = TRUE) {
    # Check de warn
    checkmate::assert_flag(
        warn,
        add = NULL,
        .var.name = "warn",
        na.ok = FALSE,
        null.ok = FALSE
    )

    # Check de la fréquence
    frequency_ts <- assert_frequency(
        frequency_ts,
        .var.name = "frequency_ts",
        warn = warn
    )

    # Check du format date_ts
    date_ts <- assert_date_ts(
        x = date_ts,
        frequency_ts,
        .var.name = "date_ts",
        warn = warn
    )

    date_obj <- date_ts2date(date_ts, frequency_ts = frequency_ts)

    year <- date_ts[1L]
    if (frequency_ts == 4L) {
        return(paste(quarters(date_obj), year))
    } else if (frequency_ts == 12L) {
        return(paste(months(date_obj, abbreviate = TRUE), year))
    }
}

#' @title Libelés pour une période
#'
#' @description La fonction `libelles` créé un vecteur de chaines de caractère
#' contenant les libelés de toutes les dates sur une période
#'
#' @inheritParams date_ts2timeunits
#' @param n un entier
#' @param warn un booleen
#'
#' @returns En sortie, la fonction retourne un vecteur de chaine de caractère de
#' longueur `n` avec les libellés de la période (de la date `date_ts` à la date
#' `date_ts + n périodes`.
#'
#' @details Pour choisir la période, il faut spécifier une date de début
#' `date_ts`, une fréquence `frequency_ts` pour le pas entre 2 dates
#' (trimestrielle ou mensuelle) et un nombre de valeur `n` (nombre de période).
#'
#' Si l'argument `warn` est `FALSE`, alors la fonction ne retournera pas de
#' warning lors de l'évaluation.
#'
#' @export
#'
#' @examples
#' libelles(date_ts = c(2019L, 10L), frequency_ts = 12L, n = 9L)
#' libelles(date_ts = c(2019L, 4L), frequency_ts = 4L, n = 3L)
#'
libelles <- function(date_ts, frequency_ts, n = 1L, warn = TRUE) {
    # Check de warn
    checkmate::assert_flag(
        warn,
        add = NULL,
        .var.name = "warn",
        na.ok = FALSE,
        null.ok = FALSE
    )

    # Check de la fréquence
    frequency_ts <- assert_frequency(
        frequency_ts,
        add = NULL,
        .var.name = "frequency_ts",
        warn = warn
    )

    # Check du format date_ts
    date_ts <- assert_date_ts(
        x = date_ts,
        frequency_ts,
        add = NULL,
        .var.name = "date_ts",
        warn = warn
    )

    # Check de l'argument n
    n <- assert_scalar_natural(n, add = NULL, .var.name = "n")

    decale_libele <- function(x) {
        date_temp <- next_date_ts(
            date_ts = date_ts,
            frequency_ts = frequency_ts,
            lag = x
        )
        return(libelles_one_date(
            date_ts = date_temp,
            frequency_ts = frequency_ts,
            warn = FALSE
        ))
    }

    return(vapply(
        X = seq_len(n) - 1L,
        FUN = decale_libele,
        FUN.VALUE = character(1)
    ))
}
