#' Formatter un objet date_ts
#'
#' @param date_ts un vecteur numérique, de préférence integer au format AAAA, c(AAAA, MM) ou c(AAAA, TT)
#' @param frequency un entier qui vaut 4L (ou 4.) pour les séries trimestrielles et 12L (ou 12.) pour les séries mensuelles.
#'
#' @return En sortie, la fonction retourne une date au format date_ts avec la période inclus entre 1 et la fréquence.
#'
#' @details Ici le formattage correspond à une réécriture de la date sans en changer la valeur. Alors que l'objet c(2020L, 12L) désigne le mois de décembre 2020 et c(2021L, 1L) le mois de janvier 2021, on peut imaginer que la date_ts c(2021L, 0L) peut aussi représenter le mois de décembre 2020.
#' @export
#'
#' @examples
#'
#' # Formattage inchangée
#' format_date_ts(c(2020L, 1L), frequency = 4L) # 1er trimestre de 2020
#' format_date_ts(c(2020L, 8L), frequency = 12L) # Aout 2020
#'
#' # Retour dans le passé
#' format_date_ts(c(2020L, 0L), frequency = 4L) # 4ème trimestre de 2019
#' format_date_ts(c(2020L, -10L), frequency = 12L) # février 2019
#'
#' # Avancée dans le futur
#' format_date_ts(c(2020L, 7L), frequency = 4L) # 3ème trimestre de 2021
#' format_date_ts(c(2020L, 13L), frequency = 4L) # janvier 2021
#'
format_date_ts <- function(date_ts, frequency, test = TRUE) {

    if (test) {
        coll <- checkmate::makeAssertCollection()

        # Check de la fréquence
        frequency <- assert_frequency(frequency, add = coll, .var.name = "frequency")
        # Check du format date_ts
        date_ts <- assert_date_ts(x = date_ts, frequency, add = coll, .var.name = "date_ts")

        checkmate::reportAssertions(coll)
    }

    if (length(date_ts) == 2L) {
        year <- date_ts[1L]
        period <- date_ts[2L]
        return(c(year + ((period - 1L) %/% frequency),
                 1L + ((period - 1L) %% frequency)))
    } else {
        return(c(date_ts, 1L))
    }
}
