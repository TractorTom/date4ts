
#' Récupère des valeurs d'un ts
#'
#' @description La fonction `getValue_ts` permet de récupérer des valeurs.
#'
#' @param dataTS un objet ts unidimensionnel conforme aux règles de assert_ts
#' @param date_ts un vecteur numérique, de préférence `integer` au format `AAAA`, `c(AAAA, MM)` ou `c(AAAA, TT)`
#' @param n un entier
#'
#' @return En sortie, la fonction retourne un vecteur (atomic) de même type que `dataTS` avec les valeurs extraites.
#' @export
#'
#' @examples
#'
#' ts1 <- ts(1:10, start = 2023, frequency = 12)
#' ts2 <- ts(letters, start = 2020, frequency = 4)
#'
#' getValue_ts(ts1, date_ts = c(2023L, 7L))
#' getValue_ts(ts2, date_ts = c(2021L, 4L), n = 4L)
#'
getValue_ts <- function(dataTS, date_ts, n = 1L) {

    # coll <- checkmate::makeAssertCollection()
    coll <- NULL

    # Check de l'objet dataTS
    assert_ts(dataTS, add = coll, .var.name = "dataTS")
    # Check l'argument n
    n <- assert_scalar_natural(n, add = coll, .var.name = "n")

    # checkmate::reportAssertions(coll)

    frequency_ts <- as.integer(stats::frequency(dataTS))

    # Check du format date_ts
    date_ts <- assert_date_ts(x = date_ts, frequency_ts = frequency_ts,
                              .var.name = "date_ts")

    output_value <- stats::window(
        x = dataTS,
        start = date_ts,
        end = next_date_ts(date_ts, frequency_ts = frequency_ts, lag = n - 1L))
    attributes(output_value) <- NULL

    return(output_value)
}
