#' Récupère des valeurs d'un ts
#'
#' @description La fonction `get_value_ts` permet de récupérer des valeurs.
#'
#' @param dataTS un objet ts unidimensionnel conforme aux règles de assert_ts
#' @param date_from un vecteur numérique, de préférence `integer` au format `AAAA`, `c(AAAA, MM)` ou `c(AAAA, TT)`
#' @param date_to un vecteur numérique, de préférence `integer` au format `AAAA`, `c(AAAA, MM)` ou `c(AAAA, TT)`
#' @param n un entier
#'
#' @return En sortie, la fonction retourne un vecteur (atomic) de même type que `dataTS` avec les valeurs extraites.
#' @export
#'
#' @examples
#'
#' ts1 <- ts(1:100, start = 2012, frequency = 12)
#' ts2 <- ts(letters, start = 2014, frequency = 4)
#' ts3 <- ts(exp(-(1:50)), start = 2015, frequency = 12)
#'
#' get_value_ts(ts1, date_from = c(2015L, 7L), date_to = c(2018L, 6L))
#' get_value_ts(ts2, date_from = c(2018L, 4L), n = 4L)
#' get_value_ts(ts3, date_to = c(2018L, 4L), n = 14L)
#'
get_value_ts <- function(dataTS, date_from, date_to, n) {
    # coll <- checkmate::makeAssertCollection()
    coll <- NULL

    # Check de l'objet dataTS
    assert_ts(dataTS, add = coll, .var.name = "dataTS")

    frequency_ts <- as.integer(stats::frequency(dataTS))

    if ((missing(date_to) + missing(date_from) + missing(n)) != 1L) {
        stop("Exactement 2 des arguments `date_from`, `date_to` et `n` doivent \u00eatre renseign\u00e9s et un manquant.")
    } else if (missing(date_from)) {
        # Check l'argument n
        n <- assert_scalar_natural(n, add = coll, .var.name = "n")
        # Check du format date_ts
        date_to <- assert_date_ts(
            x = date_to, frequency_ts = frequency_ts,
            .var.name = "date_from"
        )

        date_from <- previous_date_ts(
            date_ts = date_to,
            frequency_ts = frequency_ts, lag = n - 1L
        )
    } else if (missing(date_to)) {
        # Check l'argument n
        n <- assert_scalar_natural(n, add = coll, .var.name = "n")
        # Check du format date_ts
        date_from <- assert_date_ts(
            x = date_from, frequency_ts = frequency_ts,
            .var.name = "date_from"
        )

        date_to <- next_date_ts(
            date_ts = date_from,
            frequency_ts = frequency_ts, lag = n - 1L
        )
    } else if (missing(n)) {
        # Check du format date_ts
        date_from <- assert_date_ts(
            x = date_from, frequency_ts = frequency_ts,
            .var.name = "date_from"
        )
        # Check du format date_ts
        date_to <- assert_date_ts(
            x = date_to, frequency_ts = frequency_ts,
            .var.name = "date_from"
        )
        checkmate::assert_true(is_before(a = date_from, b = date_to, frequency_ts = frequency_ts, strict = FALSE))
    }

    # checkmate::reportAssertions(coll)

    output_value <- stats::window(
        x = dataTS,
        start = date_from,
        end = date_to, extend = TRUE
    )
    attributes(output_value) <- NULL

    return(output_value)
}
