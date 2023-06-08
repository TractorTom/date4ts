
#' Change certaines valeurs d'un ts
#'
#' @description La fonction `setValue_ts` modifie la ou les valeurs d'un objet ts à une date donnée.
#'
#' @param dataTS un objet ts unidimensionnel conforme aux règles de assert_ts
#' @param date_ts un vecteur numérique, de préférence integer au format AAAA, c(AAAA, MM) ou c(AAAA, TT)
#' @param x un vecteur de même type que le ts `dataTS`
#'
#' @return En sortie, la fonction retourne l'objet `dataTS` n objet ts modifié avec les valeurs de `x` imputés à partir de la date `date`.
#' @export
#'
#' @examples
#' setValue_ts(
#'     dataTS = ev_pib,
#'     date_ts = c(2021L, 2L),
#'     x = c(1, 2, 3)
#'     )
#'
setValue_ts <- function(dataTS, date_ts, x) {

    coll <- checkmate::makeAssertCollection()

    # Check de l'objet dataTS
    assert_ts(dataTS, add = coll, .var.name = "dataTS")

    frequency <- as.integer(stats::frequency(dataTS))

    # Check du format date_ts
    date_ts <- assert_date_ts(x = date_ts, frequency, add = coll, .var.name = "date_ts")
    # Check de l'objet x un vecteur atomic
    checkmate::assert_atomic_vector(x, add = coll, .var.name = "x")
    if (checkmate::anyMissing(x)) {
        warning(checkmate::check_atomic_vector(x, any.missing = FALSE))
    }
    # Check des types des objets
    checkmate::assert_true(typeof(dataTS) == typeof(x), add = coll, .var.name = "dataTS")

    checkmate::reportAssertions(coll)


    outputTS <- dataTS

    if (is.raw(outputTS)) {
        outputTS <- setValue_ts(
            dataTS = stats::ts(
                data = as.integer(outputTS),
                start = stats::start(outputTS),
                frequency = stats::frequency(outputTS)),
            date_ts = date_ts, x = as.integer(x))
        outputTS <- stats::ts(
            data = as.raw(outputTS),
            start = stats::start(outputTS),
            frequency = stats::frequency(outputTS))
    } else {
        start_ts <- format_date_ts(date_ts,
                                   frequency = as.integer(stats::frequency(dataTS)))
        end_ts <- next_date_ts(date_ts, frequency = as.integer(stats::frequency(dataTS)),
                               lag = length(x) - 1L)
        stats::window(
            x = outputTS, start = start_ts,
            end = end_ts,
            extend = TRUE) <- x
    }

    return(outputTS)
}

#' Combiner 2 ts
#'
#' @description La fonction `combine2ts` combine (comme c()) 2 time series de même fréquence (mensuelle ou trimestrielle).
#'
#' @param a un objet ts unidimensionnel conforme aux règles de assert_ts
#' @param b un objet ts unidimensionnel conforme aux règles de assert_ts
#'
#' @return En sortie, la fonction retourne un ts qui contient les valeurs de `a` aux temps de `a` et les valeurs de `b` aux temps de `b`.
#' @details Si `a` et `b` ont une période en commun, les valeurs de `b` écrasent celles de a sur la période concernée.
#' Si il existe une période sur laquelle ni `a` ni `b` ne prennent de valeur (mais qu'il existe des valeurs à des dates ultérieures et antérieures) alors le ts en sortie prendra NA sur cette période.
#' @export
#'
#' @examples
#'
#' trim_1 <- stats::ts(rep(1, 4), start = 2021, frequency = 4)
#'
#' mens_1 <- stats::ts(rep(1, 4), start = 2020, frequency = 12)
#' mens_2 <- stats::ts(rep(2, 4), start = 2022, frequency = 12)
#'
#' # La série de PIB est écrasé par trim_1 sur la période temporelle de trim_1
#' combine2ts(ev_pib, trim_1)
#'
#' # La période entre les séries temporelles mens_1 et mens_2 est complétée par des NA
#' combine2ts(mens_1, mens_2)
combine2ts <- function(a, b) {

    coll <- checkmate::makeAssertCollection()

    # Check de l'objet a
    assert_ts(a, add = coll, .var.name = "a")
    # Check de l'objet b
    assert_ts(b, add = coll, .var.name = "b")
    # Check same frequency
    checkmate::assert_true(stats::frequency(a) == stats::frequency(b), add = coll)
    # Check same type
    checkmate::assert_true(typeof(a) == typeof(b), add = coll)

    # temporalConsistence <- (stats::start(a) - stats::start(b)) * stats::frequency(a)
    # if (!isTRUE(all.equal(temporalConsistence, round(temporalConsistence)))) {
    #     stop("Les objets `a` et `b` doivent \u00eatre coh\u00e9rents temporellement.")
    # }

    checkmate::reportAssertions(coll)

    outputTS <- a

    if (is.raw(a)) {

        a <- stats::ts(
            x = as.integer(a),
            start = stats::start(a),
            frequency = stats::frequency(a))
        b <- stats::ts(
            x = as.integer(b),
            start = stats::start(b),
            frequency = stats::frequency(b))

        outputTS <- combine2ts(a, b)

        outputTS <- stats::ts(
            x = as.raw(outputTS),
            start = stats::start(outputTS),
            frequency = stats::frequency(outputTS))

        # Fréquence entière
    } else if (isTRUE(checkmate::check_int(stats::frequency(outputTS)))) {

        stats::window(x = outputTS, start = stats::start(b),
                      end = stats::end(b), extend = TRUE) <- b

        # Fréquence décimale
    } else if (isTRUE(checkmate::check_number(stats::frequency(outputTS)))) {

        outputDF <- as.data.frame(cbind(a, b))
        if (sum(is.na(outputDF$a) & (!is.na(outputDF$b))) > 0L) {
            warning("extending time series when replacing values")
        }

        outputDF$res <- outputDF$a
        outputDF$res[!is.na(outputDF$b)] <- outputDF$b[!is.na(outputDF$b)]

        outputTS <- stats::ts(
            data = outputDF$res,
            frequency = stats::frequency(a),
            start = min(getTimeUnits(as.integer(stats::start(a)),
                                     frequency = stats::frequency(a)),
                        getTimeUnits(as.integer(stats::start(b)),
                                     frequency = stats::frequency(b))))
    }
    return(outputTS)
}

#' @export
extend_ts <- function(dataTS, x, date_ts = NULL, replace_na = TRUE) {

    coll <- checkmate::makeAssertCollection()

    # Check de l'objet dataTS
    assert_ts(dataTS, add = coll, .var.name = "dataTS")
    # Check de l'objet x un vecteur atomic
    checkmate::assert_atomic_vector(x, add = coll, .var.name = "x")
    # Check de replace_na
    checkmate::assert_flag(replace_na, add = coll, .var.name = "replace_na")
    # Check du format date_ts
    if (!is.null(date_ts)) {
        date_ts <- assert_date_ts(x = date_ts, frequency, add = coll, .var.name = "date_ts")
    }

    checkmate::reportAssertions(coll)

    start_ts <- as.integer(stats::start(dataTS))
    end_ts <- as.integer(stats::end(dataTS))

    if (replace_na) {
        start_replacement <- next_date_ts(lastDate(dataTS))
    } else {
        start_replacement <- next_date_ts(end_ts)
    }

    frequency <- as.integer(stats::frequency(dataTS))

    if (!is.null(date_ts)) {

        if (!is_before(start_replacement, date_ts, frequency = frequency)) {
            stop("La date de fin de remplacement est ant\u00e9rieur \u00e0 la date de fin des donn\u00e9es.")
        }
        length_replacement <- diff_periode(a = start_replacement,
                                           b = date_ts, frequency = frequency)
        if (length_replacement %% length(x) != 0L) {
            stop("number of values supplied is not a sub-multiple of the number of values to be replaced")
        }
        end_replacement <- date_ts

    } else {
        end_replacement <- next_date_ts(start_replacement, lag = length(x), frequency = frequency)
    }

    stats::window(dataTS, start = start_replacement, end = end_replacement, extend = TRUE) <- x
    return(dataTS)
}

#' @export
na_trim <- function(dataTS) {

    coll <- checkmate::makeAssertCollection()

    # Check de l'objet dataTS
    assert_ts(dataTS, add = coll, .var.name = "dataTS")
    # Check du contenu (pas que des NA)
    checkmate::assert_atomic_vector(dataTS, all.missing = FALSE,
                                    add = coll, .var.name = "dataTS")

    checkmate::reportAssertions(coll)

    non_na <- seq_along(dataTS)[!is.na(dataTS)]
    content <- dataTS[min(non_na):max(non_na)]

    start_ts <- as.integer(stats::start(dataTS))
    frequency <- as.integer(stats::frequency(dataTS))

    return(stats::ts(data = dataTS,
                     start = next_date_ts(date_ts = start_ts,
                                          frequency = frequency,
                                          lag = min(non_na) - 1L),
                     frequency = frequency)
    )
}
