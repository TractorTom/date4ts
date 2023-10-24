#' Change certaines valeurs d'un ts
#'
#' @description La fonction `setValue_ts` modifie la ou les valeurs d'un objet ts à une date donnée.
#'
#' @param dataTS un objet ts unidimensionnel conforme aux règles de assert_ts
#' @param date_ts un vecteur numérique, de préférence `integer` au format `AAAA`, `c(AAAA, MM)` ou `c(AAAA, TT)`
#' @param x un vecteur de même type que le ts `dataTS`
#'
#' @return En sortie, la fonction retourne une copie de l'objet `dataTS` modifié avec les valeurs de `x` imputés à partir de la date `date_ts`.
#' @export
#'
#' @examples
#' setValue_ts(
#'     dataTS = ev_pib,
#'     date_ts = c(2021L, 2L),
#'     x = c(1, 2, 3)
#' )
#'
setValue_ts <- function(dataTS, date_ts, x) {
    # coll <- checkmate::makeAssertCollection()
    coll <- NULL

    # Check de l'objet dataTS
    assert_ts(dataTS, add = coll, .var.name = "dataTS")
    # Check de l'objet x un vecteur atomic
    checkmate::assert_atomic_vector(x, add = coll, .var.name = "x")
    if (checkmate::anyMissing(x)) {
        err <- try(checkmate::assert_atomic_vector(x, any.missing = FALSE, .var.name = "x"), silent = TRUE)
        warning(attr(err, "condition")$message)
    }
    # Check des types des objets
    if (!isTRUE(typeof(dataTS) == typeof(x))) {
        # coll$push("Les objets `dataTS` et `x` doivent \u00eatre de m\u00eame type.")
        stop("Les objets `dataTS` et `x` doivent \u00eatre de m\u00eame type.")
    }

    # checkmate::reportAssertions(coll)

    frequency_ts <- as.integer(stats::frequency(dataTS))

    # Check du format date_ts
    date_ts <- assert_date_ts(x = date_ts, frequency_ts, .var.name = "date_ts")

    outputTS <- dataTS

    if (is.raw(outputTS)) {
        outputTS <- setValue_ts(
            dataTS = stats::ts(
                data = as.integer(outputTS),
                start = stats::start(outputTS),
                frequency = stats::frequency(outputTS)
            ),
            date_ts = date_ts, x = as.integer(x)
        )
        outputTS <- stats::ts(
            data = as.raw(outputTS),
            start = stats::start(outputTS),
            frequency = stats::frequency(outputTS)
        )
    } else {
        start_ts <- format_date_ts(date_ts,
            frequency_ts = as.integer(stats::frequency(dataTS))
        )
        end_ts <- next_date_ts(date_ts,
            frequency_ts = as.integer(stats::frequency(dataTS)),
            lag = length(x) - 1L
        )
        stats::window(
            x = outputTS, start = start_ts,
            end = end_ts,
            extend = TRUE
        ) <- x
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
#'
combine2ts <- function(a, b) {
    # coll <- checkmate::makeAssertCollection()
    coll <- NULL

    # Check de l'objet a
    assert_ts(a, add = coll, .var.name = "a")
    # Check de l'objet b
    assert_ts(b, add = coll, .var.name = "b")

    # checkmate::reportAssertions(coll)

    # Check same frequency_ts
    if (!isTRUE(stats::frequency(a) == stats::frequency(b))) {
        stop("Les objets `a` et `b` doivent avoir la m\u00eame fr\u00e9quence.")
    }

    frequency_ts <- stats::frequency(a)

    # Check des types des objets
    if (!isTRUE(typeof(a) == typeof(b))) {
        stop("Les objets `a` et `b` doivent \u00eatre de m\u00eame type.")
    }

    outputTS <- a

    if (is.raw(a)) {
        a <- stats::ts(
            data = as.integer(a),
            start = stats::start(a),
            frequency = frequency_ts
        )
        b <- stats::ts(
            data = as.integer(b),
            start = stats::start(b),
            frequency = frequency_ts
        )

        outputTS <- combine2ts(a, b)

        outputTS <- stats::ts(
            data = as.raw(outputTS),
            start = stats::start(outputTS),
            frequency = frequency_ts
        )

        # Fréquence entière
    } else if (isTRUE(checkmate::check_int(frequency_ts))) {
        stats::window(
            x = outputTS, start = stats::start(b),
            end = stats::end(b), extend = TRUE
        ) <- b

        # Fréquence décimale
    } else if (isTRUE(checkmate::check_number(frequency_ts))) {
        outputDF <- as.data.frame(cbind(a, b))
        if (sum(is.na(outputDF$a) & (!is.na(outputDF$b))) > 0L) {
            warning("extending time series when replacing values")
        }
        outputDF$res <- outputDF$a
        outputDF$res[!is.na(outputDF$b)] <- outputDF$b[!is.na(outputDF$b)]

        outputTS <- stats::ts(
            data = outputDF$res,
            frequency = frequency_ts,
            start = min(
                date_ts2TimeUnits(as.integer(stats::start(a)),
                    frequency_ts = frequency_ts
                ),
                date_ts2TimeUnits(as.integer(stats::start(b)),
                    frequency_ts = frequency_ts
                )
            )
        )
    }
    return(outputTS)
}

#' Ajoute de nouvelles valeurs à un ts
#'
#' @description La fonction `extend_ts` ajoute de nouvelles valeurs à un ts
#'
#' @param dataTS un objet ts unidimensionnel conforme aux règles de assert_ts
#' @param x un vecteur de même type que le ts `dataTS`
#' @param date_ts un vecteur numérique, de préférence `integer` au format `date_ts` (`AAAA`, `c(AAAA, MM)` ou `c(AAAA, TT)`) (default NULL)
#' @param replace_na un booléen
#'
#' @return En sortie, la fonction retourne une copie de l'objet `dataTS` complété avec le vecteur `x`.
#' @details Si `replace_na` vaut `TRUE` alors le remplacement commence dès que l'objet ne contient que des NA. Dans le cas contraire, le ts est étendu, qu'il contienne des NA ou non à la fin.
#' Si le vecteur `x` est de taille un sous-multiple de la différence de période entre la date de fin de `dataTS` et `date_ts`, le vecteur `x` est répété.
#' @export
#'
#' @examples
#'
#' ts1 <- ts(c(rep(NA, 3L), 1:10, rep(NA, 3L)), start = 2020, frequency = 12)
#' x <- rep(3, 2)
#'
#' extend_ts(ts1, x)
#' extend_ts(ts1, x, replace_na = FALSE)
#' extend_ts(ts1, x, replace_na = TRUE, date_ts = c(2021L, 7L))
#'
extend_ts <- function(dataTS, x, date_ts = NULL, replace_na = TRUE) {
    # coll <- checkmate::makeAssertCollection()
    coll <- NULL

    # Check de l'objet dataTS
    assert_ts(dataTS, add = coll, .var.name = "dataTS")
    # Check de l'objet x un vecteur atomic
    checkmate::assert_atomic_vector(x, add = coll, .var.name = "x")
    # Check de replace_na
    checkmate::assert_flag(replace_na, add = coll, .var.name = "replace_na")

    # checkmate::reportAssertions(coll)

    frequency_ts <- as.integer(stats::frequency(dataTS))

    # Check du format date_ts
    if (!is.null(date_ts)) {
        date_ts <- assert_date_ts(x = date_ts, frequency_ts, add = coll, .var.name = "date_ts")
    }

    # start_ts <- as.integer(stats::start(dataTS))
    end_ts <- as.integer(stats::end(dataTS))


    if (replace_na) {
        start_replacement <- next_date_ts(last_date(dataTS), frequency_ts = frequency_ts)
    } else {
        start_replacement <- next_date_ts(end_ts, frequency_ts = frequency_ts)
    }

    if (!is.null(date_ts)) {
        if (!is_before(start_replacement, date_ts, frequency_ts = frequency_ts)) {
            stop("La date de fin de remplacement est ant\u00e9rieur \u00e0 la date de fin des donn\u00e9es.")
        }
        length_replacement <- diff_periode(
            a = start_replacement,
            b = date_ts, frequency_ts = frequency_ts
        )
        if (length_replacement %% length(x) != 0L) {
            stop("number of values supplied is not a sub-multiple of the number of values to be replaced")
        }
        end_replacement <- date_ts
    } else {
        end_replacement <- next_date_ts(start_replacement, lag = length(x) - 1L, frequency_ts = frequency_ts)
    }

    stats::window(dataTS, start = start_replacement, end = end_replacement, extend = TRUE) <- x
    return(dataTS)
}

#' Supprime les NA aux bords
#'
#' @description La fonction `na_trim` supprime les NA en début et en fin de période.
#'
#' @param dataTS un objet ts unidimensionnel conforme aux règles de assert_ts
#'
#' @return En sortie, la fonction retourne une copie de l'objet `dataTS` corrigé des NA et début et fin de série.
#' @details L'objet retourné commence et finis par des valeurs non manquantes.
#' @export
#'
#' @examples
#'
#' ts1 <- ts(c(rep(NA, 3L), 1:10, rep(NA, 3L)), start = 2020, frequency = 12)
#' ts2 <- ts(c(1:10, rep(NA, 3L)), start = c(2023, 2), frequency = 4)
#' ts3 <- ts(c(rep(NA, 3L), 1:10), start = 2000, frequency = 12)
#'
#' na_trim(ts1)
#' na_trim(ts2)
#' na_trim(ts3)
#'
na_trim <- function(dataTS) {
    # coll <- checkmate::makeAssertCollection()
    coll <- NULL

    # Check de l'objet dataTS
    assert_ts(dataTS, add = coll, .var.name = "dataTS")
    # Check du contenu (pas que des NA)
    checkmate::assert_atomic_vector(dataTS,
        all.missing = FALSE,
        add = coll, .var.name = "dataTS"
    )

    # checkmate::reportAssertions(coll)

    non_na <- seq_along(dataTS)[!is.na(dataTS)]
    content <- dataTS[min(non_na):max(non_na)]

    start_ts <- as.integer(stats::start(dataTS))
    frequency_ts <- as.integer(stats::frequency(dataTS))

    return(stats::ts(
        data = content,
        start = next_date_ts(
            date_ts = start_ts,
            frequency_ts = frequency_ts,
            lag = min(non_na) - 1L
        ),
        frequency = frequency_ts
    ))
}
