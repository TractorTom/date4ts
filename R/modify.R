#' @title Change certaines valeurs d'un ts
#'
#' @description
#' La fonction `set_value_ts` modifie la ou les valeurs d'un objet
#' ts à une date donnée.
#'
#' @inheritParams first_date
#' @inheritParams trim2mens
#' @param replacement un vecteur de même type que le ts `series`
#'
#' @returns En sortie, la fonction retourne une copie de l'objet `series`
#' modifié avec les valeurs de `replacement` imputés à partir de la date
#' `date_ts`.
#'
#' @export
#'
#' @examples
#' set_value_ts(
#'     series = ev_pib,
#'     date_ts = c(2021L, 2L),
#'     replacement = c(1, 2, 3)
#' )
#'
set_value_ts <- function(series, date_ts, replacement) {
    coll <- checkmate::makeAssertCollection()

    # Check de l'objet series
    assert_ts(series, add = coll, .var.name = "series")

    # Check de l'objet replacement un vecteur atomic
    checkmate::assert_atomic_vector(
        replacement,
        add = coll,
        .var.name = "replacement"
    )

    check_1 <- checkmate::check_atomic_vector(x = replacement)
    check_2 <- checkmate::anyMissing(x = replacement)
    if (isTRUE(check_1) && isTRUE(check_2)) {
        warning(
            "the argument replacement ",
            checkmate::check_atomic_vector(
                x = replacement,
                any.missing = FALSE
            ),
            call. = FALSE
        )
    }

    # Check des types des objets
    if (!isTRUE(typeof(series) == typeof(replacement))) {
        error_message <- paste(
            "Les objets `series` et `replacement` doivent",
            "\u00eatre de m\u00eame type."
        )
        coll$push(msg = error_message)
    }

    checkmate::reportAssertions(coll)

    frequency_ts <- as.integer(stats::frequency(series))

    # Check du format date_ts
    date_ts <- assert_date_ts(x = date_ts, frequency_ts, .var.name = "date_ts")

    ts_output <- series

    if (is.raw(ts_output)) {
        ts_output <- set_value_ts(
            series = stats::ts(
                data = as.integer(ts_output),
                start = stats::start(ts_output),
                frequency = stats::frequency(ts_output)
            ),
            date_ts = date_ts,
            replacement = as.integer(replacement)
        )
        ts_output <- stats::ts(
            data = as.raw(ts_output),
            start = stats::start(ts_output),
            frequency = stats::frequency(ts_output)
        )
    } else {
        start_ts <- normalize_date_ts(
            date_ts = date_ts,
            frequency_ts = as.integer(stats::frequency(series))
        )
        end_ts <- next_date_ts(
            date_ts,
            frequency_ts = as.integer(stats::frequency(series)),
            lag = length(replacement) - 1L
        )
        stats::window(
            x = ts_output,
            start = start_ts,
            end = end_ts,
            extend = TRUE
        ) <- replacement
    }

    return(ts_output)
}

#' @title Combiner 2 ts
#'
#' @description
#' La fonction `combine2ts` combine (comme c()) 2 time series de
#' même fréquence (mensuelle ou trimestrielle).
#'
#' @param a un objet ts unidimensionnel conforme aux règles de assert_ts
#' @param b un objet ts unidimensionnel conforme aux règles de assert_ts
#'
#' @returns En sortie, la fonction retourne un ts qui contient les valeurs de
#' `a` aux temps de `a` et les valeurs de `b` aux temps de `b`.
#' @details Si `a` et `b` ont une période en commun, les valeurs de `b` écrasent
#' celles de a sur la période concernée.
#' Si il existe une période sur laquelle ni `a` ni `b` ne prennent de valeur
#' (mais qu'il existe des valeurs à des dates ultérieures et antérieures) alors
#' le ts en sortie prendra NA sur cette période.
#' @export
#'
#' @examples
#'
#' trim_1 <- stats::ts(rep(1, 4), start = 2021, frequency = 4L)
#'
#' mens_1 <- stats::ts(rep(1, 4), start = 2020, frequency = 12L)
#' mens_2 <- stats::ts(rep(2, 4), start = 2022, frequency = 12L)
#'
#' # La série de PIB est écrasé par trim_1 sur la période temporelle de trim_1
#' combine2ts(ev_pib, trim_1)
#'
#' # La période entre les séries temporelles mens_1 et mens_2 est complétée par
#' # des NA
#' combine2ts(mens_1, mens_2)
#'
combine2ts <- function(a, b) {
    coll <- checkmate::makeAssertCollection()

    # Check de l'objet a
    assert_ts(a, add = coll, .var.name = "a")
    # Check de l'objet b
    assert_ts(b, add = coll, .var.name = "b")

    if (isTRUE(check_ts(a)) && isTRUE(check_ts(b))) {
        # Check same frequency_ts
        if (!isTRUE(stats::frequency(a) == stats::frequency(b))) {
            error_message <- paste(
                "Les objets `a` et `b` doivent",
                "avoir la m\u00eame fr\u00e9quence."
            )
            coll$push(msg = error_message)
        }

        # Check des types des objets
        if (!isTRUE(typeof(a) == typeof(b))) {
            error_message <- paste(
                "Les objets `a` et `b` doivent",
                "\u00eatre de m\u00eame type."
            )
            coll$push(msg = error_message)
        }
    }

    checkmate::reportAssertions(coll)

    frequency_ts <- stats::frequency(a)
    ts_output <- a

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

        ts_output <- combine2ts(a, b)

        ts_output <- stats::ts(
            data = as.raw(ts_output),
            start = stats::start(ts_output),
            frequency = frequency_ts
        )

        # Fréquence entière
    } else if (isTRUE(checkmate::check_int(frequency_ts))) {
        stats::window(
            x = ts_output,
            start = stats::start(b),
            end = stats::end(b),
            extend = TRUE
        ) <- b

        # Fréquence décimale
    } else if (isTRUE(checkmate::check_number(frequency_ts))) {
        df_output <- as.data.frame(cbind(a, b))
        if (sum(is.na(df_output$a) & (!is.na(df_output$b))) > 0L) {
            warning(
                "extending time series when replacing values",
                call. = FALSE
            )
        }
        df_output$res <- df_output$a
        df_output$res[!is.na(df_output$b)] <- df_output$b[!is.na(df_output$b)]

        ts_output <- stats::ts(
            data = df_output$res,
            frequency = frequency_ts,
            start = min(
                date_ts2timeunits(
                    as.integer(stats::start(a)),
                    frequency_ts = frequency_ts
                ),
                date_ts2timeunits(
                    as.integer(stats::start(b)),
                    frequency_ts = frequency_ts
                )
            )
        )
    }
    return(ts_output)
}

#' @title Ajoute de nouvelles valeurs à un ts
#'
#' @description
#' La fonction `extend_ts` ajoute de nouvelles valeurs à un ts.
#'
#' @inheritParams set_value_ts
#' @param replace_na un booléen.
#' @param date_ts_to un vecteur numérique, de préférence `integer`, au format
#' date_ts, c'est-à-dire `AAAA`, `c(AAAA, MM)` ou `c(AAAA, TT)`.
#' @param times un entier qui précise le nombre de fois où \code{replacement}
#' doit être répété, le vecteur entier.
#' @param each un entier qui précise le nombre de fois où \code{replacement}
#' doit être répété mais élément par élément.
#'
#' @returns En sortie, la fonction retourne une copie de l'objet `series`
#' complété avec le vecteur `replacement`.
#'
#' @details
#' \code{date_ts_to} désigne la date jusqu'à laquelle le remplacement
#' s'effectue. Par défault, cette valeur vaut \code{NULL}.
#'
#' Si `replace_na` vaut `TRUE` alors le remplacement commence dès que
#' l'objet ne contient que des NA. Dans le cas contraire, le ts est étendu,
#' qu'il contienne des NA ou non à la fin.
#' Si le vecteur `replacement` est de taille un sous-multiple de la différence
#' de période entre la date de fin de `series` et `date_ts_to`, le vecteur
#' `replacement` est répété jusqu'à la date `date_ts_to`. Sinon une erreur est
#' générée.
#'
#' Les arguments \code{times} et \code{each} en sont utilisé que si
#' \code{date_ts} est manquant (non fourni par l'utilisateur). Si tel est le
#' cas, ils se comporte comme si \code{replacement} devenait
#' \code{rep(replacement, times = times, each = each)}.
#'
#' @export
#'
#' @examples
#'
#' ts1 <- ts(
#'     data = c(rep(NA_integer_, 3L), 1L:10L, rep(NA_integer_, 3L)),
#'     start = 2020,
#'     frequency = 12
#' )
#' x <- rep(3L, 2L)
#'
#' extend_ts(series = ts1, replacement = x)
#' extend_ts(series = ts1, replacement = x, replace_na = FALSE)
#' extend_ts(series = ts1, replacement = x,
#'           date_ts_to = c(2021L, 7L), replace_na = TRUE)
#'
extend_ts <- function(
    series,
    replacement,
    date_ts_to = NULL,
    replace_na = TRUE,
    times = 1L,
    each = 1L
) {
    coll <- checkmate::makeAssertCollection()

    # Check de l'objet series
    assert_ts(series, add = coll, .var.name = "series")
    # Check de l'objet replacement un vecteur atomic
    checkmate::assert_atomic_vector(
        replacement,
        add = coll,
        .var.name = "replacement"
    )
    # Check de replace_na
    checkmate::assert_flag(replace_na, add = coll, .var.name = "replace_na")

    if (!isTRUE(typeof(series) == typeof(replacement))) {
        error_message <- paste(
            "Les objets `series` et `replacement` doivent",
            "\u00eatre de m\u00eame type."
        )
        coll$push(msg = error_message)
    }

    checkmate::reportAssertions(coll)

    frequency_ts <- as.integer(stats::frequency(series))

    # Check du format date_ts_to
    if (!is.null(date_ts_to)) {
        date_ts_to <- assert_date_ts(
            x = date_ts_to,
            frequency_ts,
            add = coll,
            .var.name = "date_ts_to"
        )
    } else {
        replacement <- rep(replacement, times = times, each = each)
    }

    if (is.raw(series)) {
        new_series <- stats::ts(
            data = as.integer(series),
            start = stats::start(series),
            frequency = frequency_ts
        )
        new_replacement <- as.integer(replacement)
        ts_output <- extend_ts(
            series = new_series,
            replacement = new_replacement,
            date_ts_to = date_ts_to,
            replace_na = replace_na
        )

        ts_output <- stats::ts(
            data = as.raw(ts_output),
            start = stats::start(ts_output),
            frequency = frequency_ts
        )

        return(ts_output)
    }

    start_replacement <- next_date_ts(
        date_ts = ifelse(
            test = replace_na,
            yes = last_date(series),
            no = as.integer(stats::end(series))
        ),
        frequency_ts = frequency_ts
    )

    if (!is.null(date_ts_to)) {
        if (
            !is_before(
                start_replacement,
                date_ts_to,
                frequency_ts = frequency_ts
            )
        ) {
            stop(
                c(
                    "La date de fin de remplacement est",
                    " ant\u00e9rieur \u00e0 la date de fin des donn\u00e9es."
                ),
                call. = FALSE
            )
        }
        length_replacement <- diff_periode(
            a = start_replacement,
            b = date_ts_to,
            frequency_ts = frequency_ts
        )
        if (length_replacement %% length(replacement) != 0L) {
            stop(
                c(
                    "number of values supplied is not a",
                    " sub-multiple of the number of values to be replaced"
                ),
                call. = FALSE
            )
        }
        end_replacement <- date_ts_to
    } else {
        end_replacement <- next_date_ts(
            date_ts = start_replacement,
            lag = length(replacement) - 1L,
            frequency_ts = frequency_ts
        )
    }

    stats::window(
        series,
        start = start_replacement,
        end = end_replacement,
        extend = TRUE
    ) <- replacement
    return(series)
}

#' @title Supprime les NA aux bords
#'
#' @description
#' La fonction `na_trim` supprime les NA en début et en fin de
#' période.
#'
#' @inheritParams first_date
#' @param sides une chaine de caractere qui spécifie quelle NA doivent être
#' retirés (au début et à la fin ("both"), juste au début ("left") ou juste à
#' la fin ("right"))
#'
#' @returns En sortie, la fonction retourne une copie de l'objet `series`
#' corrigée des NA et début et fin de série.
#' @details L'objet retourné commence et finis par des valeurs non manquantes.
#' @export
#'
#' @examples
#'
#' ts1 <- ts(c(rep(NA, 3L), 1:10, rep(NA, 3L)), start = 2020, frequency = 12L)
#' ts2 <- ts(c(1:10, rep(NA, 3L)), start = c(2023, 2), frequency = 4L)
#' ts3 <- ts(c(rep(NA, 3L), 1:10), start = 2000, frequency = 12L)
#'
#' na_trim(ts1)
#' na_trim(ts2)
#' na_trim(ts3)
#'
na_trim <- function(series, sides = c("both", "left", "right")) {
    coll <- NULL

    # Check de l'objet series
    assert_ts(series, add = coll, .var.name = "series")
    # Check du contenu (pas que des NA)
    checkmate::assert_atomic_vector(
        series,
        all.missing = FALSE,
        add = coll,
        .var.name = "series"
    )
    # Check de l'argument sides
    sides <- match.arg(sides)

    non_na <- seq_along(series)[!is.na(series)]

    frequency_ts <- assert_frequency(
        x = stats::frequency(series),
        add = coll,
        warn = FALSE
    )
    start_ts <- normalize_date_ts(
        date_ts = stats::start(series),
        frequency_ts = frequency_ts,
        test = FALSE
    )
    content <- switch(
        EXPR = sides,
        both = series[min(non_na):max(non_na)],
        right = series[1L:max(non_na)],
        left = series[min(non_na):length(series)]
    )
    if (sides != "right") {
        start_ts <- next_date_ts(
            date_ts = start_ts,
            frequency_ts = frequency_ts,
            lag = min(non_na) - 1L
        )
    }
    return(stats::ts(
        data = content,
        start = start_ts,
        frequency = frequency_ts
    ))
}
