
#' Change certaines valeurs d'un ts
#'
#' @description La fonction `setValue_ts` modifie la ou les valeurs d'un objet ts à une date donnée.
#'
#' @param dataTS un objet ts unidimensionnel conforme aux règles de isGoodTS
#' @param date_ts un vecteur numérique, de préférence integer au format AAAA, c(AAAA, MM) ou c(AAAA, TT)
#' @param value un vecteur de même type que le ts `dataTS`
#'
#' @return En sortie, la fonction retourne l'objet `dataTS` n objet ts modifié avec les valeurs de `value` imputés à partir de la date `date`.
#' @export
#'
#' @examples
#' ev_pib |> setValue_ts(date = c(2021L, 2L), value = c(1, 2, 3))
setValue_ts <- function(dataTS, date_ts, value) {

    # Check de l'objet dataTS
    if  (!isGoodTS(dataTS, warn = FALSE)) {
        stop("L'objet `dataTS` doit \u00eatre un ts unidimensionnel de fr\u00e9quence mensuelle ou trimestrielle.")
    }

    frequency <- stats::frequency(dataTS)

    # Check de la fréquence
    if (!is_good_frequency(frequency)) {
        stop("La fr\u00e9quence doit \u00eatre trimestrielle ou mensuelle.")
    }

    # Check du format date_ts
    if (!is_date_ts(date_ts, frequency = frequency)) {
        stop("La date est au mauvais format.")
    }

    if (!is.null(dim(value))) stop("L'argument value doit \u00eatre unidimensionnel.")
    if (typeof(dataTS) != typeof(value)) stop("Les objets dataTS et value doivent \u00eatre de m\u00eame type.")
    if (any(is.na(value))) warning("L'argument value contient des NAs.")

    outputTS <- dataTS

    if (is.raw(outputTS)) {
        outputTS <- outputTS |>
            as.integer() |>
            stats::ts(start = stats::start(outputTS), frequency = stats::frequency(outputTS)) |>
            setValue_ts(date_ts = date_ts, value = as.integer(value))
        outputTS <- outputTS |>
            as.raw() |>
            stats::ts(start = stats::start(outputTS), frequency = stats::frequency(outputTS))
    } else {
        outputTS |>
            stats::window(start = date_ts |> format_date_ts(frequency = stats::frequency(dataTS)),
                          end =   date_ts |> next_date_ts(frequency = stats::frequency(dataTS), lag = length(value) - 1L),
                          extend = TRUE) <- value
    }

    return(outputTS)
}

#' Combiner 2 ts
#'
#' @description La fonction `combine2ts` combine (comme c()) 2 time series de même fréquence (mensuelle ou trimestrielle).
#'
#' @param a un objet ts unidimensionnel conforme aux règles de isGoodTS
#' @param b un objet ts unidimensionnel conforme aux règles de isGoodTS
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
#' # La période temporelle interne entre les séries temporelles mens_1 et mens_2 est complétée par des NA
#' combine2ts(mens_1, mens_2)
combine2ts <- function(a, b) {

    # Check des objets a et b
    if  (!(isGoodTS(a, warn = FALSE) &
           isGoodTS(b, warn = FALSE))) {
        stop("Les objets `a` et `b` doivent \u00eatre des ts unidimensionnels de fr\u00e9quence mensuelle ou trimestrielle.")
    }

    if (stats::frequency(a) != stats::frequency(b)) stop("Les objets `a` et `b` doivent avoir la m\u00eame fr\u00e9quence.")
    if (typeof(a) != typeof(b))                     stop("Les objets `a` et `b` doivent \u00eatre de m\u00eame type.")

    temporalConsistence <- (stats::start(a) - stats::start(b)) * stats::frequency(a)
    if (!isTRUE(all.equal(temporalConsistence, round(temporalConsistence))))
        stop("Les objets `a` et `b` doivent \u00eatre coh\u00e9rents temporellement.")

    outputTS <- a

    if (is.raw(a)) {
        a <- a |>
            as.integer() |>
            stats::ts(start = stats::start(a),
                      frequency = stats::frequency(a))
        b <- b |>
            as.integer() |>
            stats::ts(start = stats::start(b),
                      frequency = stats::frequency(b))
        outputTS <- combine2ts(a, b)
        outputTS <- outputTS |>
            as.raw() |>
            stats::ts(start = stats::start(outputTS),
                      frequency = stats::frequency(outputTS))

    } else if (isTRUE(all.equal(stats::frequency(a),
                                round(stats::frequency(a))))) {
        outputTS |>
            stats::window(start = stats::start(b),
                          end = stats::end(b), extend = TRUE) <- b

    } else if (is.numeric(stats::frequency(outputTS))) {
        outputDF <- cbind(a, b) |> as.data.frame()
        if (sum(is.na(outputDF$a) & (!is.na(outputDF$b))) > 0L) warning("extending time series when replacing values")

        outputDF$res <- outputDF$a
        outputDF$res[!is.na(outputDF$b)] <- outputDF$b[!is.na(outputDF$b)]

        outputTS <- stats::ts(
            data = outputDF$res,
            frequency = stats::frequency(a),
            start = min(getTimeUnits(stats::start(a) |> as.integer(),
                                     frequency = stats::frequency(a)),
                        getTimeUnits(stats::start(b) |> as.integer(),
                                     frequency = stats::frequency(b))))
    }
    return(outputTS)
}


extend_ts <- function(dataTS, x, date_ts = NULL, replace_na = TRUE) {

    # Check de l'objet dataTS
    if  (!isGoodTS(dataTS, warn = FALSE)) {
        stop("L'objet `dataTS` doit \u00eatre un ts unidimensionnel de fr\u00e9quence mensuelle ou trimestrielle.")
    }

    if (!is_vector(x)) stop("L'objet `x` doit \u00eatre un vecteur unidimentionnel non vide.")

    # Check de replace_na
    if (!is_single_boolean(x = replace_na)) {
        stop("L'argument replace_na doit \u00eatre un bool\u00e9en de longueur 1.")
    }

    if (replace_na) {
        start_replacement <- lastDate(dataTS) |> next_date_ts()
    } else {
        start_replacement <- end(dataTS) |> next_date_ts()
    }

    frequency <- stats::frequency(dataTS)

    if (!is.null(date_ts)) {

        # Check de la fréquence
        if (!is_good_frequency(frequency)) {
            stop("La fr\u00e9quence doit \u00eatre trimestrielle ou mensuelle.")
        }

        # Check du format date_ts
        if (!is_date_ts(date_ts, frequency = frequency)) {
            stop("La date est au mauvais format.")
        }

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
        end_replacement <- next_date_ts(end_ts, lag = length(x) + 1L, frequency = frequency)
    }

    window(dataTS, start = start_replacement, end = date_ts, extend = TRUE) <- x
    return(dataTS)
}

na_trim <- function(dataTS) {

    # Check de l'objet dataTS
    if  (!isGoodTS(dataTS, warn = FALSE)) {
        stop("L'objet `dataTS` doit \u00eatre un ts unidimensionnel de fr\u00e9quence mensuelle ou trimestrielle.")
    }

    non_na <- seq_along(dataTS)[!is.na(dataTS)]

    if (length(non_na) == 0L) {
        stop("L'objet ne contient que des NAs.")
    }

    content <- dataTS[min(non_na):max(non_na)]

    start_ts <- stats::start(dataTS)
    frequency_ts <- stats::frequency(dataTS)

    return(ts(data = dataTS,
              start = next_date_ts(date_ts = start_ts,
                                   frequency = frequency_ts,
                                   lag = min(non_na) - 1L),
              frequency = frequency_ts)
    )
}
