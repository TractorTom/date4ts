
#' Change certaines valeurs d'un ts
#'
#' @description La fonction `setValue_ts` modifie la ou les valeurs d'un objet ts à une date donnée.
#'
#' @param dataTS un objet ts
#' @param date une date au format c(AAAA, MM)
#' @param value un vecteur de même type que le ts `dataTS`
#'
#' @return En sortie, la fonction retourne l'objet `dataTS` n objet ts modifié avec les valeurs de `value` imputés à partir de la date `date`.
#' @export
#'
#' @examples
#' ev_pib |> setValue_ts(date = c(2021L, 2L), value = c(1, 2, 3))
setValue_ts <- function(dataTS, date, value){
    if (!stats::is.ts(dataTS) | stats::is.mts(dataTS)) stop("L'objets dataTS doit être un ts unidimensionnel.")
    if (!(stats::frequency(dataTS) %in% c(4L, 12L))) stop("L'objets dataTS doit être trimestriel ou mensuel.")
    if (!ts4conj::isTSdate(date)) stop("La date est au mauvais format.")
    if (!is.null(dim(value))) stop("L'argument value doit être unidimensionnel.")
    if (typeof(dataTS) != typeof(value)) stop("Les objets dataTS et value doivent être de même type.")
    if (any(is.na(value))) warning("L'argument value contient des NAs.")

    temporalConsistence <- stats::frequency(dataTS) * stats::start(dataTS)
    if (length(stats::start(dataTS)) == 1 && !isTRUE(all.equal(temporalConsistence, round(temporalConsistence))))
        stop("Les objets a et b doivent être cohérents temporellement.")


    outputTS <- dataTS

    if (is.raw(outputTS)) {
        outputTS <- outputTS |>
            as.integer() |>
            ts(start = stats::start(outputTS), frequency = stats::frequency(outputTS)) |>
            ts4conj::setValue_ts(date = date, value = as.integer(value))
        outputTS <- outputTS |>
            as.raw() |>
            ts(start = stats::start(outputTS), frequency = stats::frequency(outputTS))
    } else {
        outputTS |>
            stats::window(start = date |> ts4conj::nextDate(frequency = stats::frequency(dataTS), lag = 0L),
                          end =   date |> ts4conj::nextDate(frequency = stats::frequency(dataTS), lag = length(value) - 1L),
                          extend = TRUE) <- value
    }

    return(outputTS)
}

#' Combiner 2 ts
#'
#' @description La fonction `combine2ts` combine (comme c()) 2 time series de même fréquence (mensuelle ou trimestrielle).
#'
#' @param a un objet ts
#' @param b un objet ts
#'
#' @return En sortie, la fonction retourne un ts qui contient les valeurs de `a` aux temps de `a` et les valeurs de `b` aux temps de `b`.
#' @details Si `a` et `b` ont une période en commun, les valeurs de `b` écrasent celles de a sur la période concernée.
#' Si il existe une période sur laquelle ni `a` ni `b` ne prennent de valeur (mais qu'il existe des valeurs à des dates ultérieures et antérieures) alors le ts en sortie prendra NA sur cette période.
#' @export
#'
#' @examples
#' #La temporalité du second ts n'est pas prise en compte
# x1 <- ts(rep(1, 4), start = 2000, frequency = 12)
# x2 <- ts(rep(2, 4), start = 2020, frequency = 12)
# combine2ts(x1, x2)
#'
#' #Attention aux NA présent en début ou fin de ts !
#' ev_pib |> combine2ts(x1)
combine2ts <- function(a, b){
    if (!(stats::is.ts(a) & stats::is.ts(b)) |
        stats::is.mts(a) | stats::is.mts(b))        stop("Les objets a et b doivent être des ts unidimensionnels.")
    if ((!stats::frequency(a) %in% c(4L, 12L)) |
        (!stats::frequency(b) %in% c(4L, 12L)))     stop("Les objets a et b doivent être trimestriels ou mensuels.")
    if (stats::frequency(a) != stats::frequency(b)) stop("Les objets a et b doivent avoir la même fréquence.")
    if (typeof(a) != typeof(b))                     stop("Les objets a et b doivent être de même type.")

    temporalConsistence <- (ts4conj::getTimeUnits(stats::start(a), frequency = stats::frequency(a)) -
                                ts4conj::getTimeUnits(stats::start(b), frequency = stats::frequency(b))) * stats::frequency(a)
    if (!isTRUE(all.equal(temporalConsistence, round(temporalConsistence))))
        stop("Les objets a et b doivent être cohérents temporellement.")

    outputTS <- a

    if (is.raw(a)){
        a <- a |> as.integer() |> ts(start = stats::start(a), frequency = stats::frequency(a))
        b <- b |> as.integer() |> ts(start = stats::start(b), frequency = stats::frequency(b))
        outputTS <- ts4conj::combine2ts(a, b)
        outputTS <- outputTS |> as.raw() |> ts(start = stats::start(outputTS), frequency = stats::frequency(outputTS))

    } else if (isTRUE(all.equal(stats::frequency(a), round(stats::frequency(a))))){
        outputTS |>
            stats::window(start = stats::start(b), end = stats::end(b), extend = T) <- b

    } else if (is.numeric(stats::frequency(outputTS))){
        outputDF <- cbind(a, b) |> as.data.frame()
        if (sum(is.na(outputDF$a) & (!is.na(outputDF$b))) > 0) warning("extending time series when replacing values")

        outputDF$res <- outputDF$a
        outputDF$res[!is.na(outputDF$b)] <- outputDF$b[!is.na(outputDF$b)]

        outputTS <- ts(outputDF$res,
                       frequency = stats::frequency(a),
                       start = min(ts4conj::getTimeUnits(stats::start(a), frequency = stats::frequency(a)),
                                   ts4conj::getTimeUnits(stats::start(b), frequency = stats::frequency(b))))
    }
    return(outputTS)
}
