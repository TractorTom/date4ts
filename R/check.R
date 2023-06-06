
#' Vérifie le format de date
#'
#' @description La fonction `is_date_ts` vérifie qu'un objet est de type AAAA, c(AAAA, MM) ou c(AAAA, TT)
#' @param date_ts un vecteur numérique, de préférence integer au format AAAA, c(AAAA, MM) ou c(AAAA, TT)
#' @param frequency un entier qui vaut 4L (ou 4.) pour les séries trimestrielles et 12L (ou 12.) pour les séries mensuelles.
#' @param warn un booléen
#'
#' @return En sortie la fonction retourne un booleen et un warning additionnel si besoin.
#' @details Les fonctions du package ts4conj sont faites pour fonctionner avec des times-series de fréquence mensuelle ou trimestrielle et basés sur le système des mois, trimestres et années classiques.
#' On cherche donc à favoriser l'utilisation de vecteur c(AAAA, MM) pour désigner la date choisie.
#' @export
#'
#' @examples
#' # De bons formats de date
#' is_date_ts(c(2020L, 8L))
#' is_date_ts(c(2020L, 2L))
#' is_date_ts(2022L)
#'
#' # Format double --> génération d'un warning
#' is_date_ts(c(2020, 4))
#' is_date_ts(2022)
#'
#' # Dépassement la fréquence--> génération d'un warning
#' is_date_ts(c(2020L, 6L), frequency = 4L)
#' is_date_ts(c(2020L, 42L), frequency = 12L)
#' is_date_ts(c(2020L, -4L))
#'
#' # Mauvaise fréquence --> reponse FALSE
#' is_date_ts(c(2020L, 7L))
#'
#' # Format non accepté --> reponse FALSE
#' is_date_ts(2022.5)
#' is_date_ts(2022 + 1/12)
#' is_date_ts(2023 + 1/4)
#' is_date_ts("2020-04-01")
#' is_date_ts(as.Date("2020-04-01"))
assert_date_ts <- function(x, frequency, add = NULL, .var.name = checkmate::vname(x)) {

    if (is.null(add)) {
        coll <- checkmate::makeAssertCollection()
    } else {
        coll <- add
    }

    # Check de la fréquence
    assert_frequency(frequency, add = coll, .var.name = "frequency")
    # Check du type
    checkmate::assert_integerish(x, add = coll, any.missing = FALSE)
    # Check de la longueur
    checkmate::assert_true(length(x) %in% c(1L, 2L), add = coll)

    if (!isTRUE(checkmate::check_integer(x))) {
        warning(checkmate::check_integer(x))
    }

    if (length(x) == 2L && !isTRUE(checkmate::check_integerish(x[2L], lower = 1L, upper = frequency))) {
        warning(checkmate::check_integerish(x[2L], lower = 1L, upper = frequency))
    }

    if (is.null(add)) {
        checkmate::reportAssertions(coll)
    }

    return(invisible(x))
}

#' Vérifie la conformité d'un objet ts dans le cadre des enquêtes de conjoncture
#'
#' @param dataTS un objet ts unidimensionnel
#' @param warn un booléen
#'
#' @return En sortie la fonction retourne un booleen qui précise si l'argument `dataTS` est conforme ou non.
#' Dans le cas où warn vaut TRUE et que le TS n'est pas conforme, un warning qui précise la raison sera déclenché.
#' @details Les fonctions du package ts4conj sont faites pour fonctionner avec des times-series de fréquence mensuelle ou trimestrielle et basés sur le système des mois, trimestres et années classiques. On travaille avec des données numériques (integer, double ou logical) mais les autres types atomic sont acceptés également.
#' @export
#'
#' @examples
#' ts1 <- ts(1:100, start = 2010L, frequency = 12L)
#' ts2 <- ts(1:100, start = 2010 + 1/7, frequency = 12L)
#' ts3 <- ts(1:100, start = 2010L, frequency = 1L)
#'
#' isGoodTS(ts1)
#' isGoodTS(ts2)
#' isGoodTS(ts3)
#'
#' isGoodTS(ts2, warn = FALSE)
#' isGoodTS(ts3, warn = FALSE)
isGoodTS <- function(dataTS, warn = TRUE) {

    # Check de warn
    checkmate::assert_flag(warn)

    # Check du type d'objet
    if (!stats::is.ts(dataTS) | stats::is.mts(dataTS)) {
        if (warn) warning("L'objet dataTS doit \u00eatre un ts unidimensionnel.")
        return(FALSE)
    }

    # Check de la fréquence
    assert_frequency(frequency, .var.name = "frequency")

    # Check de la temporalité
    if (withCallingHandlers({
        !is_date_ts(stats::start(dataTS), frequency = stats::frequency(dataTS)) |
            !is_date_ts(stats::end(dataTS), frequency = stats::frequency(dataTS))},
        warning = function(w) {
            if (w$message == "La date est de type double. Il faut privil\u00e9gier le format integer.") invokeRestart("muffleWarning")
        })
    ) {
        if (warn) warning("L'objet dataTS doit \u00eatre coh\u00e9rent avec la temporalit\u00e9 classique.")
        return(FALSE)
    }
    # Check du type des données
    if (!is.atomic(dataTS)) {
        if (warn) warning("L'objet dataTS doit \u00eatre d'un type atomic.")
        return(FALSE)
    }

    return(TRUE)
}

assert_TimeUnits <- function(x, add = NULL, .var.name = checkmate::vname(x)) {

    if (is.null(add)) {
        coll <- checkmate::makeAssertCollection()
    } else {
        coll <- add
    }

    checkmate::assert_number(x, add = coll, .var.name = .var.name, finite = TRUE)
    checkmate::assert_int(x * 12L, add = coll, .var.name = .var.name)

    if (is.null(add)) {
        checkmate::reportAssertions(coll)
    }

    return(invisible(x))
}

assert_frequency <- function(x, add = NULL, .var.name = checkmate::vname(x)) {

    if (is.null(add)) {
        coll <- checkmate::makeAssertCollection()
    } else {
        coll <- add
    }

    checkmate::assert_int(x, add = coll, .var.name = .var.name)
    checkmate::assert_true(x %in% c(4L, 12L), add = coll, .var.name = .var.name)

    if (!isTRUE(checkmate::check_integer(x))) {
        warning(checkmate::check_integer(x))
    }

    if (is.null(add)) {
        checkmate::reportAssertions(coll)
    }

    return(invisible(x))
}

assert_scalar_integer <- function(x, add = NULL, .var.name = checkmate::vname(x)) {

    if (is.null(add)) {
        coll <- checkmate::makeAssertCollection()
    } else {
        coll <- add
    }

    checkmate::assert_int(x, add = coll, .var.name = .var.name)

    if (!isTRUE(checkmate::check_integer(x))) {
        warning(checkmate::check_integer(x))
    }

    if (is.null(add)) {
        checkmate::reportAssertions(coll)
    }

    return(invisible(x))
}

assert_scalar_natural <- function(x, add = NULL, .var.name = checkmate::vname(x)) {

    if (is.null(add)) {
        coll <- checkmate::makeAssertCollection()
    } else {
        coll <- add
    }

    checkmate::assert_count(x, add = coll, .var.name = .var.name)

    if (!isTRUE(checkmate::check_integer(x))) {
        warning(checkmate::check_integer(x))
    }

    if (is.null(add)) {
        checkmate::reportAssertions(coll)
    }

    return(invisible(x))
}

assert_scalar_date <- function(x, add = NULL, .var.name = checkmate::vname(x)) {

    if (is.null(add)) {
        coll <- checkmate::makeAssertCollection()
    } else {
        coll <- add
    }
    checkmate::assert_date(x, add = coll, .var.name = .var.name)
    checkmate::assert_scalar(x, add = coll, .var.name = .var.name)

    if (is.null(add)) {
        checkmate::reportAssertions(coll)
    }

    return(invisible(x))
}
