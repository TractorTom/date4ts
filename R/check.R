
#' Vérifie le format de date
#'
#' @description La fonction `assert_date_ts` vérifie qu'un objet est de type AAAA, c(AAAA, MM) ou c(AAAA, TT)
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
#' assert_date_ts(c(2020L, 8L))
#' assert_date_ts(c(2020L, 2L))
#' assert_date_ts(2022L)
#'
#' # Format double --> génération d'un warning
#' assert_date_ts(c(2020, 4))
#' assert_date_ts(2022)
#'
#' # Dépassement la fréquence--> génération d'un warning
#' assert_date_ts(c(2020L, 6L), frequency = 4L)
#' assert_date_ts(c(2020L, 42L), frequency = 12L)
#' assert_date_ts(c(2020L, -4L))
#'
#' # Mauvaise fréquence --> reponse FALSE
#' assert_date_ts(c(2020L, 7L))
#'
#' # Format non accepté --> reponse FALSE
#' assert_date_ts(2022.5)
#' assert_date_ts(2022 + 1/12)
#' assert_date_ts(2023 + 1/4)
#' assert_date_ts("2020-04-01")
#' assert_date_ts(as.Date("2020-04-01"))
assert_date_ts <- function(x, frequency, add = NULL, .var.name = checkmate::vname(x)) {

    if (is.null(add)) {
        coll <- checkmate::makeAssertCollection()
    } else {
        coll <- add
    }

    if (!isTRUE(checkmate::check_integer(x))) {
        warning(checkmate::check_integer(x))
    }

    # Check de la fréquence
    frequency <- assert_frequency(frequency,
                                  add = coll, .var.name = "frequency")
    # Check du type
    x <- checkmate::assert_integerish(x, coerce = TRUE, any.missing = FALSE,
                                      add = coll, .var.name = .var.name)
    # Check de la longueur
    checkmate::assert_true(length(x) %in% c(1L, 2L),
                           add = coll, .var.name = .var.name)

    if ((length(x) == 2L) && !isTRUE(checkmate::check_integerish(x[2L], lower = 1L, upper = frequency))) {
        warning(checkmate::check_integerish(x[2L], lower = 1L, upper = frequency))
        x <- format_date_ts(x, frequency, test = FALSE)
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
#' assert_ts(ts1)
#' assert_ts(ts2)
#' assert_ts(ts3)
#'
#' assert_ts(ts2, warn = FALSE)
#' assert_ts(ts3, warn = FALSE)
assert_ts <- function(x, add = NULL, .var.name = checkmate::vname(x)) {

    frequency <- stats::frequency(x)
    start_ts <- stats::start(x)
    end_ts <- stats::end(x)

    # Check de la fréquence
    checkmate::assert_count(frequency, .var.name = "frequency")
    frequency <- as.integer(frequency)
    # Check de la temporalité - start
    checkmate::assert_integerish(start_ts, .var.name = "start")
    start_ts <- as.integer(start_ts)
    # Check de la temporalité - end
    checkmate::assert_integerish(end_ts, .var.name = "end")
    end_ts <- as.integer(end_ts)

    if (is.null(add)) {
        coll <- checkmate::makeAssertCollection()
    } else {
        coll <- add
    }

    # Check de la fréquence
    frequency <- assert_frequency(frequency, add = coll, .var.name = "frequency")
    # Check de la temporalité
    start_ts <- assert_date_ts(start_ts, frequency = frequency, add = coll, .var.name = "start")
    end_ts <- assert_date_ts(end_ts, frequency = frequency, add = coll, .var.name = "end")
    # Check de la classe de l'objet
    checkmate::assert_class(x, classes = "ts", add = coll, .var.name = .var.name)
    checkmate::assert_false(stats::is.mts(x), add = coll, .var.name = .var.name)
    # Check du type de données
    checkmate::assert_atomic_vector(x, add = coll, .var.name = .var.name)

    if (is.null(add)) {
        checkmate::reportAssertions(coll)
    }

    return(invisible(x))
}

assert_TimeUnits <- function(x, frequency, add = NULL, .var.name = checkmate::vname(x)) {

    if (is.null(add)) {
        coll <- checkmate::makeAssertCollection()
    } else {
        coll <- add
    }

    # Check de la fréquence
    frequency <- assert_frequency(frequency, add = coll, .var.name = "frequency")
    # Check de l'objet x (TimeUnits)
    checkmate::assert_number(x, add = coll, .var.name = .var.name, finite = TRUE)
    checkmate::assert_int(x * frequency, add = coll, .var.name = .var.name)

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

    if (!isTRUE(checkmate::check_integer(x))) {
        warning(checkmate::check_integer(x))
    }

    x <- checkmate::assert_int(x, coerce = TRUE, add = coll, .var.name = .var.name)
    checkmate::assert_choice(x, choices = c(4L, 12L), add = coll, .var.name = .var.name)

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

    if (!isTRUE(checkmate::check_integer(x))) {
        warning(checkmate::check_integer(x))
    }

    x <- checkmate::assert_int(x, coerce = TRUE, add = coll, .var.name = .var.name)

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

    if (!isTRUE(checkmate::check_integer(x))) {
        warning(checkmate::check_integer(x))
    }

    x <- checkmate::assert_count(x, coerce = TRUE,
                                 add = coll, .var.name = .var.name)

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
