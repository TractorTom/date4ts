
#' Vérifie le format de date
#'
#' @description La fonction `assert_date_ts` vérifie qu'un objet est de type AAAA, c(AAAA, MM) ou c(AAAA, TT)
#' @param x un vecteur numérique, de préférence integer au format AAAA, c(AAAA, MM) ou c(AAAA, TT)
#' @param frequency un entier qui vaut 4L (ou 4.) pour les séries trimestrielles et 12L (ou 12.) pour les séries mensuelles.
#' @param add Collection pour stocker les messages d'erreurs (Default is NULL)
#' @param .var.name Nom de l'objet à vérifier pour afficher dans les messages
#'
#' @return En sortie la fonction retourne l'objet de manière invisible ou une erreur.
#' @details Les fonctions du package ts4conj sont faites pour fonctionner avec des times-series de fréquence mensuelle ou trimestrielle et basés sur le système des mois, trimestres et années classiques.
#' On cherche donc à favoriser l'utilisation de vecteur c(AAAA, MM) pour désigner la date choisie.
#' Lorsque l'objet `x` en entrée est au mauvais format, il est corrigé pendant la vérification et l'objet en sortie est au bon format.
#' @export
#'
#' @examples
#' # De bons formats de date
#' assert_date_ts(c(2020L, 8L), frequency = 12L)
#' assert_date_ts(c(2020L, 2L), frequency = 4L)
#' assert_date_ts(2022L, frequency = 12L)
#'
#' # Format double --> génération d'un warning
#' assert_date_ts(c(2020., 4.), frequency = 4L)
#' assert_date_ts(2022., frequency = 12L)
#'
#' # Fréquence au format double --> génération d'un warning
#' assert_date_ts(c(2020L, 6L), frequency = 4.)
#' assert_date_ts(c(2020L, 42L), frequency = 12.)
#'
#' # Dépassement la fréquence --> génération d'un warning
#' assert_date_ts(c(2020L, 6L), frequency = 4L)
#' assert_date_ts(c(2020L, 42L), frequency = 12L)
#' assert_date_ts(c(2020L, -4L), frequency = 12L)
#'
assert_date_ts <- function(x, frequency, add = NULL, .var.name = checkmate::vname(x)) {

    if (is.null(add)) {
        coll <- checkmate::makeAssertCollection()
    } else {
        coll <- add
    }

    if (!isTRUE(checkmate::check_integer(x))) {
        err <- try(checkmate::assert_integer(x, .var.name = .var.name), silent = TRUE)
        warning(attr(err, "condition")$message)
    }

    # Check de la fréquence
    frequency <- assert_frequency(frequency,
                                  add = coll, .var.name = "frequency")
    # Check du type
    x <- checkmate::assert_integerish(x, coerce = TRUE, any.missing = FALSE,
                                      add = coll, .var.name = .var.name)
    # Check de la longueur
    checkmate::assert_choice(
        x = length(x), choices = c(1L, 2L),
        add = coll, .var.name = paste0("length(", .var.name, ")"))

    if ((length(x) == 2L) && !isTRUE(checkmate::check_integerish(x[2L], lower = 1L, upper = frequency))) {
        err <- try(checkmate::assert_integerish(x[2L], lower = 1L, upper = frequency, .var.name = "period"), silent = TRUE)
        warning(attr(err, "condition")$message)
    }

    x <- format_date_ts(x, frequency, test = FALSE)

    if (is.null(add)) {
        checkmate::reportAssertions(coll)
    }

    return(invisible(x))
}

#' Vérifie la conformité d'un objet ts dans le cadre des enquêtes de conjoncture
#'
#' @param x un objet ts unidimensionnel
#' @param add Collection pour stocker les messages d'erreurs (Default is NULL)
#' @param .var.name Nom de l'objet à vérifier pour afficher dans les messages
#'
#' @return En sortie la fonction retourne un booleen qui précise si l'argument `dataTS` est conforme ou non.
#' @details Les fonctions du package ts4conj sont faites pour fonctionner avec des times-series de fréquence mensuelle ou trimestrielle et basés sur le système des mois, trimestres et années classiques. On travaille avec des données numériques (integer, double ou logical) mais les autres types atomic sont acceptés également.
#' @export
#'
#' @examples
#' ts1 <- ts(1:100, start = 2010L, frequency = 12L)
#' ts2 <- ts(1:10, start = c(2020L, 4L), frequency = 4L)
#'
#' assert_ts(ts1)
#' assert_ts(ts2)
#'
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

#' @export
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

#' @export
assert_frequency <- function(x, add = NULL, .var.name = checkmate::vname(x)) {

    if (is.null(add)) {
        coll <- checkmate::makeAssertCollection()
    } else {
        coll <- add
    }

    if (!isTRUE(checkmate::check_integer(x))) {
        err <- try(checkmate::assert_integer(x, .var.name = .var.name), silent = TRUE)
        warning(attr(err, "condition")$message)
    }

    x <- checkmate::assert_int(x, coerce = TRUE, add = coll, .var.name = .var.name)
    checkmate::assert_choice(x, choices = c(4L, 12L), add = coll, .var.name = .var.name)

    if (is.null(add)) {
        checkmate::reportAssertions(coll)
    }

    return(invisible(x))
}

#' @export
assert_scalar_integer <- function(x, add = NULL, .var.name = checkmate::vname(x)) {

    if (is.null(add)) {
        coll <- checkmate::makeAssertCollection()
    } else {
        coll <- add
    }

    if (!isTRUE(checkmate::check_integer(x))) {
        err <- try(checkmate::assert_integer(x, .var.name = .var.name), silent = TRUE)
        warning(attr(err, "condition")$message)
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
        err <- try(checkmate::assert_integer(x, .var.name = .var.name), silent = TRUE)
        warning(attr(err, "condition")$message)
    }

    x <- checkmate::assert_count(x, coerce = TRUE,
                                 add = coll, .var.name = .var.name)

    if (is.null(add)) {
        checkmate::reportAssertions(coll)
    }

    return(invisible(x))
}

#' @export
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
