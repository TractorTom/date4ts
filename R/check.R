
#' Vérifie le format de date
#'
#' @description La fonction `assert_date_ts` vérifie qu'un objet est de type AAAA, c(AAAA, MM) ou c(AAAA, TT)
#'
#' @param x un vecteur numérique, de préférence `integer` au format AAAA, c(AAAA, MM) ou c(AAAA, TT)
#' @param frequency_ts un entier qui vaut 4L (ou 4.) pour les séries trimestrielles et 12L (ou 12.) pour les séries mensuelles.
#' @param add Collection pour stocker les messages d'erreurs (Default is NULL)
#' @param .var.name Nom de l'objet à vérifier pour afficher dans les messages
#' @param warn un booleen
#'
#' @return En sortie la fonction retourne l'objet de manière invisible ou une erreur.
#' @details Les fonctions du package ts4conj sont faites pour fonctionner avec des times-series de fréquence mensuelle ou trimestrielle et basés sur le système des mois, trimestres et années classiques.
#' On cherche donc à favoriser l'utilisation de vecteur c(AAAA, MM) pour désigner la date choisie.
#' Lorsque l'objet `x` en entrée est au mauvais format, il est corrigé pendant la vérification et l'objet en sortie est au bon format.
#' Si l'argument `warn` est `FALSE`, alors la fonction ne retournera pas de warning lors de l'évaluation.
#'
#' @export
#'
#' @examples
#' # De bons formats de date
#' assert_date_ts(c(2020L, 8L), frequency_ts = 12L)
#' assert_date_ts(c(2020L, 2L), frequency_ts = 4L)
#' assert_date_ts(2022L, frequency_ts = 12L)
#'
#' # Format double --> génération d'un warning
#' assert_date_ts(c(2020., 4.), frequency_ts = 4L)
#' assert_date_ts(2022., frequency_ts = 12L)
#'
#' # Fréquence au format double --> génération d'un warning
#' assert_date_ts(c(2020L, 6L), frequency_ts = 4.)
#' assert_date_ts(c(2020L, 42L), frequency_ts = 12.)
#'
#' # Dépassement la fréquence --> génération d'un warning
#' assert_date_ts(c(2020L, 6L), frequency_ts = 4L)
#' assert_date_ts(c(2020L, 42L), frequency_ts = 12L)
#' assert_date_ts(c(2020L, -4L), frequency_ts = 12L)
#'
assert_date_ts <- function(x, frequency_ts, add = NULL, .var.name = checkmate::vname(x), warn = TRUE) {

    if (is.null(add)) {
        coll <- checkmate::makeAssertCollection()
    } else {
        coll <- add
    }

    # Check de la fréquence
    frequency_ts <- assert_frequency(frequency_ts,
                                     add = coll, .var.name = "frequency_ts", warn = warn)


    # Check du type
    if (isTRUE(checkmate::check_numeric(x, any.missing = FALSE,
                                        min.len = 1L, max.len = 2L, finite = TRUE))) {
        x_corr <- checkmate::assert_integerish(x, coerce = TRUE, any.missing = FALSE,
                                               add = coll, .var.name = .var.name)
    } else {
        checkmate::assert_numeric(x, any.missing = FALSE,
                                  add = coll, .var.name = .var.name,
                                  min.len = 1L, max.len = 2L, finite = TRUE)
    }

    if (is.null(add)) {
        checkmate::reportAssertions(coll)
    }

    if (warn && (!isTRUE(checkmate::check_integer(x)))) {
        err <- try(checkmate::assert_integer(x, .var.name = .var.name), silent = TRUE)
        warning(attr(err, "condition")$message)
    }

    if (warn && (length(x) == 2L) && !isTRUE(checkmate::check_integerish(x[2L], lower = 1L, upper = frequency_ts))) {
        err <- try(checkmate::assert_integerish(x[2L], lower = 1L, upper = frequency_ts, .var.name = "period"), silent = TRUE)
        warning(attr(err, "condition")$message)
    }

    x <- format_date_ts(x_corr, frequency_ts, test = FALSE)

    return(invisible(x))
}

#' Vérifie la conformité d'un objet ts
#'
#' @description La fonction `assert_ts` vérifie qu'un objet ts est bien conforme.
#'
#' @param x un objet ts unidimensionnel
#' @param add Collection pour stocker les messages d'erreurs (Default is NULL)
#' @param .var.name Nom de l'objet à vérifier pour afficher dans les messages
#'
#' @return En sortie la fonction retourne l'objet `x` de manière invisible ou une erreur.
#' @details Les fonctions du package ts4conj sont faites pour fonctionner avec des times-series de fréquence mensuelle ou trimestrielle et basées sur le système des mois, trimestres et années classiques.
#' On travaille avec des données numériques (integer, double ou logical) mais les autres types atomic sont acceptés également.
#' On cherche donc à favoriser l'utilisation de séries temporelles classiques utilisants des types atomiques.
#' Lorsque l'objet `x` en entrée est au mauvais format, une erreur est généré.
#'
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

    if (is.null(add)) {
        coll <- checkmate::makeAssertCollection()
    } else {
        coll <- add
    }

    # Check de la fréquence
    frequency_ts <- stats::frequency(x)
    frequency_ts <- assert_frequency(frequency_ts, add = coll, .var.name = "frequency_ts", warn = FALSE)
    # Check de la temporalité
    start_ts <- stats::start(x)
    start_ts <- assert_date_ts(start_ts, frequency_ts = frequency_ts, add = coll, .var.name = "start", warn = FALSE)

    end_ts <- stats::end(x)
    end_ts <- assert_date_ts(end_ts, frequency_ts = frequency_ts, add = coll, .var.name = "end", warn = FALSE)
    # Check de la classe de l'objet
    checkmate::assert_class(x, classes = "ts", add = coll, .var.name = .var.name)
    checkmate::assert_false(stats::is.mts(x), add = coll, .var.name = paste0("is.mts(", .var.name, ")"))
    # Check du type de données
    checkmate::assert_atomic_vector(x, add = coll, .var.name = .var.name)

    if (is.null(add)) {
        checkmate::reportAssertions(coll)
    }

    return(invisible(x))
}

#' Vérifie la conformité d'un objet TimeUnits
#'
#' @description La fonction `assert_TimeUnits` vérifie qu'un objet est un TimeUnits.
#'
#' @param x un numérique qui représente le time units de
#' @param frequency_ts un entier qui vaut 4L (ou 4.) pour les séries trimestrielles et 12L (ou 12.) pour les séries mensuelles.
#' @param add Collection pour stocker les messages d'erreurs (Default is NULL)
#' @param .var.name Nom de l'objet à vérifier pour afficher dans les messages
#'
#' @return En sortie la fonction retourne l'objet `x` de manière invisible ou une erreur.
#'
#' @details Un objet de type TimeUnits est un numérique qui désigne l'année et la période en cours avec ses décimales.
#' Ainsi pour une série temporelle mensuelle, `2020.5` représente la moitié de l'année donc juillet 2020 et s'écrit `c(2020L, 7L)` au format date_ts.
#' @export
#'
#' @examples
#'
#' assert_TimeUnits(2020.5, frequency_ts = 12L)
#' assert_TimeUnits(2020.5, frequency_ts = 4L)
#' assert_TimeUnits(2023, frequency_ts = 12L)
#'
#' assert_TimeUnits(2000 + 5/12, frequency_ts = 12L)
#' assert_TimeUnits(2015 + 3/4, frequency_ts = 4L)
#'
assert_TimeUnits <- function(x, frequency_ts, add = NULL, .var.name = checkmate::vname(x)) {

    if (is.null(add)) {
        coll <- checkmate::makeAssertCollection()
    } else {
        coll <- add
    }

    # Check de la fréquence
    frequency_ts <- assert_frequency(frequency_ts, add = coll, .var.name = "frequency_ts")
    # Check de l'objet x (TimeUnits)
    checkmate::assert_number(x, add = coll, .var.name = .var.name, finite = TRUE)

    if (is.null(add)) {
        checkmate::reportAssertions(coll)
    }

    checkmate::assert_int(x * frequency_ts, .var.name = .var.name)

    return(invisible(x))
}

#' Vérifie la conformité d'une fréquence
#'
#' @param x un entier qui vaut 4L (ou 4.) pour les séries trimestrielles et 12L (ou 12.) pour les séries mensuelles.
#' @param add Collection pour stocker les messages d'erreurs (Default is NULL)
#' @param .var.name Nom de l'objet à vérifier pour afficher dans les messages
#' @param warn un booleen
#'
#' @return En sortie la fonction retourne l'objet `x` de manière invisible ou une erreur.
#'
#' @details La fréquence d'une série temporelle est soit mensuelle (`12L` ou `12.`) soit trimestrielle (`4L` ou `4.`). Les autres fréquences ne sont pas acceptées.
#' Cette fonction s'appuie essentiellement sur les fonctions `checkmate::assert_int` et `checkmate::assert_choice`.
#' Il y a néanmoins une petite subtilité : on vérifie si l'objet `x` est de type double ou integer.
#' Dans le premier cas, on affichera un warning et on corrigera l'objet au format integer pour les traitements ultérieurs. En sortie, `x` est retourné de manière invisible.
#' Si l'argument `warn` est `FALSE`, alors la fonction ne retournera pas de warning lors de l'évaluation.
#'
#' @export
#'
#' @examples
#'
#' assert_frequency(4L)
#' assert_frequency(12L)
#'
assert_frequency <- function(x, add = NULL, .var.name = checkmate::vname(x), warn = TRUE) {

    if (is.null(add)) {
        coll <- checkmate::makeAssertCollection()
    } else {
        coll <- add
    }

    x_corr <- checkmate::assert_int(x, coerce = TRUE, add = coll, .var.name = .var.name)
    checkmate::assert_choice(x_corr, choices = c(4L, 12L), add = coll, .var.name = .var.name)

    if (is.null(add)) {
        checkmate::reportAssertions(coll)
    }

    if (warn && (!isTRUE(checkmate::check_integer(x)))) {
        err <- try(checkmate::assert_integer(x, .var.name = .var.name), silent = TRUE)
        warning(attr(err, "condition")$message)
    }
    x <- x_corr

    return(invisible(x))
}


#' Vérifie la conformité d'un entier scalaire
#'
#' @param x un entier
#' @param add Collection pour stocker les messages d'erreurs (Default is NULL)
#' @param .var.name Nom de l'objet à vérifier pour afficher dans les messages
#' @param warn un booleen
#'
#' @return En sortie la fonction retourne l'objet `x` de manière invisible ou une erreur.
#'
#' @details On vérifie que l'objet `x` en entrée est bien un entier.
#' Cette fonction s'appuie essentiellement sur la fonction `checkmate::assert_int`.
#' Il y a néanmoins une petite subtilité : on vérifie si l'objet `x` est de type double ou integer.
#' Dans le premier cas, on affichera un warning et on corrigera l'objet au format integer pour les traitements ultérieurs. En sortie, `x` est retourné de manière invisible.
#' Si l'argument `warn` est `FALSE`, alors la fonction ne retournera pas de warning lors de l'évaluation.
#'
#' @export
#'
#' @seealso [assert_scalar_natural()]
#'
#' @examples
#'
#' assert_scalar_integer(1L)
#' assert_scalar_integer(100L)
#' assert_scalar_integer(-4L)
#' assert_scalar_integer(0L)
#'
assert_scalar_integer <- function(x, add = NULL, .var.name = checkmate::vname(x), warn = TRUE) {

    if (is.null(add)) {
        coll <- checkmate::makeAssertCollection()
    } else {
        coll <- add
    }

    if (warn && !isTRUE(checkmate::check_integer(x))) {
        err <- try(checkmate::assert_integer(x, .var.name = .var.name), silent = TRUE)
        warning(attr(err, "condition")$message)
    }

    x <- checkmate::assert_int(x, coerce = TRUE, add = coll, .var.name = .var.name)

    if (is.null(add)) {
        checkmate::reportAssertions(coll)
    }

    return(invisible(x))
}

#' Vérifie la conformité d'un entier naturel
#'
#' @param x un entier naturel (strictement positif)
#' @param add Collection pour stocker les messages d'erreurs (Default is NULL)
#' @param .var.name Nom de l'objet à vérifier pour afficher dans les messages
#' @param warn un booleen
#'
#' @return En sortie la fonction retourne l'objet `x` de manière invisible ou une erreur.
#'
#' @details Cette fonction s'appuie essentiellement sur la fonction `checkmate::assert_count`.
#' Il y a néanmoins une petite subtilité : on vérifie si l'objet `x` est de type double ou integer.
#' Dans le premier cas, on affichera un warning et on corrigera l'objet au format integer pour les traitements ultérieurs. En sortie, `x` est retourné de manière invisible.
#' Si l'argument `warn` est `FALSE`, alors la fonction ne retournera pas de warning lors de l'évaluation.
#'
#' @export
#'
#' @seealso [assert_scalar_integer()]
#'
#' @examples
#'
#' # Avec des entier integer
#' assert_scalar_natural(1L)
#' assert_scalar_natural(100L)
#'
#' # Avec des entiers double
#' assert_scalar_natural(2.)
#' assert_scalar_natural(457)
#'
assert_scalar_natural <- function(x, add = NULL, .var.name = checkmate::vname(x), warn = TRUE) {

    if (is.null(add)) {
        coll <- checkmate::makeAssertCollection()
    } else {
        coll <- add
    }

    x_corr <- checkmate::assert_count(x, coerce = TRUE, positive = TRUE,
                                 add = coll, .var.name = .var.name)

    if (is.null(add)) {
        checkmate::reportAssertions(coll)
    }

    if (warn && !isTRUE(checkmate::check_integer(x))) {
        err <- try(checkmate::assert_integer(x, .var.name = .var.name), silent = TRUE)
        warning(attr(err, "condition")$message)
    }
    x <- x_corr

    return(invisible(x))
}

#' Vérifie la conformité d'une date scalaire
#'
#' @param x un objet de type `Date`.
#' @param add Collection pour stocker les messages d'erreurs (Default is NULL)
#' @param .var.name Nom de l'objet à vérifier pour afficher dans les messages
#'
#' @return En sortie la fonction retourne l'objet `x` de manière invisible ou une erreur.
#'
#' @details On vérifie que l'objet `x` en entrée est bien au format `Date` et qu'il s'agit d'un scalaire (vecteur de taille 1).
#' Cette fonction s'appuie essentiellement sur les fonctions `checkmate::assert_date` et `checkmate::assert_scalar`.
#'
#' @export
#'
#' @examples
#'
#' assert_scalar_date(as.Date("2018-01-24"))
#' assert_scalar_date(as.Date("2000-02-29"))
#' assert_scalar_date(Sys.Date())
#'
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
