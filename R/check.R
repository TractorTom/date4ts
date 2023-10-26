#' Vérifie le format de date
#'
#' @description La fonction `assert_date_ts` vérifie qu'un objet est de type `AAAA`, `c(AAAA, MM)` ou `c(AAAA, TT)`
#'
#' @param x un vecteur numérique, de préférence `integer` au format `AAAA`, `c(AAAA, MM)` ou `c(AAAA, TT)`
#' @param frequency_ts un entier qui vaut `4L` (ou `4.`) pour les séries trimestrielles et `12L` (ou `12.`) pour les séries mensuelles.
#' @param add Collection pour stocker les messages d'erreurs (Default is NULL)
#' @param .var.name Nom de l'objet à vérifier pour afficher dans les messages
#' @param warn un booleen
#'
#' @return En sortie la fonction retourne l'objet `x` de manière invisible ou une erreur.
#' @details Les fonctions du package TractorTsbox sont faites pour fonctionner avec des times-series de fréquence mensuelle ou trimestrielle et basés sur le système des mois, trimestres et années classiques.
#' On cherche donc à favoriser l'utilisation de vecteur `c(AAAA, MM)` pour désigner la date choisie.
#' Lorsque l'objet `x` en entrée est au mauvais format, il est corrigé pendant la vérification et l'objet en sortie est au bon format.
#' Si l'argument `warn` est `FALSE`, alors la fonction ne retournera pas de warning lors de l'évaluation.
#'
#' Ici, l'argument `frequency_ts` est nécessaire car une date sous la forme c(AAAA, PP), avec PP le nombre de période, ne désigne pas une date absolue. Par exemple, c(2020L 5L) désigne mai 2020 pour une fréquence mensuelle et le 1er trimestre 2021 pour une fréquence trimestrielle.
#'
#' Selon le préfixe de la fonction :
#'
#'  - si le check réussi :
#'      - la fonction `assert_date_ts` retourne l'objet `x` de manière invisible;
#'      - la fonction `check_date_ts` retourne le booléen `TRUE`.
#'
#'  - si le check échoue :
#'      - la fonction `assert_date_ts` retourne un message d'erreur;
#'      - la fonction `check_date_ts` retourne une chaîne de caractère signalant le problème.
#'
#' @export
#'
#' @examples
#' # De bons formats de date
#' assert_date_ts(c(2020L, 8L), frequency_ts = 12L)
#' assert_date_ts(c(2020L, 2L), frequency_ts = 4L)
#' check_date_ts(2022L, frequency_ts = 12L)
#'
#' # Format double --> génération d'un warning
#' assert_date_ts(c(2020., 4.), frequency_ts = 4L)
#' assert_date_ts(2022., frequency_ts = 12L)
#' check_date_ts(2022., frequency_ts = 12L)
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
#' # Avec des erreurs
#' check_date_ts(1:10, frequency_ts = 12L)
#'
check_date_ts <- function(x, frequency_ts, .var.name = checkmate::vname(x), warn = TRUE) {

    verif <- TRUE
    output <- c()

    # Check de la fréquence
    cond1 <- check_frequency(frequency_ts, warn = warn, .var.name = "frequency_ts")
    if (!isTRUE(cond1)) {
        verif <- FALSE
        output <- output <- c(output, cond1)
    }

    # Check du type
    cond2 <- checkmate::check_numeric(
        x, any.missing = FALSE,
        min.len = 1L, max.len = 2L, finite = TRUE
    )

    if (isTRUE(cond2)) {
        cond3 <- checkmate::check_integerish(x, any.missing = FALSE)
        if (!isTRUE(cond3)) {
            verif <- FALSE
            output <- output <- c(output, paste("*", .var.name, cond3))
        }
    } else {
        verif <- FALSE
        output <- output <- c(output, paste("*", .var.name, cond2))
    }

    if (warn && (!isTRUE(checkmate::check_integer(x)))) {
        err <- try(checkmate::assert_integer(x, .var.name = .var.name), silent = TRUE)
        warning(attr(err, "condition")$message)
    }

    if (isTRUE(cond1) && isTRUE(cond2) && isTRUE(cond3) && warn && (length(x) == 2L) && !isTRUE(checkmate::check_integerish(x[2L], lower = 1L, upper = frequency_ts))) {
        err <- try(checkmate::assert_integerish(x[2L], lower = 1L, upper = frequency_ts, .var.name = "period"), silent = TRUE)
        warning(attr(err, "condition")$message)
    }

    output <- paste(output, collapse = "\n")

    return(ifelse(verif, verif, output))
}

#' @name check_date_ts
#' @export
#'
assert_date_ts <- function(x, frequency_ts, add = NULL, .var.name = checkmate::vname(x), warn = TRUE) {

    if (is.null(add)) {
        coll <- checkmate::makeAssertCollection()
    } else {
        coll <- add
    }

    # Check de la fréquence
    frequency_ts <- assert_frequency(
        frequency_ts,
        add = coll, .var.name = "frequency_ts", warn = warn
    )

    # Check du type
    # Ici on passe d'abord par un check car il y a une génération de warning non voulue sinon...
    if (isTRUE(checkmate::check_numeric(
        x, any.missing = FALSE,
        min.len = 1L, max.len = 2L, finite = TRUE
    )
    )) {

        x_corr <- checkmate::assert_integerish(
            x,
            coerce = TRUE, any.missing = FALSE,
            add = coll, .var.name = .var.name
        )
    } else {
        checkmate::assert_numeric(
            x, any.missing = FALSE,
            add = coll, .var.name = .var.name,
            min.len = 1L, max.len = 2L, finite = TRUE
        )
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
#' @description Les fonctions `assert_ts` et `check_ts` vérifient qu'un objet ts est bien conforme.
#'
#' @param x un objet ts unidimensionnel
#' @param add Collection pour stocker les messages d'erreurs (Default is NULL)
#' @param .var.name Nom de l'objet à vérifier pour afficher dans les messages
#'
#' @return En sortie la fonction retourne l'objet `x` de manière invisible ou une erreur.
#' @details Les fonctions du package TractorTsbox sont faites pour fonctionner avec des times-series de fréquence mensuelle ou trimestrielle et basées sur le système des mois, trimestres et années classiques.
#' On travaille avec des données numériques (integer, double ou logical) mais les autres types atomic sont acceptés également.
#' On cherche donc à favoriser l'utilisation de séries temporelles classiques utilisants des types atomiques.
#' Lorsque l'objet `x` en entrée est au mauvais format, une erreur est généré.
#'
#' Selon le préfixe de la fonction :
#'
#'  - si le check réussi :
#'      - la fonction `assert_ts` retourne l'objet `x` de manière invisible;
#'      - la fonction `check_ts` retourne le booléen `TRUE`.
#'
#'  - si le check échoue :
#'      - la fonction `assert_ts` retourne un message d'erreur;
#'      - la fonction `check_ts` retourne une chaîne de caractère signalant le problème.
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
#' check_ts(ts1)
#' check_ts(ts2)
#'
#' # Exemples avec des erreurs
#'
#' check_ts(1)
#' check_ts(ts(1:10, start = 2010L, frequency = 2L))
#' check_ts(1:10)
#'
check_ts <- function(x, .var.name = checkmate::vname(x)) {

    verif <- TRUE
    output <- c()

    # Check de la classe de l'objet
    cond1 <- checkmate::check_class(x, classes = "ts", null.ok = FALSE)

    if (isTRUE(cond1)) {

        # Check de la fréquence
        cond2 <- check_expression({
            stats::frequency(x)
        })

        if (isTRUE(cond2)) {
            frequency_ts <- stats::frequency(x)
            cond3 <- check_frequency(frequency_ts, warn = FALSE, .var.name = "frequency_ts")

            if (!isTRUE(cond3)) {
                verif <- FALSE
                output <- c(output, cond3)

            } else {

                # Check de la temporalité - start
                cond4 <- check_expression(expr = {
                    stats::start(x)
                })

                if (isTRUE(cond4)) {
                    start_ts <- stats::start(x)
                    cond5 <- check_date_ts(start_ts, frequency_ts = frequency_ts,  warn = FALSE, .var.name = paste0("stats::start(", .var.name, ")"))

                    if (!isTRUE(cond5)) {
                        verif <- FALSE
                        output <- c(output, cond5)
                    }

                } else {
                    verif <- FALSE
                    output <- c(output, cond4)
                }

                # Check de la temporalité - end
                cond6 <- check_expression(expr = {
                    stats::end(x)
                })

                if (isTRUE(cond6)) {
                    end_ts <- stats::end(x)
                    cond7 <- check_date_ts(end_ts, frequency_ts = frequency_ts,  warn = FALSE, .var.name = paste0("stats::end(", .var.name, ")"))

                    if (!isTRUE(cond7)) {
                        verif <- FALSE
                        output <- c(output, cond7)
                    }

                } else {
                    verif <- FALSE
                    output <- c(output, cond6)
                }
            }

        } else {
            verif <- FALSE
            output <- c(output, cond2)
        }

        # Check que l'objet ne soit pas un mts
        cond8 <- checkmate::check_false(stats::is.mts(x))
        if (!isTRUE(cond8)) {
            verif <- FALSE
            output <- c(output, paste0("* stats::is.mts(", .var.name, ") ", cond8))
        }

        # Check du type de données
        cond9 <- checkmate::check_atomic_vector(x)
        if (!isTRUE(cond9)) {
            verif <- FALSE
            output <- c(output, paste("*", .var.name, cond9))
        }

    } else {
        verif <- FALSE
        output <- c(output, paste("*", .var.name, cond1))
    }

    output <- paste(output, collapse = "\n")

    return(ifelse(verif, verif, output))
}

#' @name check_ts
#' @export
#'
assert_ts <- function(x, add = NULL, .var.name = checkmate::vname(x)) {

    if (is.null(add)) {
        coll <- checkmate::makeAssertCollection()
    } else {
        coll <- add
    }

    checkmate::assert_class(x, classes = "ts", add = coll, .var.name = .var.name)

    # Check de la fréquence
    frequency_ts <- assert_expression(
        expr = {
            stats::frequency(x)
        }
    )
    frequency_ts <- assert_frequency(frequency_ts, add = coll, .var.name = "frequency_ts", warn = FALSE)

    # Check de la temporalité
    start_ts <- assert_expression(
        expr = {
            stats::start(x)
        }
    )
    start_ts <- assert_date_ts(start_ts, frequency_ts = frequency_ts, add = coll, .var.name = "start", warn = FALSE)

    end_ts <- assert_expression(
        expr = {
            stats::end(x)
        }
    )
    end_ts <- assert_date_ts(end_ts, frequency_ts = frequency_ts, add = coll, .var.name = "end", warn = FALSE)

    # Check de la classe de l'objet
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
#' @param frequency_ts un entier qui vaut `4L` (ou `4.`) pour les séries trimestrielles et `12L` (ou `12.`) pour les séries mensuelles.
#' @param add Collection pour stocker les messages d'erreurs (Default is NULL)
#' @param .var.name Nom de l'objet à vérifier pour afficher dans les messages
#'
#' @return En sortie la fonction retourne l'objet `x` de manière invisible ou une erreur.
#'
#' @details
#' Un objet de type TimeUnits est un numérique qui désigne l'année et la période en cours avec ses décimales.
#' Ainsi pour une série temporelle mensuelle, `2020.5` représente la moitié de l'année donc juillet 2020 et s'écrit `c(2020L, 7L)` au format date_ts.
#'
#' Selon le préfixe de la fonction :
#'
#'  - si le check réussi :
#'      - la fonction `assert_TimeUnits` retourne l'objet `x` de manière invisible;
#'      - la fonction `check_TimeUnits` retourne le booléen `TRUE`.
#'
#'  - si le check échoue :
#'      - la fonction `assert_TimeUnits` retourne un message d'erreur;
#'      - la fonction `check_TimeUnits` retourne une chaîne de caractère signalant le problème.
#'
#' @export
#'
#' @examples
#'
#' assert_TimeUnits(2020.5, frequency_ts = 12L)
#' assert_TimeUnits(2020.5, frequency_ts = 4L)
#' assert_TimeUnits(2023, frequency_ts = 12L)
#'
#' assert_TimeUnits(2000 + 5 / 12, frequency_ts = 12L)
#' assert_TimeUnits(2015 + 3 / 4, frequency_ts = 4L)
#'
#' check_TimeUnits(2020.5, frequency_ts = 12L)
#' check_TimeUnits(2015 + 3 / 4, frequency_ts = 4L)
#'
#' # Avec erreur
#'
#' check_TimeUnits(list(1), frequency_ts = 12L)
#' check_TimeUnits(2000, frequency_ts = 1L)
#'
check_TimeUnits <- function(x, frequency_ts, .var.name = checkmate::vname(x)) {

    verif <- TRUE
    output <- c()

    cond1 <- check_frequency(frequency_ts, .var.name = "frequency_ts")

    if (!isTRUE(cond1)) {
        verif <- FALSE
        output <- c(output, cond1)
    }

    cond2 <- checkmate::check_number(x, finite = TRUE, na.ok = FALSE)

    if (!isTRUE(cond2)) {
        verif <- FALSE
        output <- c(output, paste("*", .var.name, cond2))
    }

    if (isTRUE(cond1) && isTRUE(cond2)) {
        if (!isTRUE(checkmate::check_int(x * frequency_ts))) {
            verif <- FALSE
            output <- c(output, paste(.var.name, "* frequency_ts", checkmate::check_int(x * frequency_ts)))
        }
    }

    output <- paste(output, collapse = "\n")

    return(ifelse(verif, verif, output))
}

#' @name check_TimeUnits
#' @export
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

    checkmate::assert_int(x * frequency_ts, .var.name = paste0(.var.name, " * frequency_ts"))

    if (is.null(add)) {
        checkmate::reportAssertions(coll)
    }

    return(invisible(x))
}

#' Vérifie la conformité d'une fréquence
#'
#' @param x un entier qui vaut `4L` (ou `4.`) pour les séries trimestrielles et `12L` (ou `12.`) pour les séries mensuelles.
#' @param warn un booleen
#' @param add Collection pour stocker les messages d'erreurs (Default is NULL)
#' @param .var.name Nom de l'objet à vérifier pour afficher dans les messages
#'
#' @details La fréquence d'une série temporelle est soit mensuelle (`12L` ou `12.`) soit trimestrielle (`4L` ou `4.`). Les autres fréquences ne sont pas acceptées.
#' Cette fonction s'appuie essentiellement sur les fonctions `checkmate::check_numeric`, `checkmate::check_int` et `checkmate::check_choice`.
#' Il y a néanmoins une petite subtilité : on vérifie si l'objet `x` est de type double ou integer.
#' Dans le premier cas, on affichera un warning et on corrigera l'objet au format integer pour les traitements ultérieurs. En sortie, `x` est retourné de manière invisible.
#' Si l'argument `warn` est `FALSE`, alors la fonction ne retournera pas de warning lors de l'évaluation.
#'
#' Selon le préfixe de la fonction :
#'
#'  - si le check réussi :
#'      - la fonction `assert_frequency` retourne l'objet `x` de manière invisible;
#'      - la fonction `check_frequency` retourne le booléen `TRUE`.
#'
#'  - si le check échoue :
#'      - la fonction `assert_frequency` retourne un message d'erreur;
#'      - la fonction `check_frequency` retourne une chaîne de caractère signalant le problème.
#'
#' @export
#'
#' @examples
#'
#' assert_frequency(4L)
#' assert_frequency(12L)
#'
#' check_frequency(4L)
#' check_frequency(12L)
#'
#' # Avec des erreurs,
#'
#' check_frequency(Inf, warn = FALSE)
#' check_frequency(1:10)
#' check_frequency(1L)
#'
check_frequency <- function(x, .var.name = checkmate::vname(x), warn = TRUE) {

    verif <- TRUE
    output <- c()

    if (!isTRUE(checkmate::check_numeric(x, any.missing = FALSE, finite = TRUE))) {
        verif <- FALSE
        output <- c(output, checkmate::check_numeric(x, any.missing = FALSE, finite = TRUE))

    } else {
        if (warn && (!isTRUE(checkmate::check_integer(x)))) {
            err <- try(checkmate::assert_integer(x, .var.name = .var.name), silent = TRUE)
            warning(attr(err, "condition")$message)
        }

        if (!isTRUE(checkmate::check_int(x))) {
            verif <- FALSE
            output <- c(output, checkmate::check_int(x))

        } else if (!isTRUE(checkmate::check_choice(x, choices = c(4L, 12L)))) {
            verif <- FALSE
            output <- c(output, checkmate::check_choice(x, choices = c(4L, 12L)))
        }
    }

    output <- paste("*", .var.name, ":", output)
    output <- paste(output, collapse = "\n")

    return(ifelse(verif, verif, output))
}

#' @name check_frequency
#' @export
#'
assert_frequency <- function(x, add = NULL, .var.name = checkmate::vname(x), warn = TRUE) {

    if (is.null(add)) {
        coll <- checkmate::makeAssertCollection()
    } else {
        coll <- add
    }

    # Check du type
    # Ici on passe d'abord par un check car il y a une génération de warning non voulue sinon...
    if (isTRUE(checkmate::check_numeric(x, any.missing = FALSE, finite = TRUE))) {
        x_corr <- checkmate::assert_int(x, coerce = TRUE, add = coll,
                                        .var.name = .var.name)
        checkmate::assert_choice(x_corr, choices = c(4L, 12L),
                                 add = coll, .var.name = .var.name)
    } else {
        checkmate::assert_numeric(x, any.missing = FALSE, finite = TRUE,
                                  add = coll, .var.name = .var.name)
    }

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
#' @param x un entier relatif (positif, négatif ou nul)
#' @param add Collection pour stocker les messages d'erreurs (Default is NULL)
#' @param .var.name Nom de l'objet à vérifier pour afficher dans les messages
#' @param warn un booleen
#'
#' @return En sortie la fonction retourne l'objet `x` de manière invisible ou une erreur.
#'
#' @details
#' On vérifie que l'objet `x` en entrée est bien un entier.
#' Cette fonction s'appuie essentiellement sur la fonction `checkmate::assert_int`.
#' Il y a néanmoins une petite subtilité : on vérifie si l'objet `x` est de type double ou integer. Si l'objet est de type double (et non integer), la fonction retournera aussi un warning.
#' Dans le premier cas, on affichera un warning et on corrigera l'objet au format integer pour les traitements ultérieurs. En sortie, `x` est retourné de manière invisible.
#' Si l'argument `warn` vaut `FALSE`, alors la fonction ne retournera pas de warning lors de l'évaluation.
#'
#' Selon le préfixe de la fonction :
#'
#'  - si le check réussi :
#'      - la fonction `assert_scalar_integer` retourne l'objet `x` de manière invisible;
#'      - la fonction `check_scalar_integer` retourne le booléen `TRUE`.
#'
#'  - si le check échoue :
#'      - la fonction `assert_scalar_integer` retourne un message d'erreur;
#'      - la fonction `check_scalar_integer` retourne une chaîne de caractère signalant le problème.
#'
#' @export
#'
#' @seealso [check_scalar_natural()], [assert_scalar_natural()]
#'
#' @examples
#'
#' assert_scalar_integer(1L)
#' assert_scalar_integer(100L)
#' assert_scalar_integer(-4L)
#' assert_scalar_integer(0L)
#'
#' check_scalar_integer(1L)
#' check_scalar_integer(100L)
#' check_scalar_integer(-4L)
#' check_scalar_integer(0L)
#'
#' # Avec des erreurs,
#'
#' check_scalar_integer(Inf)
#' check_scalar_integer(1:10)
#' check_scalar_integer(pi)
#' check_scalar_integer(2.)
#'
check_scalar_integer <- function(x, warn = TRUE) {

    verif <- checkmate::check_numeric(x, finite = TRUE, any.missing = FALSE)

    if (isTRUE(verif)) {
        verif <- checkmate::check_int(x)
    }

    if (isTRUE(verif) && warn && !isTRUE(checkmate::check_integer(x))) {
        err <- try(
            checkmate::assert_integer(x),
            silent = TRUE
        )
        warning(attr(err, "condition")$message)
    }

    return(ifelse(isTRUE(verif), verif, paste("\n*", verif)))
}

#' @name check_scalar_integer
#' @export
#'
assert_scalar_integer <- function(x, add = NULL, .var.name = checkmate::vname(x), warn = TRUE) {

    if (is.null(add)) {
        coll <- checkmate::makeAssertCollection()
    } else {
        coll <- add
    }

    if (warn && !isTRUE(checkmate::check_integer(x))) {
        err <- try(checkmate::assert_integer(x, .var.name = .var.name),
                   silent = TRUE)
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
#' @description
#' Le but de cett fonction est de tester si une variable x est un nombre naturel strictement positif.
#'
#' @param x un entier naturel strictement positif
#' @param add Collection pour stocker les messages d'erreurs (Default is NULL)
#' @param .var.name Nom de l'objet à vérifier pour afficher dans les messages
#' @param warn un booleen
#'
#' @return En sortie la fonction retourne l'objet `x` de manière invisible ou une erreur.
#'
#' @details
#' Cette fonction s'appuie essentiellement sur la fonction `checkmate::assert_count`.
#' Il y a néanmoins une petite subtilité : on vérifie si l'objet `x` est de type double ou integer.
#' Dans le premier cas, on affichera un warning et on corrigera l'objet au format integer pour les traitements ultérieurs. En sortie, `x` est retourné de manière invisible.
#' Si l'argument `warn` est `FALSE`, alors la fonction ne retournera pas de warning lors de l'évaluation.
#'
#' Selon le préfixe de la fonction :
#'
#'  - si le check réussi :
#'      - la fonction `assert_scalar_natural` retourne l'objet `x` de manière invisible;
#'      - la fonction `check_scalar_natural` retourne le booléen `TRUE`.
#'
#'  - si le check échoue :
#'      - la fonction `assert_scalar_natural` retourne un message d'erreur;
#'      - la fonction `check_scalar_natural` retourne une chaîne de caractère signalant le problème.
#'
#' @export
#'
#' @seealso [check_scalar_integer()], [assert_scalar_integer()]
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
check_scalar_natural <- function(x, warn = TRUE) {

    verif <- checkmate::check_numeric(x, finite = TRUE)

    if (isTRUE(verif)) {
        verif <- checkmate::check_count(x, positive = TRUE, na.ok = FALSE, null.ok = FALSE)
    }

    if (isTRUE(verif) && warn && !isTRUE(checkmate::check_integer(x))) {
        err <- try(
            checkmate::assert_integer(x),
            silent = TRUE
        )
        warning(attr(err, "condition")$message)
    }

    return(ifelse(isTRUE(verif), verif, paste("\n*", verif)))
}

#' @name check_scalar_natural
#' @export
#'
assert_scalar_natural <- function(x, add = NULL, .var.name = checkmate::vname(x), warn = TRUE) {

    if (is.null(add)) {
        coll <- checkmate::makeAssertCollection()
    } else {
        coll <- add
    }

    # Check du type
    # Ici on passe d'abord par un check car il y a une génération de warning non voulue sinon...
    if (isTRUE(checkmate::check_numeric(x, finite = TRUE))) {
        x_corr <- checkmate::assert_count(x, coerce = TRUE, positive = TRUE,
                                          add = coll, .var.name = .var.name)
    } else {
        checkmate::assert_numeric(x, finite = TRUE,
                                  add = coll, .var.name = .var.name)
    }

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
#' @details
#' On vérifie que l'objet `x` en entrée est bien au format `Date` et qu'il s'agit d'un scalaire (vecteur de taille 1).
#' Cette fonction s'appuie essentiellement sur les fonctions `checkmate::assert_date` et `checkmate::assert_scalar`.
#'
#' Selon le préfixe de la fonction :
#'
#'  - si le check réussi :
#'      - la fonction `assert_scalar_date` retourne l'objet `x` de manière invisible;
#'      - la fonction `check_scalar_date` retourne le booléen `TRUE`.
#'
#'  - si le check échoue :
#'      - la fonction `assert_scalar_date` retourne un message d'erreur;
#'      - la fonction `check_scalar_date` retourne la chaîne de caractère correspondante à l'erreur du check.
#'
#' @export
#'
#' @examples
#'
#' assert_scalar_date(as.Date("2018-01-24"))
#' assert_scalar_date(as.Date("2000-02-29"))
#' assert_scalar_date(Sys.Date())
#'
#' check_scalar_date(as.Date("2018-01-24"))
#' check_scalar_date(as.Date("2000-02-29"))
#' check_scalar_date(Sys.Date())
#'
#' # Avec des erreurs
#'
#' check_scalar_date(2L)
#' check_scalar_date(seq(from = as.Date("2000-01-01"), to = Sys.Date(), by = "year"))
#'
check_scalar_date <- function(x) {

    verif <- TRUE
    output <- c()

    if (!isTRUE(checkmate::check_date(x))) {
        verif <- FALSE
        output <- c(output, checkmate::check_date(x))
    }

    if (!isTRUE(checkmate::check_scalar(x))) {
        verif <- FALSE
        output <- c(output, checkmate::check_scalar(x))
    }

    output <- paste(output, collapse = "\n")

    return(ifelse(verif, verif, output))
}

#' @name check_scalar_date
#' @export
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
#' Vérifie la conformité d'une expression
#'
#' @param expr une expression à évaluer
#'
#' @return En sortie la fonction retourne l'objet `x` (le résultat de l'évaluation de l'expression `expr`) de manière invisible ou une erreur.
#'
#' @details La fonction évalue l'expression `expr`. Le check vérifie si la fonction génère une erreur ou un warning. Si elle ne génère aucun message particulier, on retourne alors l'objet `x` (le résultat de l'évaluation de l'expression `expr`), sans erreur.
#'
#' Selon le préfixe de la fonction :
#'
#'  - si le check réussi :
#'      - la fonction `assert_expression` retourne l'objet `x` de manière invisible;
#'      - la fonction `check_expression` retourne le booléen `TRUE`.
#'
#'  - si le check échoue :
#'      - la fonction `assert_expression` retourne un message d'erreur;
#'      - la fonction `check_expression` retourne la chaîne de caractère "Invalid expression".
#'
#' @export
#'
#' @examples
#'
#' assert_expression(expr = {2 + 2})
#' assert_expression(expr = {is.integer(1L)})
#' try(assert_expression(expr = {log("a")}), silent = TRUE)
#'
#' check_expression(expr = {2 + 2})
#' check_expression(expr = {is.integer(1L)})
#' check_expression(expr = {log("a")})
#'
check_expression <- function(expr) {
    out <- tryCatch(
        expr,
        error = function(e) e,
        warning = function(w) w
    )

    if (inherits(out, "warning") || inherits(out, "error")) {
        return(paste("Invalid expression :", deparse(substitute(expr))))
    }

    return(TRUE)
}

#' @name check_expression
#' @export
#'
assert_expression <- function(expr) {
    out <- tryCatch(
        expr,
        error = function(e) e,
        warning = function(w) w
    )

    if (inherits(out, "warning") || inherits(out, "error")) {
        stop(paste("Invalid expression :", deparse(substitute(expr))))
    }

    return(invisible(expr))
}
