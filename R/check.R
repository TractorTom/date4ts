
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
is_date_ts <- function(date_ts, frequency = 12L, warn = TRUE) {

    # Check de warn
    checkmate::assert_flag(warn)

    # Check de la fréquence
    if (!is_good_frequency(frequency)) {
        stop("La fr\u00e9quence doit \u00eatre trimestrielle ou mensuelle.")
    }

    if (!class(date_ts) %in% c("integer", "numeric")) {
        return(FALSE)
    }

    cond_warning <- (is.double(date_ts)) &&
        (length(date_ts) %in% 1L:2L) &&
        (isTRUE(all.equal(date_ts, round(date_ts)))) &&
        (all(!is.na(date_ts)))

    cond_TRUE <- (is.integer(date_ts)) &&
        (length(date_ts) %in% 1L:2L) &&
        (all(!is.na(date_ts)))

    if (!cond_warning && !cond_TRUE) {
        return(FALSE)
    }

    if (cond_warning) {
        if (warn) warning("La date est de type double. Il faut privil\u00e9gier le format integer.")
    }

    if (length(date_ts) == 2L && (date_ts[2] <= 0L || date_ts[2] > frequency)) {
        if (warn) warning("Le nombre de p\u00e9riode est n\u00e9gatif ou nul ou d\u00e9passe la fr\u00e9quence. La date va \u00eatre reformatt\u00e9e.")
    }
    return(TRUE)
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
    if (!(stats::frequency(dataTS) %in% c(4L, 12L))) {
        if (warn) warning("L'objet dataTS doit \u00eatre de fr\u00e9quence mensuelle ou trimestrielle.")
        return(FALSE)
    }
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

#' Vérifie qu'on objet est bien un vecteur non vide
#'
#' @param x un vecteur
#'
#' @return En sortie la fonction retourne un booleen qui précise si l'argument `x` est bien un vecteur (unidimentionnel) non vide.
#' @export
#'
#' @examples
#'
#' # Réponse positive
#' is_vector(1:3)
#' is_vector(letters)
#' is_vector(seq(
#'     from = as.Date("2000-01-01"),
#'     to = as.Date("2023-05-28"),
#'     by = "month"))
#'
#' # Réponse négative
#' is_vector(NULL)
#' is_vector(list(1, 2, 3))
#' is_vector()
is_vector <- function(x) {

    if (!is.atomic(x)) return(FALSE)
    if (length(x) == 0) return(FALSE)

    return(TRUE)
}

is_TimeUnits <- function(x) {
    return(is.numeric(x) && length(x) == 1L && !all(is.na(x)))
}

is_good_frequency <- function(x) {
    return(is.numeric(x) && length(x) == 1L && x %in% c(4L, 12L))
}

is_single_integer <- function(x, warn = FALSE) {

    # Check de warn
    checkmate::assert_flag(warn)

    if (!isTRUE(reason <- checkmate::check_count(x, positive = TRUE))) {
        if (warn) warning(reason)
        return(FALSE)
    }
    return(TRUE)
}

is_single_date <- function(x, warn = FALSE) {

    # Check de warn
    checkmate::assert_flag(warn)

    if (!isTRUE(reason <- checkmate::check_date(x))) {
        if (warn) warning(reason)
        return(FALSE)
    }

    if (length(x) != 0) {
        if (warn) warning("L'argument x doit être de longueur 1.")
        return(FALSE)
    }

    return(TRUE)
}
