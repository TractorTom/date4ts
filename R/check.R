
#' Vérifie le format de date
#'
#' @description La fonction `isGoodDate` vérifie qu'un objet est de type AAAA, c(AAAA, MM) ou c(AAAA, TT)
#' @param date_ts un vecteur numérique, de préférence integer au format AAAA, c(AAAA, MM) ou c(AAAA, TT)
#'
#' @return En sortie la fonction retourne un booleen et un warning additionnel si besoin.
#' @details Les fonctions du package ts4conj sont faites pour fonctionner avec des times-series de fréquence mensuelle ou trimestrielle et basés sur le système des mois, trimestres et années classiques.
#' On cherche donc à favoriser l'utilisation de vecteur c(AAAA, MM) pour désigner la date choisie.
#' @export
#'
#' @examples
#' # De bons formats de date
#' isGoodDate(c(2020L, 8L))
#' isGoodDate(c(2020L, 8L), frequency = 12L)
#' isGoodDate(c(2020L, 2L), frequency = 4L)
#' isGoodDate(2022L)
#'
#' # Formats double --> génération d'un warning
#' isGoodDate(c(2020, 4))
#' isGoodDate(c(2020, 4), frequency = 12)
#' isGoodDate(c(2020, 4), frequency = 4)
#' isGoodDate(2022)
#'
#' # Mauvaise fréquence --> reponse FALSE
#' isGoodDate(c(2020L, 7L), frequency = 4L)
#' isGoodDate(2022 + 1/12)
#'
#' # Format non accepté --> reponse FALSE
#' isGoodDate(2022.5)
#' isGoodDate(2022 + 1/12)
isGoodDate <- function(date_ts, frequency = 12L) {

    if (!is.numeric(frequency) || length(frequency) != 1L || !frequency %in% c(4L, 12L))
        stop("La fr\u00e9quence doit \u00eatre trimestrielle ou mensuelle.")

    if (!class(date_ts) %in% c("integer", "numeric")) {
        return(FALSE)
    } else if (is.double(date_ts)
               && length(date_ts) %in% 1L:2L
               && isTRUE(all.equal(date_ts, round(date_ts)))
               && all(!is.na(date_ts))) {
        warning("La date est de type double. Il faut privil\u00e9gier le format integer.")
        if (length(date_ts) == 2) {
            return(date_ts[2] > 0 && date_ts[2] <= frequency)
        }
    } else if (!(is.integer(date_ts)
                && length(date_ts) %in% 1L:2L
                && all(!is.na(date_ts)))) {
        return(FALSE)
    }

    if (length(date_ts) == 2) {
        return(date_ts[2] > 0 && date_ts[2] <= frequency)
    }
    return(TRUE)
}

#' Vérifie la conformité d'un objet ts dans le cadre des enquêtes de conjoncture
#'
#' @param dataTS un objet ts unidimensionnel
#' @param withWarning un booléen
#'
#' @return En sortie la fonction retourne un booleen qui précise si le TS est conforme ou non.
#' Dans le cas où withWarning vaut TRUE et que le TS n'est pas conforme, un warning qui précise la raison sera déclenché.
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
#' isGoodTS(ts2, withWarning = FALSE)
#' isGoodTS(ts3, withWarning = FALSE)
isGoodTS <- function(dataTS, withWarning = TRUE) {

    #check de withWarning
    if (!(withWarning |> (\(x) (is.logical(x) && length(x) == 1 && !is.na(x)))(x = _)))
        stop("L'argument withWarning doit \u00eatre un bool\u00e9en de longueur 1.")
    #check du type d'objet
    if (!stats::is.ts(dataTS) | stats::is.mts(dataTS)) {
        if (withWarning) warning("L'objets dataTS doit \u00eatre un ts unidimensionnel.")
        return(FALSE)
    }
    #Check de la fréquence
    if (!(stats::frequency(dataTS) %in% c(4L, 12L))) {
        if (withWarning) warning("L'objets dataTS doit \u00eatre de fr\u00e9quence mensuelle ou trimestrielle.")
        return(FALSE)
    }
    #Check de la temporalité
    if (withCallingHandlers({
        !ts4conj::isGoodDate(stats::start(dataTS), frequency = stats::frequency(dataTS)) |
            !ts4conj::isGoodDate(stats::end(dataTS), frequency = stats::frequency(dataTS))},
        warning = function(w) {
            if (w$message == "La date est de type double. Il faut privil\u00e9gier le format integer.") invokeRestart("muffleWarning")
        })
    ) {
        if (withWarning) warning("L'objets dataTS doit \u00eatre coh\u00e9rent avec la temporalit\u00e9 classique.")
        return(FALSE)
    }
    #Check du type des données
    if (!is.atomic(dataTS)) {
        if (withWarning) warning("L'objets dataTS doit \u00eatre d'un type atomic.")
        return(FALSE)
    }

    return(TRUE)
}

