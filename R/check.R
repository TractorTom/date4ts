
#' Vérifie le format de date
#'
#' @description La fonction `isTSdate` vérifie qu'un objet est de type AAAA, c(AAAA, MM) ou c(AAAA, TT)
#' @param date un vecteur numérique, de préférence integer au format AAAA, c(AAAA, MM) ou c(AAAA, TT)
#'
#' @return En sortie la fonction retourne un booleen et un warning additionnel si besoin.
#' @details Les fonctions du package ts4conj sont faites pour fonctionner avec des times-series de fréquence mensuelle ou trimestrielle et basé sur le système des mois, trimestres et années classiques.
#' On cherche donc à favoriser l'utilisation de vecteur c(AAAA, MM) pour désigner la date choisie.
#' @export
#'
#' @examples
#' #De bons formats de date
#' isTSdate(c(2020L, 4L))
#' isTSdate(2022L)
#'
#' #Formats avec un warning
#' isTSdate(c(2020, 4))
#' isTSdate(2022)
#'
#' #Format non accepté --> erreur
#' isTSdate(2022.5)
#' isTSdate(2022 + 1/12)
isTSdate <- function(date){
    if (!class(date) %in% c("integer", "numeric")) return(FALSE)
    if (is.integer(date) &&
        length(date) %in% 1:2 &&
        all(!is.na(date))) return(TRUE)
    if (is.double(date) &&
        length(date) %in% 1:2 &&
        isTRUE(all.equal(date, round(date))) &&
        all(!is.na(date))){
        warning("La date est de type double. Il faut privil\u00e9gier le format integer.")
        return(TRUE)
    }
    return(FALSE)
}
