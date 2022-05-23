
isTSdate <- function(date){
    if (!class(date) %in% c("integer", "numeric")) return(FALSE)
    if (is.integer(date) &&
        length(date) %in% 1:2 &&
        all(!is.na(date))) return(TRUE)
    if (is.double(date) &&
        length(date) %in% 1:2 &&
        isTRUE(all.equal(date, round(date))) &&
        all(!is.na(date))){
        warning("La date est de type double. Il faut privilégier le format integer.")
        return(TRUE)
    }
    return(FALSE)
}


