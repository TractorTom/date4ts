
as.YYYYTT <- function(timets) return(c(timets %/% 1, (timets %% 1) * 4 + 1))

as.YYYYMM <- function(timets) return(c(timets %/% 1, (timets %% 1) * 12 + 1))

trim2mens <- function(date){
    year <- date[1]
    trim <- date[2]
    return(c(year, trim * 3 - 2))
}

mens2trim <- function(date){
    year <- date[1]
    month <- date[2]
    return(c(year, 1 + ((month - 1) %/% 3)))
}
