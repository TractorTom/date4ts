

previousDate <- function(date, frequency, lag = 1L){
    year <- date[1L]
    month <- date[2L]
    return(c(year + ((month - 1L - lag) %/% frequency),
             1L + ((month - 1L - lag) %% frequency)))
}

nextDate <- function(date, frequency, lag = 1L){
    if (length(date) == 2L){
        year <- date[1L]
        month <- date[2L]
        return(c(year + ((month - 1L + lag) %/% frequency),
                 1L + ((month - 1L + lag) %% frequency)))
    } else return(date + lag / frequency)
}


firstDate <- function(dataTS){
    timeTS <- dataTS |> zoo::na.trim() |> stats::time()
    firstTime <- timeTS[1L]
    return(c(firstTime %/% 1L, (firstTime %% 1L) * stats::frequency(dataTS) + 1L))
}

lastDate <- function(dataTS){
    timeTS <- dataTS |> zoo::na.trim() |> stats::time()
    lastTime <- timeTS[length(timeTS)]
    return(c(lastTime %/% 1L, (lastTime %% 1L) * stats::frequency(dataTS) + 1L))
}

getValue_ts <- function(dataTS, date, nb = 1L){
    return(dataTS |>
               stats::window(start = date, end = ts4conj::nextDate(date, frequency = 12L, lag = nb - 1L)) |>
               as.numeric())
}
