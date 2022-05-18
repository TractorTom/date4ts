

previousDate <- function(date, frequency, lag = 1){
    year <- date[1]
    month <- date[2]
    return(c(year + ((month - 1 - lag) %/% frequency),
             1 + ((month - 1 - lag) %% frequency)))
}

nextDate <- function(date, frequency, lag = 1){
    year <- date[1]
    month <- date[2]
    return(c(year + ((month - 1 + lag) %/% frequency),
             1 + ((month - 1 + lag) %% frequency)))
}

firstDate <- function(dataTS){
    timeTS <- dataTS |> zoo::na.trim() |> stats::time()
    firstTime <- timeTS[1]
    return(c(firstTime %/% 1, (firstTime %% 1) * stats::frequency(dataTS) + 1))
}

lastDate <- function(dataTS){
    timeTS <- dataTS |> zoo::na.trim() |> stats::time()
    lastTime <- timeTS[length(timeTS)]
    return(c(lastTime %/% 1, (lastTime %% 1) * stats::frequency(dataTS) + 1))
}

getValue_ts <- function(dataTS, date, nb = 1){
    return(dataTS |>
               stats::window(start = date, end = ts4conj::nextDate(date, frequency = 12, lag = nb - 1)) |>
               as.numeric())
}
