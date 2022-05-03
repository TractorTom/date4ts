
setValue_ts <- function(dataTS, date, value){
    dataTS |>
        window(start = date, end = nextDate(date, frequency = 12, lag = length(value) - 1)) <- value
        return(dataTS)
}

combine2ts <- function(a, b) return(c(a, b) |> ts(start = start(a), frequency = frequency(a)))

