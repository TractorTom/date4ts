
getValue_ts <- function(dataTS, date, nb = 1L) {
    return(dataTS |>
               stats::window(start = date, end = ts4conj::next_date_ts(date, frequency = 12L, lag = nb - 1L)) |>
               as.numeric())
}
