
libeles <- function(date, frequency, nb = 1){

    if (nb <= 0) return(NULL)

    libeles_one_date <- function(date1){
        year <- date1[1]
        if (frequency == 4){
            quarter <- date1[2]
            return(paste0("T", quarter, " ", year))
        } else if (frequency == 12){
            month <- date1[2]
            return(paste(year, sprintf("%02.f", month), "01", sep = "-") |>
                       as.Date() |>
                       format(format = "%b %Y"))
        }
    }

    return(sapply(0:(nb - 1), FUN = \(lag) (lag |> nextDate(date = date, frequency = frequency) |> libeles_one_date())))
}
