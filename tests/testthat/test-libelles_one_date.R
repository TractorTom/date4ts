
# Initialisation ---------------------------------------------------------------

set.seed(2025L)

create_random_type <- function(type, len = NULL) {
    if (is.null(len)) len <- sample(1L:1000L, size = 1)
    if (type == "character") return(strsplit(intToUtf8(sample(c(1L:55295L, 57344L:1114111L), size = len, replace = TRUE)), "")[[1]])
    if (type == "integer") return(sample(-20000000L:20000000L, size = len, replace = TRUE))
    if (type == "double") return(runif(n = len, min = -10000L, max = 10000L))
    if (type == "logical") return(sample(x = c(TRUE, FALSE), size = len, replace = TRUE))
    if (type == "complex") return(complex(real = runif(n = len, min = -10000L, max = 10000),
                                          imaginary = runif(n = len, min = -10000L, max = 10000L)))
    if (type == "raw") return(sample(x = as.raw(0L:255L), size = len, replace = TRUE))
    if (type == "Date") return(sample(x = seq(as.Date('1950/01/01'), as.Date('2022/01/01'), by = "day"), size = len, replace = T))
    stop("Le type n'est pas reconnu.")
}

create_random_date <- function() {
    if (runif(1, 0, 1) > .5) return(sample(1950L:2022L, size = 1L))
    return(c(sample(1950L:2022L, size = 1L),
             sample(-20L:20L, size = 1L)))
}

liste_type <- c("integer", "character", "double", "logical", "complex", "raw", "Date")
object_bank_R <- fuzzr::test_all()
weird_frequency <- c(1, 2, 7, .1, 1/3, 3.5, 365.25, pi)
wrong_dates <- c(
    fuzzr::test_all()[-10],
    list(list(2020L, 5L), list(2L, "a", 3.5), list(NULL), list(2005), list(c(2022L, 8L)), list(c(2022L, 8.))),
    lapply(liste_type[-c(1L, 3L)], create_random_type, len = 2),
    lapply(liste_type[-c(1L, 3L)], create_random_type, len = 3),
    list(2019.5, 2020 + 1/12, pi / 4, c(2020, 2.5), c(2010.25, 3), c(2002, 3, 1), c("2002", "3")),
    list(c(2020L, NA_integer_), c(NA_integer_, 5L), c(NA_integer_, NA_integer_), c(2020, NA_real_), c(NA_real_, 5), c(NA_real_, NA_real_)),
    list(2L:4L, c(2020.0, 7, 1), c(2020L, 0L, NA_integer_), numeric(0), integer(0))
)

# Tests de résultats positifs --------------------------------------------------

liste_months_name <- c("janv.", "févr.", "mars", "avr.", "mai", "juin", "juil.", "août", "sept.", "oct.", "nov.", "déc.")
liste_year <- c(1950L, 1978L, 1999L, 2000L, 2022L)

testthat::test_that("good result for integer date", {
    for (month in -20L:20L) {
        for (year in liste_year) {
            real_year <- year + (month - 1L) %/% 12L
            testthat::expect_identical(libelles_one_date(date = c(year, month), frequency = 12L),
                                       paste(liste_months_name[(month - 1L) %% 12L + 1L], real_year))
        }
    }
})

testthat::test_that("good result for integer date", {
    for (quarter in -20L:20L) {
        for (year in liste_year) {
            real_year <- year + (quarter - 1L) %/% 4L
            testthat::expect_identical(libelles_one_date(date = c(year, quarter), frequency = 4L),
                                       paste0("T", (quarter - 1L) %% 4L + 1L, " ", real_year))
        }
    }
})

# Tests de résultats négatifs --------------------------------------------------

testthat::test_that("miscellaneous date are not allowed", {
    for (wrong_date in wrong_dates) {
        testthat::expect_error(libelles_one_date(date = wrong_date, frequency = 12L),
                               regexp = "La date est au mauvais format.")
    }
    for (wrong_date in wrong_dates) {
        testthat::expect_error(libelles_one_date(date = wrong_date, frequency = 4L),
                               regexp = "La date est au mauvais format.")
    }
})

testthat::test_that("miscellaneous frequency are not allowed", {
    for (wrong_frequency in c(object_bank_R, weird_frequency)) {
        testthat::expect_error(libelles_one_date(date = create_random_date(), frequency = wrong_frequency),
                               regexp = "La fréquence doit être trimestrielle ou mensuelle.")
    }
})


