
# Initialisation ---------------------------------------------------------------

set.seed(2030L)

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

liste_type <- c("integer", "character", "double", "logical", "complex", "raw", "Date")


# Tests de résultats positifs --------------------------------------------------

good_years <- c(-200L, -1L, 0L, 1L, 2L, 1950L, 1999L, 2000L, 2001L,  2022L, 3000L)
conversion_mens_trim <- data.frame(mens = 1L:12L, trim = rep(1L:4L, each = 3))

testthat::test_that("good result for integer date", {
    for (good_year in good_years) {
        for (mens in -40L:40L) {

            mens_real <- (mens - 1L) %% 12L + 1L
            year_real <- good_year + (mens - 1L) %/% 12L
            date_expected <- c(year_real, conversion_mens_trim[mens_real, "trim"])
            res <- mens2trim(c(good_year, mens))

            testthat::expect_identical(res, date_expected)
            testthat::expect_type(res, "integer")
        }
    }
})


# Tests de résultats négatifs --------------------------------------------------

wrong_dates <- c(
    fuzzr::test_all()[-10],
    list(list(2020L, 5L), list(2L, "a", 3.5), list(NULL), list(2005), list(c(2022L, 8L)), list(c(2022L, 8.))),
    lapply(liste_type[-c(1L, 3L)], create_random_type, len = 2),
    lapply(liste_type, create_random_type, len = 3),
    list(2019.5, 2020 + 1/12, pi / 4, c(2020, 2.5), c(2010.25, 3), c(2002, 3, 1), c("2002", "3")),
    list(c(2020L, NA_integer_), c(NA_integer_, 5L), c(NA_integer_, NA_integer_), c(2020, NA_real_), c(NA_real_, 5), c(NA_real_, NA_real_)),
    list(2L:4L, c(2020.0, 7, 1), c(2020L, 0L, NA_integer_), numeric(0), integer(0))
)

testthat::test_that("detection of wrong dates", {
    for (wrong_date in wrong_dates) testthat::expect_error(mens2trim(wrong_date), regexp = "La date est au mauvais format.")
})

# Tests positifs avec warning --------------------------------------------------

warning_years <- c(-200., -1., ., 1., 2., 1950., 2000., 2022., 3000.)
warning_months <- c(-200., -5., -1., ., 1., 3., 5., 12., 13., 46.)

testthat::test_that("good result for integer date", {
    for (warning_year in warning_years) {
        for (warning_month in warning_months) {

            testthat::expect_warning({resTrim <- mens2trim(c(warning_year, warning_month))}, regexp = "La date est de type double. Il faut privilégier le format integer.")

            mens_real <- (warning_month - 1L) %% 12L + 1L
            year_real <- warning_year + (warning_month - 1L) %/% 12L
            date_expected <- c(year_real, conversion_mens_trim[mens_real, "trim"]) |> as.integer()

            testthat::expect_identical(resTrim, date_expected)
            testthat::expect_type(resTrim, "integer")
        }
    }
})

