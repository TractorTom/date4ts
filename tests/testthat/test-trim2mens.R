
# Initialisation ---------------------------------------------------------------

set.seed(2029L)

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
conversion_trim_mens <- data.frame(trim = 1L:4L, mens = c(1L, 4L, 7L, 10L))

testthat::test_that("good result for integer date", {
    for (good_year in good_years) {
        for (trim in 1:4) {
            trim_real <- (trim - 1L) %% 4L + 1L
            year_real <- good_year + (trim - 1L) %/% 4L
            date_expected <- c(year_real, conversion_trim_mens[trim_real, "mens"])
            res <- trim2mens(c(good_year, trim))


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
    for (wrong_date in wrong_dates) testthat::expect_error(trim2mens(wrong_date), regexp = "La date est au mauvais format.")
})

# Tests positifs avec warning --------------------------------------------------

double_years <- c(-200., -1., 0., 1., 2., 1950., 2000., 2022., 3000.)

warning_double_quarters <- c(-200., -5., -1., 0., 5., 12., 13., 46.)
warning_integer_trims <- c(-200L, -5L, -1L, 0L, 5L, 12L, 13L, 46L)

double_quarters <- c(1., 2., 3., 4.)
good_quarters <- 1L:4L

testthat::test_that("warning for integer date", {
    for (good_year in good_years) {
        for (warning_trim in warning_integer_trims) {

            testthat::expect_warning({resMens <- trim2mens(c(good_year, warning_trim))}, regexp = "Le nombre de période est négatif ou nul ou dépasse la fréquence. La date va être reformattée.")

            trim_real <- (warning_trim - 1L) %% 4L + 1L
            year_real <- good_year + (warning_trim - 1L) %/% 4L
            date_expected <- c(year_real, conversion_trim_mens[trim_real, "mens"]) |> as.integer()

            testthat::expect_identical(resMens, date_expected)
            testthat::expect_type(resMens, "integer")
        }
    }
})

testthat::test_that("warning for double date", {
    for (warning_year in double_years) {
        for (good_quarter in good_quarters) {

            testthat::expect_warning({resMens <- trim2mens(c(warning_year, good_quarter))}, regexp = "La date est de type double. Il faut privilégier le format integer.")

            trim_real <- (good_quarter - 1L) %% 4L + 1L
            year_real <- warning_year + (good_quarter - 1L) %/% 4L
            date_expected <- c(year_real, conversion_trim_mens[trim_real, "mens"]) |> as.integer()

            testthat::expect_identical(resMens, date_expected)
            testthat::expect_type(resMens, "integer")
        }
    }

    for (good_year in good_years) {
        for (warning_trim in double_quarters) {

            testthat::expect_warning({resMens <- trim2mens(c(good_year, warning_trim))}, regexp = "La date est de type double. Il faut privilégier le format integer.")

            trim_real <- (warning_trim - 1L) %% 4L + 1L
            year_real <- good_year + (warning_trim - 1L) %/% 4L
            date_expected <- c(year_real, conversion_trim_mens[trim_real, "mens"]) |> as.integer()

            testthat::expect_identical(resMens, date_expected)
            testthat::expect_type(resMens, "integer")
        }
    }

    for (warning_year in double_years) {
        for (warning_trim in double_quarters) {

            testthat::expect_warning({resMens <- trim2mens(c(good_year, warning_trim))}, regexp = "La date est de type double. Il faut privilégier le format integer.")

            trim_real <- (warning_trim - 1L) %% 4L + 1L
            year_real <- good_year + (warning_trim - 1L) %/% 4L
            date_expected <- c(year_real, conversion_trim_mens[trim_real, "mens"]) |> as.integer()

            testthat::expect_identical(resMens, date_expected)
            testthat::expect_type(resMens, "integer")
        }
    }
})

testthat::test_that("several warning", {
    for (warning_year in double_years) {
        for (warning_trim in warning_integer_trims) {

            w <- testthat::capture_warnings({resMens <- trim2mens(c(warning_year, warning_trim))})
            testthat::expect_match(object = w, regexp = "La date est de type double. Il faut privilégier le format integer.", all = FALSE)
            testthat::expect_match(object = w, regexp = "Le nombre de période est négatif ou nul ou dépasse la fréquence. La date va être reformattée.", all = FALSE)

            trim_real <- (warning_trim - 1L) %% 4L + 1L
            year_real <- warning_year + (warning_trim - 1L) %/% 4L
            date_expected <- c(year_real, conversion_trim_mens[trim_real, "mens"]) |> as.integer()

            testthat::expect_identical(resMens, date_expected)
            testthat::expect_type(resMens, "integer")
        }
    }

    for (warning_year in double_years) {
        for (warning_trim in warning_double_quarters) {

            w <- testthat::capture_warnings({resMens <- trim2mens(c(warning_year, warning_trim))})
            testthat::expect_match(object = w, regexp = "La date est de type double. Il faut privilégier le format integer.", all = FALSE)
            testthat::expect_match(object = w, regexp = "Le nombre de période est négatif ou nul ou dépasse la fréquence. La date va être reformattée.", all = FALSE)

            trim_real <- (warning_trim - 1L) %% 4L + 1L
            year_real <- warning_year + (warning_trim - 1L) %/% 4L
            date_expected <- c(year_real, conversion_trim_mens[trim_real, "mens"]) |> as.integer()

            testthat::expect_identical(resMens, date_expected)
            testthat::expect_type(resMens, "integer")
        }
    }

    for (good_year in good_years) {
        for (warning_trim in warning_double_quarters) {

            w <- testthat::capture_warnings({resMens <- trim2mens(c(good_year, warning_trim))})
            testthat::expect_match(object = w, regexp = "La date est de type double. Il faut privilégier le format integer.", all = FALSE)
            testthat::expect_match(object = w, regexp = "Le nombre de période est négatif ou nul ou dépasse la fréquence. La date va être reformattée.", all = FALSE)

            trim_real <- (warning_trim - 1L) %% 4L + 1L
            year_real <- good_year + (warning_trim - 1L) %/% 4L
            date_expected <- c(year_real, conversion_trim_mens[trim_real, "mens"]) |> as.integer()

            testthat::expect_identical(resMens, date_expected)
            testthat::expect_type(resMens, "integer")
        }
    }
})

