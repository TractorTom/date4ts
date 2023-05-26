
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
        for (mens in 1:12) {

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

double_years <- c(-200., -1., 0., 1., 2., 1950., 2000., 2022., 3000.)

warning_double_months <- c(-200., -5., -1., 0., 13., 46.)
warning_integer_months <- c(-200L, -5L, -1L, 0L, 13L, 46L)

double_months <- c(1., 2., 3., 4., 5., 6., 7., 8., 9., 10., 11., 12.)
good_months <- 1L:12L

testthat::test_that("warning for integer date", {
    for (good_year in good_years) {
        for (warning_month in warning_integer_months) {

            testthat::expect_warning({resTrim <- mens2trim(c(good_year, warning_month))}, regexp = "Le nombre de période est négatif ou nul ou dépasse la fréquence. La date va être reformattée.")

            mens_real <- (warning_month - 1L) %% 12L + 1L
            year_real <- good_year + (warning_month - 1L) %/% 12L
            date_expected <- c(year_real, conversion_mens_trim[mens_real, "trim"]) |> as.integer()

            testthat::expect_identical(resTrim, date_expected)
            testthat::expect_type(resTrim, "integer")
        }
    }
})

testthat::test_that("warning for double date", {
    for (warning_year in double_years) {
        for (good_month in good_months) {

            testthat::expect_warning({resTrim <- mens2trim(c(warning_year, good_month))}, regexp = "La date est de type double. Il faut privilégier le format integer.")

            mens_real <- (good_month - 1L) %% 12L + 1L
            year_real <- warning_year + (good_month - 1L) %/% 12L
            date_expected <- c(year_real, conversion_mens_trim[mens_real, "trim"]) |> as.integer()

            testthat::expect_identical(resTrim, date_expected)
            testthat::expect_type(resTrim, "integer")
        }
    }

    for (good_year in good_years) {
        for (warning_month in double_months) {

            testthat::expect_warning({resTrim <- mens2trim(c(good_year, warning_month))}, regexp = "La date est de type double. Il faut privilégier le format integer.")

            mens_real <- (warning_month - 1L) %% 12L + 1L
            year_real <- good_year + (warning_month - 1L) %/% 12L
            date_expected <- c(year_real, conversion_mens_trim[mens_real, "trim"]) |> as.integer()

            testthat::expect_identical(resTrim, date_expected)
            testthat::expect_type(resTrim, "integer")
        }
    }

    for (warning_year in double_years) {
        for (warning_month in double_months) {

            testthat::expect_warning({resTrim <- mens2trim(c(good_year, warning_month))}, regexp = "La date est de type double. Il faut privilégier le format integer.")

            mens_real <- (warning_month - 1L) %% 12L + 1L
            year_real <- good_year + (warning_month - 1L) %/% 12L
            date_expected <- c(year_real, conversion_mens_trim[mens_real, "trim"]) |> as.integer()

            testthat::expect_identical(resTrim, date_expected)
            testthat::expect_type(resTrim, "integer")
        }
    }
})

testthat::test_that("several warning", {
    for (warning_year in double_years) {
        for (warning_month in warning_integer_months) {

            w <- testthat::capture_warnings({resTrim <- mens2trim(c(warning_year, warning_month))})
            testthat::expect_match(object = w, regexp = "La date est de type double. Il faut privilégier le format integer.", all = FALSE)
            testthat::expect_match(object = w, regexp = "Le nombre de période est négatif ou nul ou dépasse la fréquence. La date va être reformattée.", all = FALSE)

            mens_real <- (warning_month - 1L) %% 12L + 1L
            year_real <- warning_year + (warning_month - 1L) %/% 12L
            date_expected <- c(year_real, conversion_mens_trim[mens_real, "trim"]) |> as.integer()

            testthat::expect_identical(resTrim, date_expected)
            testthat::expect_type(resTrim, "integer")
        }
    }

    for (warning_year in double_years) {
        for (warning_month in warning_double_months) {

            w <- testthat::capture_warnings({resTrim <- mens2trim(c(warning_year, warning_month))})
            testthat::expect_match(object = w, regexp = "La date est de type double. Il faut privilégier le format integer.", all = FALSE)
            testthat::expect_match(object = w, regexp = "Le nombre de période est négatif ou nul ou dépasse la fréquence. La date va être reformattée.", all = FALSE)

            mens_real <- (warning_month - 1L) %% 12L + 1L
            year_real <- warning_year + (warning_month - 1L) %/% 12L
            date_expected <- c(year_real, conversion_mens_trim[mens_real, "trim"]) |> as.integer()

            testthat::expect_identical(resTrim, date_expected)
            testthat::expect_type(resTrim, "integer")
        }
    }

    for (good_year in good_years) {
        for (warning_month in warning_double_months) {

            w <- testthat::capture_warnings({resTrim <- mens2trim(c(good_year, warning_month))})
            testthat::expect_match(object = w, regexp = "La date est de type double. Il faut privilégier le format integer.", all = FALSE)
            testthat::expect_match(object = w, regexp = "Le nombre de période est négatif ou nul ou dépasse la fréquence. La date va être reformattée.", all = FALSE)

            mens_real <- (warning_month - 1L) %% 12L + 1L
            year_real <- good_year + (warning_month - 1L) %/% 12L
            date_expected <- c(year_real, conversion_mens_trim[mens_real, "trim"]) |> as.integer()

            testthat::expect_identical(resTrim, date_expected)
            testthat::expect_type(resTrim, "integer")
        }
    }
})
