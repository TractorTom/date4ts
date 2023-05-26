
# Initialisation ---------------------------------------------------------------

# stop("Il faut compléter avec l'utilisation de l'argumetn frequency pour donner de nouveaux cas d'erreur et dupliquer les tests avec ou sans l'argument.")
# stop("Faire les vérifs avec frequency")

set.seed(2024L)

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

list_type <- c("integer", "character", "double", "logical", "complex", "raw", "Date")


# Tests de résultats positifs --------------------------------------------------

good_years <- c(-200L, -1L, 0L, 1L, 2L, 1950L, 2000L, 2022L, 3000L)
list_good_months <- 1L:12L
list_good_quarters <- 1L:4L

testthat::test_that("good result for integer date month", {
    for (year in good_years) {
        testthat::expect_true(is.date_ts(year, frequency = 12L))
        for (month in list_good_months) {
            testthat::expect_true(is.date_ts(c(year, month), frequency = 12L))
        }
    }
})

testthat::test_that("good result for integer date quarter", {
    for (year in good_years) {
        testthat::expect_true(is.date_ts(year, frequency = 4L))
        for (quarter in list_good_quarters) {
            testthat::expect_true(is.date_ts(c(year, quarter), frequency = 4L))
        }
    }
})


# Tests positifs avec warning --------------------------------------------------

## Mensuel -----------------------------------------------------------------

double_years <- c(-200., -1., 0., 1., 2., 1950., 2000., 2022., 3000.)

warning_double_months <- c(-200., -5., -1., 0., 13., 46.)
warning_integer_months <- c(-200L, -5L, -1L, 0L, 13L, 46L)

double_months <- c(1., 2., 3., 4., 5., 6., 7., 8., 9., 10., 11., 12.)

testthat::test_that("warning for integer date", {
    for (good_year in good_years) {
        for (warning_month in warning_integer_months) {
            warning_date <- c(good_year, warning_month)
            testthat::expect_warning({boolRes <- is.date_ts(warning_date)}, regexp = "Le nombre de période est négatif ou nul ou dépasse la fréquence. La date va être reformattée.")
            testthat::expect_true(boolRes)
        }
    }
})

testthat::test_that("warning for double date", {
    for (warning_year in double_years) {

        warning_date <- warning_year
        testthat::expect_warning({boolRes <- is.date_ts(warning_date)}, regexp = "La date est de type double. Il faut privilégier le format integer.")
        testthat::expect_true(boolRes)

        for (good_month in list_good_months) {

            warning_date <- c(warning_year, good_month)
            testthat::expect_warning({boolRes <- is.date_ts(warning_date)}, regexp = "La date est de type double. Il faut privilégier le format integer.")
            testthat::expect_true(boolRes)

        }
    }

    for (good_year in good_years) {
        for (warning_month in double_months) {

            warning_date <- c(good_year, warning_month)
            testthat::expect_warning({boolRes <- is.date_ts(warning_date)}, regexp = "La date est de type double. Il faut privilégier le format integer.")
            testthat::expect_true(boolRes)

        }
    }

    for (warning_year in double_years) {
        for (warning_month in double_months) {

            warning_date <- c(warning_year, warning_month)
            testthat::expect_warning({boolRes <- is.date_ts(warning_date)}, regexp = "La date est de type double. Il faut privilégier le format integer.")
            testthat::expect_true(boolRes)

        }
    }
})

testthat::test_that("several warning", {
    for (warning_year in double_years) {
        for (warning_month in warning_integer_months) {

            warning_date <- c(warning_year, warning_month)
            w <- testthat::capture_warnings({boolRes <- is.date_ts(warning_date)})
            testthat::expect_match(object = w, regexp = "La date est de type double. Il faut privilégier le format integer.", all = FALSE)
            testthat::expect_match(object = w, regexp = "Le nombre de période est négatif ou nul ou dépasse la fréquence. La date va être reformattée.", all = FALSE)

            testthat::expect_true(boolRes)
        }
    }

    for (warning_year in double_years) {
        for (warning_month in warning_double_months) {

            warning_date <- c(warning_year, warning_month)
            w <- testthat::capture_warnings({boolRes <- is.date_ts(warning_date)})
            testthat::expect_match(object = w, regexp = "La date est de type double. Il faut privilégier le format integer.", all = FALSE)
            testthat::expect_match(object = w, regexp = "Le nombre de période est négatif ou nul ou dépasse la fréquence. La date va être reformattée.", all = FALSE)

            testthat::expect_true(boolRes)
        }
    }

    for (good_year in good_years) {
        for (warning_month in warning_double_months) {

            warning_date <- c(warning_year, warning_month)
            w <- testthat::capture_warnings({boolRes <- is.date_ts(warning_date)})
            testthat::expect_match(object = w, regexp = "La date est de type double. Il faut privilégier le format integer.", all = FALSE)
            testthat::expect_match(object = w, regexp = "Le nombre de période est négatif ou nul ou dépasse la fréquence. La date va être reformattée.", all = FALSE)

            testthat::expect_true(boolRes)
        }
    }
})


## Trimestriel -----------------------------------------------------------------

warning_double_quarters <- c(-200., -5., -1., 0., 5., 12., 13., 46.)
warning_integer_quarters <- c(-200L, -5L, -1L, 0L, 5L, 12L, 13L, 46L)

double_quarters <- c(1., 2., 3., 4.)
list_good_quarters <- 1L:4L

testthat::test_that("warning for integer date", {
    for (good_year in good_years) {
        for (warning_quarter in warning_integer_quarters) {
            warning_date <- c(good_year, warning_quarter)
            testthat::expect_warning({boolRes <- is.date_ts(warning_date, frequency = 4L)}, regexp = "Le nombre de période est négatif ou nul ou dépasse la fréquence. La date va être reformattée.")
            testthat::expect_true(boolRes)
        }
    }
})

testthat::test_that("warning for double date", {
    for (warning_year in double_years) {

        warning_date <- warning_year
        testthat::expect_warning({boolRes <- is.date_ts(warning_date, frequency = 4L)}, regexp = "La date est de type double. Il faut privilégier le format integer.")
        testthat::expect_true(boolRes)

        for (good_quarter in list_good_quarters) {

            warning_date <- c(warning_year, good_quarter)
            testthat::expect_warning({boolRes <- is.date_ts(warning_date, frequency = 4L)}, regexp = "La date est de type double. Il faut privilégier le format integer.")
            testthat::expect_true(boolRes)

        }
    }

    for (good_year in good_years) {
        for (warning_quarter in double_quarters) {

            warning_date <- c(good_year, warning_quarter)
            testthat::expect_warning({boolRes <- is.date_ts(warning_date, frequency = 4L)}, regexp = "La date est de type double. Il faut privilégier le format integer.")
            testthat::expect_true(boolRes)

        }
    }

    for (warning_year in double_years) {
        for (warning_quarter in double_quarters) {

            warning_date <- c(warning_year, warning_quarter)
            testthat::expect_warning({boolRes <- is.date_ts(warning_date, frequency = 4L)}, regexp = "La date est de type double. Il faut privilégier le format integer.")
            testthat::expect_true(boolRes)

        }
    }
})

testthat::test_that("several warning", {
    for (warning_year in double_years) {
        for (warning_quarter in warning_integer_quarters) {

            warning_date <- c(warning_year, warning_quarter)
            w <- testthat::capture_warnings({boolRes <- is.date_ts(warning_date, frequency = 4L)})
            testthat::expect_match(object = w, regexp = "La date est de type double. Il faut privilégier le format integer.", all = FALSE)
            testthat::expect_match(object = w, regexp = "Le nombre de période est négatif ou nul ou dépasse la fréquence. La date va être reformattée.", all = FALSE)

            testthat::expect_true(boolRes)
        }
    }

    for (warning_year in double_years) {
        for (warning_quarter in warning_double_quarters) {

            warning_date <- c(warning_year, warning_quarter)
            w <- testthat::capture_warnings({boolRes <- is.date_ts(warning_date, frequency = 4L)})
            testthat::expect_match(object = w, regexp = "La date est de type double. Il faut privilégier le format integer.", all = FALSE)
            testthat::expect_match(object = w, regexp = "Le nombre de période est négatif ou nul ou dépasse la fréquence. La date va être reformattée.", all = FALSE)

            testthat::expect_true(boolRes)
        }
    }

    for (good_year in good_years) {
        for (warning_quarter in warning_double_quarters) {

            warning_date <- c(warning_year, warning_quarter)
            w <- testthat::capture_warnings({boolRes <- is.date_ts(warning_date, frequency = 4L)})
            testthat::expect_match(object = w, regexp = "La date est de type double. Il faut privilégier le format integer.", all = FALSE)
            testthat::expect_match(object = w, regexp = "Le nombre de période est négatif ou nul ou dépasse la fréquence. La date va être reformattée.", all = FALSE)

            testthat::expect_true(boolRes)
        }
    }
})


# Tests de résultats négatifs --------------------------------------------------

wrong_dates <- c(
    fuzzr::test_all()[-10],
    list(list(2020L, 5L), list(2L, "a", 3.5), list(NULL), list(2005), list(c(2022L, 8L)), list(c(2022L, 8.))),
    lapply(list_type[-c(1L, 3L)], create_random_type, len = 2),
    lapply(list_type, create_random_type, len = 3),
    list(2019.5, 2020 + 1/12, pi / 4, c(2020, 2.5), c(2010.25, 3), c(2002, 3, 1), c("2002", "3")),
    list(c(2020L, NA_integer_), c(NA_integer_, 5L), c(NA_integer_, NA_integer_), c(2020, NA_real_), c(NA_real_, 5), c(NA_real_, NA_real_)),
    list(2L:4L, c(2020.0, 7, 1), c(2020L, 0L, NA_integer_), numeric(0), integer(0))
)

testthat::test_that("detection of wrong dates", {
    for (wrong_date in wrong_dates) testthat::expect_false(is.date_ts(wrong_date))
})

object_bank_R <- fuzzr::test_all()
weird_frequency <- c(1, 2, 7, .1, 1/3, 3.5, 365.25, pi)

testthat::test_that("miscellaneous frequency are not allowed", {
    for (wrong_frequency in c(object_bank_R, weird_frequency)) {
        testthat::expect_error(is.date_ts(date_ts = create_random_date(), frequency = wrong_frequency),
                               regexp = "La fréquence doit être trimestrielle ou mensuelle.")
    }
})

