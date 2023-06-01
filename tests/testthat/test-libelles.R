
# Initialisation ---------------------------------------------------------------

set.seed(2026L)


# Tests de résultats positifs --------------------------------------------------

testthat::test_that("good result for monthly date", {
    for (month in c(warning_integer_months, good_months)) {
        for (year in good_years) {
            for (len in list_len) {
                testthat::expect_identical(libelles(date = c(year, month), frequency = 12L, nb = len),
                                           paste(list_months_name[((month:(month + len - 1L)) - 1L) %% 12L + 1L],
                                                 year +            ((month:(month + len - 1L)) - 1L) %/% 12L))
            }
        }
    }
})

testthat::test_that("good result for quarter date", {
    for (quarter in c(warning_integer_quarters, good_quarters)) {
        for (year in good_years) {
            for (len in list_len) {
                testthat::expect_identical(libelles(date = c(year, quarter), frequency = 4L, nb = len),
                                           paste0("T",   ((quarter:(quarter + len - 1L)) - 1L) %% 4L + 1L, " ",
                                                  year + ((quarter:(quarter + len - 1L)) - 1L) %/% 4L))
            }
        }
    }
})


# Tests de résultats négatifs --------------------------------------------------

testthat::test_that("miscellaneous date are not allowed", {
    for (wrong_date in wrong_dates) {
        testthat::expect_error(libelles(date = wrong_date, frequency = 12L),
                               regexp = "La date est au mauvais format.")
        testthat::expect_error(libelles(date = wrong_date, frequency = 4L),
                               regexp = "La date est au mauvais format.")
    }
})

testthat::test_that("miscellaneous frequency are not allowed", {
    for (wrong_frequency in c(object_bank_R, weird_frequency)) {
        testthat::expect_error(libelles(date = create_random_date(), frequency = wrong_frequency),
                               regexp = "La fréquence doit être trimestrielle ou mensuelle.")
    }
})

testthat::test_that("miscellaneous nb are not allowed", {
    for (wrong_nb in c(object_bank_R[-10])) {
        testthat::expect_error(libelles(date = create_random_date(), frequency = 12L),
                               regexp = "L'argument nb doit être un entier.")
        testthat::expect_error(libelles(date = create_random_date(), frequency = 4L),
                               regexp = "L'argument nb doit être un entier.")
    }
})

