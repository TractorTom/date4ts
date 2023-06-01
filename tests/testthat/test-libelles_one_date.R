
# Initialisation ---------------------------------------------------------------

set.seed(2025L)


# Tests de résultats positifs --------------------------------------------------

testthat::test_that("good result for integer date", {
    for (month in good_months) {
        for (year in good_years) {
            real_year <- year + (month - 1L) %/% 12L
            testthat::expect_identical(libelles_one_date(date = c(year, month), frequency = 12L),
                                       paste(list_months_name[(month - 1L) %% 12L + 1L], real_year))
        }
    }
})

testthat::test_that("good result for integer date", {
    for (quarter in good_quarters) {
        for (year in good_years) {
            real_year <- year + (quarter - 1L) %/% 4L
            testthat::expect_identical(libelles_one_date(date = c(year, quarter), frequency = 4L),
                                       paste0("T", (quarter - 1L) %% 4L + 1L, " ", real_year))
        }
    }
})


# Test de résultats positifs avec warning ---------------------------------

testthat::test_that("warning for integer date", {
    for (month in warning_integer_months) {
        for (year in good_years) {
            real_year <- year + (month - 1L) %/% 12L
            testthat::expect_warning({libel <- libelles_one_date(date = c(year, month), frequency = 12L)},
                                     regexp = "Le nombre de période est négatif ou nul ou dépasse la fréquence. La date va être reformattée.")
            testthat::expect_identical(libel,
                                       paste(list_months_name[(month - 1L) %% 12L + 1L], real_year))
        }
    }
})

testthat::test_that("warning for integer date", {
    for (quarter in warning_integer_quarters) {
        for (year in good_years) {
            real_year <- year + (quarter - 1L) %/% 4L
            testthat::expect_warning({libel <- libelles_one_date(date = c(year, quarter), frequency = 4L)},
                                     regexp = "Le nombre de période est négatif ou nul ou dépasse la fréquence. La date va être reformattée.")
            testthat::expect_identical(libel,
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
