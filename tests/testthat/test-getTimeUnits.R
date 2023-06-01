
# Initialisation ---------------------------------------------------------------

set.seed(2031L)


# Tests de résultats positifs --------------------------------------------------

testthat::test_that("good result for integer date", {
    for (year in good_years) {
        testthat::expect_equal(getTimeUnits(date_ts = year, frequency = 12L),
                               year)
        for (month in good_months) {
            testthat::expect_equal(getTimeUnits(date_ts = c(year, month), frequency = 12L),
                                   year + (month - 1) / 12)
        }
    }
})

testthat::test_that("good result for integer date", {
    for (year in good_years) {
        testthat::expect_equal(getTimeUnits(date_ts = year, frequency = 4L),
                               year)
        for (quarter in good_quarters) {
            testthat::expect_equal(getTimeUnits(date_ts = c(year, quarter), frequency = 4L),
                                   year + (quarter - 1) / 4)
        }
    }
})


# Test de résultat positif avec warnings ----------------------------------

testthat::test_that("warning for integer date", {
    for (year in good_years) {
        for (month in warning_integer_months) {
            testthat::expect_warning({resTU <- getTimeUnits(date_ts = c(year, month), frequency = 12L)},
                                     regexp = "Le nombre de p\u00e9riode est n\u00e9gatif ou nul ou d\u00e9passe la fr\u00e9quence. La date va \u00eatre reformatt\u00e9e.")
            testthat::expect_equal(resTU, year + (month - 1) / 12)
        }
    }
})


testthat::test_that("good result for integer date", {
    for (year in good_years) {
        for (quarter in list_warning_quarters) {
            testthat::expect_warning({resTU <- getTimeUnits(date_ts = c(year, quarter), frequency = 4L)},
                                     regexp = "Le nombre de p\u00e9riode est n\u00e9gatif ou nul ou d\u00e9passe la fr\u00e9quence. La date va \u00eatre reformatt\u00e9e.")
            testthat::expect_equal(resTU, year + (quarter - 1) / 4)
        }
    }
})


# Tests de résultats négatifs --------------------------------------------------

testthat::test_that("miscellaneous date are not allowed", {
    for (wrong_date in wrong_dates) {
        testthat::expect_error(getTimeUnits(date_ts = wrong_date, frequency = 12L),
                               regexp = "La date est au mauvais format.")
    }
    for (wrong_date in  wrong_dates) {
        testthat::expect_error(getTimeUnits(date_ts = wrong_date, frequency = 4L),
                               regexp = "La date est au mauvais format.")
    }
})

testthat::test_that("miscellaneous frequency are not allowed", {
    for (wrong_frequency in c(object_bank_R, weird_frequency)) {
        testthat::expect_error(getTimeUnits(date_ts = create_random_date(), frequency = wrong_frequency),
                               regexp = "La fr\u00e9quence doit \u00eatre trimestrielle ou mensuelle.")
    }
})
