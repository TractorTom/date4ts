# Initialisation ---------------------------------------------------------------

set.seed(2045L)


# Tests de résultats positifs --------------------------------------------------

testthat::test_that("good result with Date", {

    testthat::expect_silent(date2date_ts(date = Sys.Date(), frequency_ts = 4L))
    testthat::expect_silent(date2date_ts(date = Sys.Date(), frequency_ts = 12L))

    for (year in good_years[-1:-2]) {
        for (month in good_months) {
            for (day in seq_len(28L)) {
                dateA <- as.Date(paste0(year, "-", month, "-", day), format = "%Y-%m-%d")

                testthat::expect_identical(
                    date2date_ts(date = dateA, frequency_ts = 12L),
                    c(year, month)
                )
                testthat::expect_identical(
                    date2date_ts(date = dateA, frequency_ts = 4L),
                    c(year, month %/% 3L + 1L)
                )
            }

        }
    }
})


# Test de résultat positif avec warnings ---------------------------------------

## Fréquence -------------------------------------------------------------------

testthat::test_that("warning for integer date", {
    for (year in good_years) {
        for (month in good_months) {
            good_timeunits <- year + (month - 1) / 12
            testthat::expect_warning(
                res <- assert_timeunits(x = good_timeunits, frequency_ts = 12.),
                regexp = message_double("frequency_ts")
            )
            testthat::expect_identical(res, good_timeunits)
        }
    }

    for (year in good_years) {
        for (quarter in good_quarters) {
            good_timeunits <- year + (quarter - 1) / 4
            testthat::expect_warning(
                res <- assert_timeunits(x = good_timeunits, frequency_ts = 4.),
                regexp = message_double("frequency_ts")
            )
            testthat::expect_identical(res, good_timeunits)
        }
    }
})


# Tests de résultats négatifs --------------------------------------------------

testthat::test_that("miscellaneous date are not allowed", {
    for (wrong_date in object_bank_R[-34L]) {
        testthat::expect_error(date2date_ts(date = wrong_date, frequency_ts = 12L))
    }
    for (wrong_date in object_bank_R[-34L]) {
        testthat::expect_error(date2date_ts(date = wrong_date, frequency_ts = 4L))
    }
})

testthat::test_that("miscellaneous frequency are not allowed", {
    for (wrong_frequency in c(object_bank_R, weird_frequency)) {
        testthat::expect_error(date2date_ts(date = create_random_type(type = "Date", len = 1L), frequency_ts = wrong_frequency))
    }
})
