# Initialisation ---------------------------------------------------------------

set.seed(2046L)


# Tests de résultats positifs --------------------------------------------------

testthat::test_that("good result with Date", {

    for (n in list_len[-1L]) {
        testthat::expect_silent(substr_year(date = Sys.Date(), n = n))
    }

    for (year in good_years[-1L:-2L]) {
        for (month in good_months) {
            for (day in seq_len(28L)) {
                dateA <- as.Date(paste0(year, "-", month, "-", day), format = "%Y-%m-%d")

                for (n in list_len[-1L]) {
                    if (year - n >= 0) {
                        date_theo <- as.Date(paste0(year - n, "-", month, "-", day), format = "%Y-%m-%d")
                        testthat::expect_identical(
                            object = substr_year(date = dateA, n = n),
                            expected = date_theo
                        )
                    }

                }
            }
        }
    }
})


# Test de résultat positif avec warnings ---------------------------------------

## n -------------------------------------------------------------------

testthat::test_that("warning for integer date", {
    for (year in good_years) {
        for (month in good_months) {
            good_timeunits <- year + (month - 1) / 12
            testthat::expect_warning(
                res <- assert_timeunits(x = good_timeunits, n = 12.),
                regexp = message_double("frequency_ts")
            )
            testthat::expect_identical(res, good_timeunits)
        }
    }

    for (year in good_years) {
        for (quarter in good_quarters) {
            good_timeunits <- year + (quarter - 1) / 4
            testthat::expect_warning(
                res <- assert_timeunits(x = good_timeunits, n = 4.),
                regexp = message_double("frequency_ts")
            )
            testthat::expect_identical(res, good_timeunits)
        }
    }
})

testthat::test_that("good result with warning n", {

    for (n in list_len[-1L]) {
        n <- as.double(n)
        testthat::expect_warning(
            res <- substr_year(date = Sys.Date(), n = n),
            regexp = message_double("n")
        )
    }

    for (year in good_years[-1L:-2L]) {
        for (month in good_months) {
            for (day in seq_len(28L)) {
                dateA <- as.Date(paste0(year, "-", month, "-", day), format = "%Y-%m-%d")

                for (n in list_len[-1L]) {
                    if (year - n >= 0L) {

                        n <- as.double(n)

                        date_theo <- as.Date(paste0(year - n, "-", month, "-", day), format = "%Y-%m-%d")
                        testthat::expect_warning(
                            res <- substr_year(date = dateA, n = n),
                            regexp = message_double("n")
                        )
                        testthat::expect_identical(res, good_timeunits)
                    }

                }
            }
        }
    }
})

# Tests de résultats négatifs --------------------------------------------------

testthat::test_that("miscellaneous date are not allowed", {
    for (wrong_date in object_bank_R[-34L]) {
        testthat::expect_error(substr_year(date = wrong_date, n = 12L))
    }
    for (wrong_date in object_bank_R[-34L]) {
        testthat::expect_error(substr_year(date = wrong_date, n = 4L))
    }
})

testthat::test_that("miscelanous n are not accepted", {
    for (wrong_n in c(list(0., 0L),
                      list_wrong_date_ts,
                      object_bank_R[-10L],
                      rnorm(10L),
                      as.double(-abs(c(list_lag, list_len, create_random_type("integer", len = 10L)))),
                      -abs(c(list_len, list_lag, create_random_type("integer", len = 10L))))
    ) {
        testthat::expect_error(substr_year(date = create_random_type(type = "Date", len = 1L), n = wrong_n))
    }
})
