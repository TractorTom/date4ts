
# Initialisation ---------------------------------------------------------------

set.seed(2025L)


# Tests de résultats positifs --------------------------------------------------

testthat::test_that("good result for integer date", {
    for (month in good_months) {
        for (year in good_years) {
            real_year <- year + (month - 1L) %/% 12L
            testthat::expect_identical(libelles_one_date(date = c(year, month), frequency_ts = 12L),
                                       paste(list_months_name[(month - 1L) %% 12L + 1L], real_year))
        }
    }
})

testthat::test_that("good result for integer date", {
    for (quarter in good_quarters) {
        for (year in good_years) {
            real_year <- year + (quarter - 1L) %/% 4L
            testthat::expect_identical(libelles_one_date(date = c(year, quarter), frequency_ts = 4L),
                                       paste0("Q", (quarter - 1L) %% 4L + 1L, " ", real_year))
        }
    }
})


# Test de résultats positifs avec warning ---------------------------------

testthat::test_that("warning for integer date out of range", {
    for (month in warning_integer_months) {
        for (year in good_years) {
            real_year <- year + (month - 1L) %/% 12L
            testthat::expect_warning({libel <- libelles_one_date(date = c(year, month), frequency_ts = 12L)},
                                     regexp = "Assertion on 'period' failed: Element 1 is not <= 12.|Assertion on 'period' failed: Element 1 is not >= 1.")
            testthat::expect_identical(libel,
                                       paste(list_months_name[(month - 1L) %% 12L + 1L], real_year))
        }
    }
})

testthat::test_that("warning for integer date out of range", {
    for (quarter in warning_integer_quarters) {
        for (year in good_years) {
            real_year <- year + (quarter - 1L) %/% 4L
            testthat::expect_warning({libel <- libelles_one_date(date = c(year, quarter), frequency_ts = 4L)},
                                     regexp = "Assertion on 'period' failed: Element 1 is not <= 4.|Assertion on 'period' failed: Element 1 is not >= 1.")
            testthat::expect_identical(libel,
                                       paste0("Q", (quarter - 1L) %% 4L + 1L, " ", real_year))
        }
    }
})

testthat::test_that("warning for double date", {

    for (month in double_months) {
        for (year in good_years) {
            real_year <- year + (month - 1L) %/% 12L
            testthat::expect_warning({libel <- libelles_one_date(date = c(year, month), frequency_ts = 12L)},
                                     regexp = "Assertion on 'date_ts' failed: Must be of type 'integer', not 'double'.")
            testthat::expect_identical(libel,
                                       paste(list_months_name[(month - 1L) %% 12L + 1L], real_year))
        }
    }

    for (month in good_months) {
        for (year in double_years) {
            real_year <- year + (month - 1L) %/% 12L
            testthat::expect_warning({libel <- libelles_one_date(date = c(year, month), frequency_ts = 12L)},
                                     regexp = "Assertion on 'date_ts' failed: Must be of type 'integer', not 'double'.")
            testthat::expect_identical(libel,
                                       paste(list_months_name[(month - 1L) %% 12L + 1L], real_year))
        }
    }

    for (quarter in double_quarters) {
        for (year in good_years) {
            real_year <- year + (quarter - 1L) %/% 4L
            testthat::expect_warning({libel <- libelles_one_date(date = c(year, quarter), frequency_ts = 4L)},
                                     regexp = "Assertion on 'date_ts' failed: Must be of type 'integer', not 'double'.")
            testthat::expect_identical(libel,
                                       paste0("Q", (quarter - 1L) %% 4L + 1L, " ", real_year))
        }
    }

    for (quarter in good_quarters) {
        for (year in double_years) {
            real_year <- year + (quarter - 1L) %/% 4L
            testthat::expect_warning({libel <- libelles_one_date(date = c(year, quarter), frequency_ts = 4L)},
                                     regexp = "Assertion on 'date_ts' failed: Must be of type 'integer', not 'double'.")
            testthat::expect_identical(libel,
                                       paste0("Q", (quarter - 1L) %% 4L + 1L, " ", real_year))
        }
    }
})

testthat::test_that("warning for double date and out of range", {

    for (month in warning_double_months) {
        for (year in good_years) {
            real_year <- year + (month - 1L) %/% 12L
            testthat::expect_warning(
                testthat::expect_warning(
                    object = {libel <- libelles_one_date(date = c(year, month), frequency_ts = 12L)},
                    regexp = "Assertion on 'period' failed: Element 1 is not <= 12.|Assertion on 'period' failed: Element 1 is not >= 1."),
                regexp = "Assertion on 'date_ts' failed: Must be of type 'integer', not 'double'.")
            testthat::expect_identical(libel,
                                       paste(list_months_name[(month - 1L) %% 12L + 1L], real_year))
        }
    }

    for (quarter in warning_double_quarters) {
        for (year in good_years) {
            real_year <- year + (quarter - 1L) %/% 4L
            testthat::expect_warning(
                testthat::expect_warning(
                    object = {libel <- libelles_one_date(date = c(year, quarter), frequency_ts = 4L)},
                    regexp = "Assertion on 'period' failed: Element 1 is not <= 4.|Assertion on 'period' failed: Element 1 is not >= 1."),
                regexp = "Assertion on 'date_ts' failed: Must be of type 'integer', not 'double'.")
            testthat::expect_identical(libel,
                                       paste0("Q", (quarter - 1L) %% 4L + 1L, " ", real_year))
        }
    }
})

testthat::test_that("warning for double monthly frequency", {
    for (month in good_months) {
        for (year in good_years) {
            real_year <- year + (month - 1L) %/% 12L
            testthat::expect_warning({libel <- libelles_one_date(date = c(year, month), frequency_ts = 12)},
                                     regexp = warning_frequency_double)
            testthat::expect_identical(libel,
                                       paste(list_months_name[(month - 1L) %% 12L + 1L], real_year))
        }
    }
})

testthat::test_that("warning for double quaterly frequency", {
    for (quarter in good_quarters) {
        for (year in good_years) {
            real_year <- year + (quarter - 1L) %/% 4L
            testthat::expect_warning({libel <- libelles_one_date(date = c(year, quarter), frequency_ts = 4)},
                                     regexp = warning_frequency_double)
            testthat::expect_identical(libel,
                                       paste0("Q", (quarter - 1L) %% 4L + 1L, " ", real_year))
        }
    }
})


# Tests de résultats négatifs --------------------------------------------------

testthat::test_that("miscellaneous date are not allowed", {
    for (wrong_date in wrong_dates) {
        testthat::expect_error(libelles_one_date(date = wrong_date, frequency_ts = 12L))
        testthat::expect_error(libelles_one_date(date = wrong_date, frequency_ts = 4L))
    }
})

testthat::test_that("miscellaneous frequency are not allowed", {
    for (wrong_frequency in c(object_bank_R, weird_frequency)) {
        testthat::expect_error(libelles_one_date(date = create_random_date(), frequency_ts = wrong_frequency))
    }
})
