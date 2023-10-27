# Initialisation ---------------------------------------------------------------

set.seed(2026L)


# Tests de résultats positifs --------------------------------------------------

testthat::test_that("good result for monthly date", {
    for (month in good_months) {
        for (year in good_years) {
            for (len in list_len[c(-1L, -9L)]) {
                testthat::expect_identical(
                    libelles(date = c(year, month), frequency_ts = 12L, n = len),
                    paste(
                        list_months_name[((month:(month + len - 1L)) - 1L) %% 12L + 1L],
                        year + ((month:(month + len - 1L)) - 1L) %/% 12L
                    )
                )
            }
        }
    }
})

testthat::test_that("good result for quarter date", {
    for (quarter in good_quarters) {
        for (year in good_years) {
            for (len in list_len[c(-1L, -9L)]) {
                testthat::expect_identical(
                    libelles(date = c(year, quarter), frequency_ts = 4L, n = len),
                    paste0(
                        "Q", ((quarter:(quarter + len - 1L)) - 1L) %% 4L + 1L, " ",
                        year + ((quarter:(quarter + len - 1L)) - 1L) %/% 4L
                    )
                )
            }
        }
    }
})


# Tests de résultats positifs avec warning -------------------------------------

testthat::test_that("warning result for monthly date", {
    for (month in warning_integer_months) {
        for (year in good_years) {
            for (len in list_len[c(-1L, -9L)]) {
                testthat::expect_warning(
                    testthat::expect_identical(
                        libelles(date = c(year, month), frequency_ts = 12L, n = len, warn = TRUE),
                        paste(
                            list_months_name[((month:(month + len - 1L)) - 1L) %% 12L + 1L],
                            year + ((month:(month + len - 1L)) - 1L) %/% 12L
                        )
                    )
                )
                testthat::expect_warning(
                    testthat::expect_identical(
                        libelles(date = c(year, month), frequency_ts = 12L, n = len),
                        paste(
                            list_months_name[((month:(month + len - 1L)) - 1L) %% 12L + 1L],
                            year + ((month:(month + len - 1L)) - 1L) %/% 12L
                        )
                    )
                )
                testthat::expect_no_warning(
                    testthat::expect_identical(
                        libelles(date = c(year, month), frequency_ts = 12L, n = len, warn = FALSE),
                        paste(
                            list_months_name[((month:(month + len - 1L)) - 1L) %% 12L + 1L],
                            year + ((month:(month + len - 1L)) - 1L) %/% 12L
                        )
                    )
                )
            }
        }
    }
})

testthat::test_that("warning result for quarter date", {
    for (quarter in warning_integer_quarters) {
        for (year in good_years) {
            for (len in list_len[c(-1L, -9L)]) {
                testthat::expect_warning(
                    testthat::expect_identical(
                        libelles(date = c(year, quarter), frequency_ts = 4L, n = len, warn = TRUE),
                        paste0(
                            "Q", ((quarter:(quarter + len - 1L)) - 1L) %% 4L + 1L, " ",
                            year + ((quarter:(quarter + len - 1L)) - 1L) %/% 4L
                        )
                    )
                )
                testthat::expect_warning(
                    testthat::expect_identical(
                        libelles(date = c(year, quarter), frequency_ts = 4L, n = len),
                        paste0(
                            "Q", ((quarter:(quarter + len - 1L)) - 1L) %% 4L + 1L, " ",
                            year + ((quarter:(quarter + len - 1L)) - 1L) %/% 4L
                        )
                    )
                )
                testthat::expect_no_warning(
                    testthat::expect_identical(
                        libelles(date = c(year, quarter), frequency_ts = 4L, n = len, warn = FALSE),
                        paste0(
                            "Q", ((quarter:(quarter + len - 1L)) - 1L) %% 4L + 1L, " ",
                            year + ((quarter:(quarter + len - 1L)) - 1L) %/% 4L
                        )
                    )
                )
            }
        }
    }
})


testthat::test_that("warning result for monthly date double", {
    for (month in double_months) {
        for (year in good_years) {
            for (len in list_len[c(-1L, -9L)]) {
                testthat::expect_warning(
                    testthat::expect_identical(
                        libelles(date = c(year, month), frequency_ts = 12L, n = len, warn = TRUE),
                        paste(
                            list_months_name[((month:(month + len - 1L)) - 1L) %% 12L + 1L],
                            year + ((month:(month + len - 1L)) - 1L) %/% 12L
                        )
                    )
                )
                testthat::expect_warning(
                    testthat::expect_identical(
                        libelles(date = c(year, month), frequency_ts = 12L, n = len),
                        paste(
                            list_months_name[((month:(month + len - 1L)) - 1L) %% 12L + 1L],
                            year + ((month:(month + len - 1L)) - 1L) %/% 12L
                        )
                    )
                )
                testthat::expect_no_warning(
                    testthat::expect_identical(
                        libelles(date = c(year, month), frequency_ts = 12L, n = len, warn = FALSE),
                        paste(
                            list_months_name[((month:(month + len - 1L)) - 1L) %% 12L + 1L],
                            year + ((month:(month + len - 1L)) - 1L) %/% 12L
                        )
                    )
                )
            }
        }
    }
    for (month in good_months) {
        for (year in double_years) {
            for (len in list_len[c(-1L, -9L)]) {
                testthat::expect_warning(
                    testthat::expect_identical(
                        libelles(date = c(year, month), frequency_ts = 12L, n = len, warn = TRUE),
                        paste(
                            list_months_name[((month:(month + len - 1L)) - 1L) %% 12L + 1L],
                            year + ((month:(month + len - 1L)) - 1L) %/% 12L
                        )
                    )
                )
                testthat::expect_warning(
                    testthat::expect_identical(
                        libelles(date = c(year, month), frequency_ts = 12L, n = len),
                        paste(
                            list_months_name[((month:(month + len - 1L)) - 1L) %% 12L + 1L],
                            year + ((month:(month + len - 1L)) - 1L) %/% 12L
                        )
                    )
                )
                testthat::expect_no_warning(
                    testthat::expect_identical(
                        libelles(date = c(year, month), frequency_ts = 12L, n = len, warn = FALSE),
                        paste(
                            list_months_name[((month:(month + len - 1L)) - 1L) %% 12L + 1L],
                            year + ((month:(month + len - 1L)) - 1L) %/% 12L
                        )
                    )
                )
            }
        }
    }
})

testthat::test_that("warning result for quarter date double", {
    for (quarter in double_quarters) {
        for (year in good_years) {
            for (len in list_len[c(-1L, -9L)]) {
                testthat::expect_warning(
                    testthat::expect_identical(
                        libelles(date = c(year, quarter), frequency_ts = 4L, n = len, warn = TRUE),
                        paste0(
                            "Q", ((quarter:(quarter + len - 1L)) - 1L) %% 4L + 1L, " ",
                            year + ((quarter:(quarter + len - 1L)) - 1L) %/% 4L
                        )
                    )
                )
                testthat::expect_warning(
                    testthat::expect_identical(
                        libelles(date = c(year, quarter), frequency_ts = 4L, n = len),
                        paste0(
                            "Q", ((quarter:(quarter + len - 1L)) - 1L) %% 4L + 1L, " ",
                            year + ((quarter:(quarter + len - 1L)) - 1L) %/% 4L
                        )
                    )
                )
                testthat::expect_no_warning(
                    testthat::expect_identical(
                        libelles(date = c(year, quarter), frequency_ts = 4L, n = len, warn = FALSE),
                        paste0(
                            "Q", ((quarter:(quarter + len - 1L)) - 1L) %% 4L + 1L, " ",
                            year + ((quarter:(quarter + len - 1L)) - 1L) %/% 4L
                        )
                    )
                )
            }
        }
    }
    for (quarter in good_quarters) {
        for (year in double_years) {
            for (len in list_len[c(-1L, -9L)]) {
                testthat::expect_warning(
                    testthat::expect_identical(
                        libelles(date = c(year, quarter), frequency_ts = 4L, n = len, warn = TRUE),
                        paste0(
                            "Q", ((quarter:(quarter + len - 1L)) - 1L) %% 4L + 1L, " ",
                            year + ((quarter:(quarter + len - 1L)) - 1L) %/% 4L
                        )
                    )
                )
                testthat::expect_warning(
                    testthat::expect_identical(
                        libelles(date = c(year, quarter), frequency_ts = 4L, n = len),
                        paste0(
                            "Q", ((quarter:(quarter + len - 1L)) - 1L) %% 4L + 1L, " ",
                            year + ((quarter:(quarter + len - 1L)) - 1L) %/% 4L
                        )
                    )
                )
                testthat::expect_no_warning(
                    testthat::expect_identical(
                        libelles(date = c(year, quarter), frequency_ts = 4L, n = len, warn = FALSE),
                        paste0(
                            "Q", ((quarter:(quarter + len - 1L)) - 1L) %% 4L + 1L, " ",
                            year + ((quarter:(quarter + len - 1L)) - 1L) %/% 4L
                        )
                    )
                )
            }
        }
    }
})


# Tests de résultats négatifs --------------------------------------------------

testthat::test_that("miscellaneous date are not allowed", {
    for (wrong_date in wrong_dates) {
        testthat::expect_error(libelles(date = wrong_date, frequency_ts = 12L, warn = FALSE))
        testthat::expect_error(libelles(date = wrong_date, frequency_ts = 4L, warn = FALSE))
    }
})

testthat::test_that("miscellaneous frequency are not allowed", {
    for (wrong_frequency in c(object_bank_R, weird_frequency)) {
        testthat::expect_error(libelles(date = create_random_date(), frequency_ts = wrong_frequency, warn = FALSE))
    }
})

testthat::test_that("miscellaneous n are not allowed", {
    for (wrong_n in c(list(0., 0L), object_bank_R[-10])) {
        testthat::expect_error(libelles(date = create_random_date(), frequency_ts = 12L, n = wrong_n, warn = FALSE))
        testthat::expect_error(libelles(date = create_random_date(), frequency_ts = 4L, n = wrong_n, warn = FALSE))
    }
})
