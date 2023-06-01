
# Initialisation ---------------------------------------------------------------

set.seed(2027L)

object_bank_R <- fuzzr::test_all()

# Tests de résultats positifs --------------------------------------------------

testthat::test_that("good result for integer date", {
    for (quarter in c(warning_integer_quarters, good_quarters)) {
        for (year in good_years) {
            TSdate <- as.integer(c(year + quarter %/% 4L, quarter %% 4L + 1L))
            res <- as.YYYYTT(year + quarter / 4L)
            testthat::expect_type(res, "integer")
            testthat::expect_identical(res, TSdate)
        }
    }
})

# Tests de résultats négatifs --------------------------------------------------

testthat::test_that("miscellaneous input are not allowed", {
    for (wrong_time in object_bank_R[-c(10L, 16L)]) {
        testthat::expect_error(as.YYYYTT(wrong_time),
                               regexp = "L'input TimeUnits est au mauvais format.")
    }
    for (wrong_time in wrong_TimeUnits) {
        testthat::expect_error(as.YYYYTT(wrong_time),
                               regexp = "L'input TimeUnits n'est pas coh\u00e9rent temporellement avec les trimestres classiques.")
    }
})
