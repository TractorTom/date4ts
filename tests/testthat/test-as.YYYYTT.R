
# Initialisation ---------------------------------------------------------------

set.seed(2027L)

liste_type <- c("integer", "character", "double", "logical", "complex", "raw", "Date")
object_bank_R <- fuzzr::test_all()

# Tests de résultats positifs --------------------------------------------------

liste_year <- c(1950L, 1978L, 1999L, 2000L, 2022L)

testthat::test_that("good result for integer date", {
    for (quarter in -20L:20L) {
        for (year in liste_year) {
            TSdate <- c(year + quarter %/% 4L, quarter %% 4L + 1L) |> as.integer()
            res <- as.YYYYTT(year + quarter / 4L)
            testthat::expect_type(res, "integer")
            testthat::expect_identical(res, TSdate)
        }
    }
})

# Tests de résultats négatifs --------------------------------------------------

wrong_timeUnits <- list(2020 + 1/7, pi, 2020 - 1/13)

testthat::test_that("miscellaneous input are not allowed", {
    for (wrong_time in object_bank_R[-c(10L, 16L)]) {
        testthat::expect_error(as.YYYYTT(wrong_time),
                               regexp = "L'input timeUnits est au mauvais format.")
    }
    for (wrong_time in wrong_timeUnits) {
        testthat::expect_error(as.YYYYTT(wrong_time),
                               regexp = "L'input timeUnits n'est pas cohérent temporellement avec les trimestres classiques.")
    }
})


