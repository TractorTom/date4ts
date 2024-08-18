# Initialisation ---------------------------------------------------------------

set.seed(2035L)


# Tests de résultats positifs --------------------------------------------------

testthat::test_that("good result for integer frequency", {
    testthat::expect_silent(res <- assert_frequency(x = 12L))
    testthat::expect_identical(res, 12L)

    testthat::expect_silent(res <- assert_frequency(x = 4L))
    testthat::expect_identical(res, 4L)
})

testthat::test_that("good result for double frequency without warning", {
    testthat::expect_silent(res <- assert_frequency(x = 12., warn = FALSE))
    testthat::expect_identical(res, 12L)

    testthat::expect_silent(res <- assert_frequency(x = 4., warn = FALSE))
    testthat::expect_identical(res, 4L)
})


# Tests positifs avec warning --------------------------------------------------

testthat::test_that("warning for integer date", {

    testthat::expect_warning(
        res <- assert_frequency(x = 12., warn = TRUE),
        regexp = double_instead_of_integer
    )
    testthat::expect_identical(res, 12L)

    testthat::expect_warning(
        res <- assert_frequency(x = 4., warn = TRUE),
        regexp = double_instead_of_integer
    )
    testthat::expect_identical(res, 4L)

    testthat::expect_warning(
        res <- assert_frequency(x = 12.),
        regexp = double_instead_of_integer
    )
    testthat::expect_identical(res, 12L)

    testthat::expect_warning(
        res <- assert_frequency(x = 4.),
        regexp = double_instead_of_integer
    )
    testthat::expect_identical(res, 4L)

})


# Tests de résultats négatifs --------------------------------------------------

testthat::test_that("miscellaneous frequency are not allowed", {
    for (wrong_frequency in c(object_bank_R, weird_frequency)) {
        testthat::expect_error(
            assert_frequency(x = wrong_frequency, warn = FALSE)
        )
    }
})
