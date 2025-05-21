testthat::test_that("good result", {
    testthat::expect_no_error(assert_date_ts(x = 2020L, frequency_ts = 12L))
    testthat::expect_no_error(assert_date_ts(
        x = c(2020L, 12L),
        frequency_ts = 12L
    ))
    testthat::expect_no_error(assert_date_ts(
        x = c(2020L, 2L),
        frequency_ts = 4L
    ))
})
