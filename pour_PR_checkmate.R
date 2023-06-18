
f1 <- function(x) {
    x_corr <- checkmate::assert_int(
        x = x, coerce = TRUE)
    return(invisible(x_corr))
}

f2 <- function(x) {
    coll <- checkmate::makeAssertCollection()
    x_corr <- checkmate::assert_int(
        x = x, coerce = TRUE, add = coll)
    checkmate::reportAssertions(coll)
    return(invisible(x_corr))
}

f1(1L)
f2(1L)

f1(Inf)
f2(Inf)
