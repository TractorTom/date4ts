
assert_1 <- function(x, add = NULL, .var.name = checkmate::vname(x)) {

    if (is.null(add)) {
        coll <- checkmate::makeAssertCollection()
    } else {
        coll <- add
    }

    checkmate::assert_scalar(x, add = coll, .var.name = .var.name)
    checkmate::assert_int(x * 12L, add = coll, .var.name = .var.name)

    if (is.null(add)) {
        checkmate::reportAssertions(coll)
    }

    return(invisible(x))
}

assert_2 <- function(x, add = NULL, .var.name = checkmate::vname(x)) {

    if (is.null(add)) {
        coll <- checkmate::makeAssertCollection()
    } else {
        coll <- add
    }

    checkmate::assert_count(x, add = add, .var.name = .var.name)

    if (is.null(add)) {
        checkmate::reportAssertions(coll)
    }

    return(invisible(x))
}

f <- function(x, y) {

    coll <- checkmate::makeAssertCollection()

    assert_1(x, add = coll)
    assert_2(y, add = coll)

    checkmate::reportAssertions(coll)

    return(2)
}



f(c(1L, 2L), 2L)
f(c(1., 2.), 2L)
f(matrix(0, nrow = 2, 4), 2L)
f(matrix(0, nrow = 2, 4), -7)




