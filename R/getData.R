
#' @export
getValue_ts <- function(dataTS, date_ts, n = 1L) {

    coll <- checkmate::makeAssertCollection()

    # Check de l'objet dataTS
    assert_ts(dataTS, add = coll, .var.name = "dataTS")

    frequency_ts <- as.integer(stats::frequency(dataTS))

    # Check l'argument n
    n <- assert_scalar_natural(n, add = coll, .var.name = "n")
    # Check du format date_ts
    date_ts <- assert_date_ts(x = date_ts, frequency_ts = frequency_ts,
                   add = coll, .var.name = "date_ts")

    checkmate::reportAssertions(coll)

    output_value <- as.numeric(stats::window(
        x = dataTS,
        start = date_ts,
        end = next_date_ts(date_ts, frequency_ts = frequency_ts, lag = n - 1L)))

    return(output_value)
}
