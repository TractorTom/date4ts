
getValue_ts <- function(dataTS, date_ts, nb = 1L) {

    coll <- checkmate::makeAssertCollection()

    # Check de l'objet dataTS
    assert_ts(dataTS, add = coll, .var.name = "dataTS")
    # Check l'argument nb
    assert_scalar_natural(nb, add = coll, .var.name = "nb")
    # Check du format date_ts
    assert_date_ts(x = date_ts,
                   frequency = as.integer(stats::frequency(dataTS)),
                   add = coll, .var.name = "date_ts")

    checkmate::reportAssertions(coll)

    output_value <- as.numeric(stats::window(
        x = dataTS,
        start = date_ts,
        end = next_date_ts(date_ts, frequency = 12L, lag = nb - 1L)))

    return(output_value)
}
