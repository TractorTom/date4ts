# Options ----------------------------------------------------------------------

withr::local_locale(.new = c(LC_TIME = "en_US"))
withr::with_envvar(new = c(lang = "en_US"), {

    # Création de fonctions --------------------------------------------------------

    create_random_type <- function(type, len = NULL) {
        if (is.null(len)) len <- sample(1L:1000L, size = 1L)

        if (type == "character") {
            output <- strsplit(x = intToUtf8(sample(c(1L:55295L, 57344L:1114111L),
                                                    size = len, replace = TRUE)),
                               split = "",
                               fixed = TRUE)[[1L]]
        } else if (type == "integer") {
            output <- sample(-20000000L:20000000L, size = len, replace = TRUE)
        } else if (type == "double") {
            output <- runif(n = len, min = -10000L, max = 10000L)
        } else if (type == "logical") {
            output <- sample(x = c(TRUE, FALSE), size = len, replace = TRUE)
        } else if (type == "complex") {
            output <- complex(
                real = runif(n = len, min = -10000L, max = 10000L),
                imaginary = runif(n = len, min = -10000L, max = 10000L)
            )
        } else if (type == "raw") {
            output <- sample(x = as.raw(0L:255L), size = len, replace = TRUE)
        } else if (type == "Date") {
            output <- sample(x = seq(as.Date("1950-01-01"), as.Date("2024-01-01"), by = "day"), size = len, replace = TRUE)
        } else {
            stop("Le type n'est pas reconnu.")
        }
        return(output)
    }

    create_random_date_ts <- function(frequency_ts = NULL) {
        if (runif(n = 1L, min = 0L, max = 1L) > 0.5) {
            return(sample(1950L:2022L, size = 1L))
        }

        if (!is.null(frequency_ts)) {
            return(c(
                sample(1950L:2022L, size = 1L),
                sample(seq_len(frequency_ts), size = 1L)
            ))
        }

        return(c(
            sample(1950L:2022L, size = 1L),
            sample(-20L:20L, size = 1L)
        ))
    }

    create_random_ts <- function(type = NULL, len = NULL, start = NULL, frequency = NULL) {
        if (is.null(type)) type <- sample(list_type, size = 1L)
        if (is.null(len)) len <- sample(1L:1000L, size = 1L)
        if (is.null(frequency)) frequency <- sample(c(4L, 12L), size = 1L)
        if (is.null(start)) start <- create_random_date_ts()

        content <- create_random_type(type, len)

        return(ts(content, start = start, frequency = frequency))
    }

    create_NA_type <- function(type, len = 1L) {
        if (type == "character") {
            output <- rep(x = NA_character_, times = len)
        } else if (type == "integer") {
            output <- rep(x = NA_integer_, times = len)
        } else if (type == "double") {
            output <- rep(x = NA_real_, times = len)
        } else if (type == "logical") {
            output <- rep(x = NA, times = len)
        } else if (type == "complex") {
            output <- rep(x = NA_complex_, times = len)
        } else if (type == "Date") {
            output <- create_NA_type(type = "integer", len = len)
            class(output) <- "Date"
        } else if (type == "raw") {
            output <- rep(x = as.raw(0x00), times = len)
        } else {
            stop("Le type n'est pas reconnu.")
        }
        return(output)
    }


    # Variables globales de test ---------------------------------------------------


    ## Types d'objets --------------------------------------------------------------

    list_type <- c("integer", "character", "double", "logical", "complex", "raw", "Date")

    object_bank_R <- fuzzr::test_all()

    wrong_type_ts <- list(
        ts(list(2L), start = 2010L, frequency = 12L),
        ts(list(2, 3, c(1, 2)), start = 2010L, frequency = 12L),
        ts(list(2L, 3L, 4L), start = 2010L, frequency = 12L),
        ts(list(2L, list("3L"), 4L:15L), start = 2010L, frequency = 12L)
    )


    ## Fréquences ------------------------------------------------------------------

    list_frequence <- c(4L, 12L)
    weird_frequency <- list(1L, 2, 7, 0.1, 1 / 3, 3.5, 365, 365.25, pi)


    ## Dates -----------------------------------------------------------------------

    # Time Units
    list_wrong_timeunits <- list(2020 + 1 / 7, pi, 2020 - 1 / 13, Inf)

    # Années
    good_years <- c(-200L, -1L, 0L, 1L, 2L, 1950L, 1999L, 2000L, 2001L, 2022L, 3000L)
    double_years <- c(-200., -1., 0., 1., 2., 1950., 2000., 2022., 3000.)

    # Month
    warning_double_months <- c(-200., -20., -5., -1., 0., 13., 46., 200.)
    warning_integer_months <- c(-200L, -20L, -12L, -5L:0L, 13L:15L, 24L, 46L, 200L)

    double_months <- c(1., 2., 3., 4.0, 5., 6., 7., 8., 9., 10., 11., 12.0)
    good_months <- 1L:12L

    # Quarter
    warning_double_quarters <- c(-200., -20., -5., -3., -2., -1., 0., 5., 12.0, 13., 46.)
    warning_integer_quarters <- c(-200L, -20L, -5L, -3L, -2L, -1L, 0L, 5L, 12L, 13L, 46L)

    double_quarters <- c(1., 2., 3., 4.0)
    good_quarters <- 1L:4L

    # Dates

    list_start <- list(c(2020L, -1L), c(2020L, 0L), c(2020L, 4L), c(2020L, 5L), c(2020L, 12L), c(2020L, 13L), 2019L)

    list_wrong_date_ts <- c(
        fuzzr::test_all()[-10L],
        list(list(2020L, 5L), list(2L, "a", 3.5), list(NULL), list(2005), list(c(2022L, 8L)), list(c(2022L, 8.))),
        lapply(list_type[-c(1L, 3L)], create_random_type, len = 2L),
        lapply(list_type, create_random_type, len = 3),
        list(2019.5, 2020. + 1. / 12.0, pi / 4.0, c(2020., 2.5), c(2010.25, 3.), c(2002., 3., 1.), c("2002", "3")),
        list(c(2020L, NA_integer_), c(NA_integer_, 5L), c(NA_integer_, NA_integer_), c(2020, NA_real_), c(NA_real_, 5.), c(NA_real_, NA_real_)),
        list(2L:4L, c(2020.0, 7., 1.), c(2020L, 0L, NA_integer_), numeric(0), integer(0), Inf, c(2000L, Inf), c(Inf, 4.0)),
        rnorm(10L)
    )


    ## Conversions et labels -------------------------------------------------------

    # Conversion
    conversion_quarter_month <- data.frame(quarter = 1L:4L, month = c(1L, 4L, 7L, 10L))
    conversion_month_quarter <- data.frame(month = 1L:12L, quarter = rep(1L:4L, each = 3L))

    # Labels
    # list_months_name <- c("janv.", "f\u00e9vr.", "mars", "avr.", "mai", "juin", "juil.", "ao\u00fbt", "sept.", "oct.", "nov.", "d\u00e9c.")
    list_months_name <- c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")


    # Autres objets ----------------------------------------------------------------

    list_len <- c(0L:5L, 10L, 100L, 10000L)
    list_lag <- c(-1000L, -5L, -2L:2L, 5L, 1000L)


    # Error / warning messages -----------------------------------------------------

    message_double <- function(var) {
        return(paste0("Assertion on '", var, "' failed: Must be of type 'integer', not 'double'."))
    }

    invalid_monthly_period <- "Assertion on 'period' failed: Element 1 is not >= 1.|Assertion on 'period' failed: Element 1 is not <= 12.0"
    invalid_quaterly_period <- "Assertion on 'period' failed: Element 1 is not >= 1.|Assertion on 'period' failed: Element 1 is not <= 4.0"

    double_instead_of_integer <- "Must be of type 'integer', not 'double'."
    warning_extend <- "extending time series when replacing values"

})
