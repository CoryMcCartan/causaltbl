#' Work directly with `causal_cols`
#'
#' These functions are aimed at developers who wish to extend `causal_tbl`
#' functionality.
#'
#' @param data A [causal_tbl].
#' @param value New value for `causal_cols`.
#' @param ... Named attributes to add to `data`'s causal attributes.
#' @param what The causal column to get or set.
#' @param ptype A type to coerce a single added column to.
#'
#' @returns Varies. Setter methods return the original `data`, perhaps invisibly.
#'
#' @examples
#' data <- data.frame(
#'   milk_first = c(0, 1, 0, 1, 1, 0, 0, 1),
#'   guess = c(0, 1, 0, 1, 1, 0, 0, 1)
#' ) |>
#'   set_causal_col("treatment", guess=milk_first)
#' print(data)
#' get_causal_col(data, "treatment")
#' causal_cols(data)
#' @export
causal_cols <- function(data) {
    attr(data, "causal_cols")
}
#' @describeIn causal_cols Set `causal_cols`
#' @export
`causal_cols<-` = function(data, value) {
    attr(data, "causal_cols") <- value
    data
}

#' @describeIn causal_cols Set column(s) for a `causal_col`
#' @export
set_causal_col <- function(data, what, ...) {
    data <- as_causal_tbl(data)
    dots <- rlang::quo(c(...))
    cols <- multi_col_name(dots, data, what)
    causal_cols(data)[[what]] <- cols
    data
}
#' @describeIn causal_cols Add a single column to a `causal_col`
#' @export
add_causal_col <- function(data, what, ..., ptype=NULL) {
    data <- as_causal_tbl(data)
    dots <- enquos(...)
    if (length(dots) > 1) {
        cli_abort("Use {.fn set_causal_col} to add more than one column at a time")
    }

    col <- single_col_name(dots[[1]], data, what)
    names(col) = names(dots)

    if (what %in% names(causal_cols(data))) {
        causal_cols(data)[[what]] <- c(causal_cols(data)[[what]], col)
    } else {
        causal_cols(data)[[what]] <- col
    }

    if (!is.null(ptype)) {
        data[[col]] <- vctrs::vec_cast(data[[col]], ptype, x_arg=col)
    }
    data
}

#' @describeIn causal_cols Get the column name of the requested variable
#' @export
get_causal_col <- function(data, what) {
    causal_cols(data)[[what]]
}




#' Define an outcome variable for a `causal_tbl`
#'
#' @param data a data frame or `causal_tbl`
#' @param outcome the column containing the outcome variable (tidy-selected).
#'    Must be numeric or coercible to numeric.
#'
#' @return A `causal_tbl`
#' @export
#'
#' @examples
#' data <- data.frame(
#'   milk_first = c(0, 1, 0, 1, 1, 0, 0, 1),
#'   guess = c(0, 1, 0, 1, 1, 0, 0, 1)
#' ) |>
#'   set_outcome(guess)
#' print(data) # a causal_tbl
#' get_outcome(data)
set_outcome <- function(data, outcome) {
    data <- as_causal_tbl(data)
    col <- single_col_name(enquo(outcome), data, "outcome")
    causal_cols(data)$outcomes <- col
    # coerce
    data[[col]] <- vctrs::vec_cast(data[[col]], numeric(), x_arg=col)
    # handle names
    if (has_treatment(data)) {
        names(causal_cols(data)$treatments)[1] <- col
    }
    data
}

#' @rdname set_outcome
#' @return For `get_outcome()` the column name of the outcome variable
#' @export
get_outcome <- function(data) {
    causal_cols(data)$outcomes[1]
}
#' @rdname set_outcome
#' @return For `has_outcome()`, `TRUE` if there is an outcome variable set
#' @export
has_outcome <- function(data) {
    !is.null(causal_cols(data)$outcomes)
}
#' @rdname set_outcome
#' @return For `pull_outcome()` the vector of the outcome variable.
#' @export
pull_outcome <- function(data) {
    if (!has_outcome(data)) {
        cli::cli_abort("No outcome is set in {.arg data}.")
    }
    data[[get_outcome(data)]]
}



#' Define a treatment variable for a `causal_tbl`
#'
#' @param data a data frame or `causal_tbl`
#' @param treatment the column containing the treatment variable (tidy-selected).
#'    Must be numeric or coercible to numeric.
#' @param outcome the column containing the corresponding outcome variable (tidy-selected).
#'   Default is `get_outcome()`
#'
#' @return A `causal_tbl`
#' @export
#'
#' @examples
#' data <- data.frame(
#'   milk_first = c(0, 1, 0, 1, 1, 0, 0, 1),
#'   guess = c(0, 1, 0, 1, 1, 0, 0, 1)
#' ) |>
#'   set_treatment(milk_first)
#' print(data) # a causal_tbl
#' get_treatment(data)
set_treatment <- function(data, treatment, outcome = get_outcome()) {
    data <- as_causal_tbl(data)
    col <- single_col_name(enquo(treatment), data, "treatment")
    causal_cols(data)$treatments <- col
    # coerce
    data[[col]] <- vctrs::vec_cast(data[[col]], numeric(), x_arg=col)
    # handle names
    if (has_outcome(data)) {
        names(causal_cols(data)$treatments) <- get_outcome(data)
    }
    data
}

#' @rdname set_treatment
#' @return For `get_treatment()` the column name of the treatment variable.
#'   If an outcome variable has been set, the output `name()` will be the
#'   outcome column.
#' @export
get_treatment <- function(data) {
    causal_cols(data)$treatments[1]
}
#' @rdname set_treatment
#' @return For `has_treatment()`, `TRUE` if there is a treatment variable set
#' @export
has_treatment <- function(data) {
    !is.null(causal_cols(data)$treatments)
}
#' @rdname set_treatment
#' @return For `pull_treatment()` the vector of the treatment variable.
#' @export
pull_treatment <- function(data) {
    if (!has_treatment(data)) {
        cli::cli_abort("No treatment is set in {.arg data}.")
    }
    data[[get_treatment(data)]]
}



#' Define a panel data structure for a `causal_tbl`
#'
#' @param data a data frame or `causal_tbl`
#' @param unit the column indexing treatment units (tidy-selected)
#' @param time the column indexing treatment time (tidy-selected)
#'
#' @return A `causal_tbl`
#' @export
#'
#' @examples
#' data <- data.frame(
#'   id = c("a", "a", "a", "a", "b", "b", "b", "b"),
#'   year = rep(2015:2018, 2),
#'   trt = c(0, 0, 0, 0, 0, 0, 1, 1),
#'   y = c(1, 3, 2, 3, 2, 4, 4, 5)
#' ) |>
#'   set_panel(unit=id, time=year)
#' print(data) # a causal_tbl
#' get_panel(data)
set_panel <- function(data, unit, time) {
    data <- as_causal_tbl(data)
    col_unit <- single_col_name(enquo(unit), data, "unit")
    col_time <- single_col_name(enquo(time), data, "time")
    causal_cols(data)$panel_unit = col_unit
    causal_cols(data)$panel_time = col_time
    data[[col_time]] <- vctrs::vec_cast(data[[col_time]], integer(), x_arg=col_time)
    data
}

#' @rdname set_panel
#' @return For `get_panel()` a list with the column names of the unit and time variables
#' @export
get_panel <- function(data) {
    list(
        unit = causal_cols(data)$panel_unit,
        time = causal_cols(data)$panel_time
    )
}
#' @rdname set_panel
#' @return For `has_panel()`, `TRUE` if there is panel data structure
#' @export
has_panel <- function(data) {
    !is.null(causal_cols(data)$panel_unit) &&
        !is.null(causal_cols(data)$panel_time)
}
#' @rdname set_panel
#' @return For `pull_panel_unit()` and `pull_panel_time()` the vector of the panel variable.
#' @export
pull_panel_unit <- function(data) {
    if (!has_panel(data)) {
        cli::cli_abort("No panel is set in {.arg data}.")
    }
    data[[get_panel(data)$unit]]
}
#' @rdname set_panel
#' @export
pull_panel_time <- function(data) {
    if (!has_panel(data)) {
        cli::cli_abort("No panel is set in {.arg data}.")
    }
    data[[get_panel(data)$time]]
}




# Helper
single_col_name <- function(expr, data, arg) {
    idx <- tidyselect::eval_select(expr, data, allow_rename=FALSE)
    if (length(idx) == 0) {
        cli_abort("Must select a column for {.arg {arg}}", call=parent.frame())
    } else if (length(idx) > 1) {
        cli_abort("Only one column name is allowed for {.arg {arg}}", call=parent.frame())
    }
    names(data)[idx]
}

# Helper
multi_col_name <- function(expr, data, arg) {
    idx <- tidyselect::eval_select(expr, data, allow_rename=TRUE)
    if (length(idx) == 0) {
        cli_abort("Must select a column for {.arg {arg}}", call=parent.frame())
    }
    out <- names(data)[idx]
    nms <- names(idx)
    if (!all(nms == out)) {
        nms[nms == out] = ""
        names(out) <- nms
    }
    out
}
