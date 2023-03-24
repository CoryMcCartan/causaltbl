# internal accessors
causal_cols <- function(data) {
    attr(data, "causal_cols")
}
`causal_cols<-` = function(data, value) {
    attr(data, "causal_cols") <- value
    data
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
    causal_cols(data)$outcome <- col
    data[[col]] <- vctrs::vec_cast(data[[col]], numeric(), x_arg=col)
    data
}

#' @rdname set_outcome
#' @return For `get_outcome()` the column name of the outcome variable
#' @export
get_outcome <- function(data) {
    causal_cols(data)$outcome
}
#' @rdname set_outcome
#' @return For `has_outcome()`, `TRUE` if there is an outcome variable set
#' @export
has_outcome <- function(data) {
    !is.null(causal_cols(data)$outcome)
}



#' Define a treatment variable for a `causal_tbl`
#'
#' @param data a data frame or `causal_tbl`
#' @param treatment the column containing the treatment variable (tidy-selected).
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
#'   set_treatment(milk_first)
#' print(data) # a causal_tbl
#' get_treatment(data)
set_treatment <- function(data, treatment) {
    data <- as_causal_tbl(data)
    col <- single_col_name(enquo(treatment), data, "treatment")
    causal_cols(data)$treatment <- col
    data[[col]] <- vctrs::vec_cast(data[[col]], numeric(), x_arg=col)
    data
}

#' @rdname set_treatment
#' @return For `get_treatment()` the column name of the treatment variable
#' @export
get_treatment <- function(data) {
    causal_cols(data)$treatment
}
#' @rdname set_treatment
#' @return For `has_treatment()`, `TRUE` if there is a treatment variable set
#' @export
has_treatment <- function(data) {
    !is.null(causal_cols(data)$treatment)
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
#'   y = c(1, 3, 2, 3, 1, 3, 4, 5)
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
