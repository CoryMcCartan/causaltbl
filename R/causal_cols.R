# internal accessors
causal_cols <- function(data) {
    attr(data, "causal_cols")
}
`causal_cols<-` = function(data, value) {
    attr(data, "causal_cols") <- value
    data
}


#' Use `outcome` Attribute for a `causal_tbl`
#'
#' @param data a data frame or `causal_tbl`
#' @param outcome column name of outcome (tidy-selected)
#'
#' @return A `causal_tbl`, otherwise the column name of the outcome
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
    data
}

#' @rdname set_outcome
#' @return For `get_outcome()` the column name of the outcome variable
#' @export
get_outcome <- function(data) {
    causal_cols(data)$outcome
}

#' Use `treatment` Attribute for a `causal_tbl`
#'
#' @param data a data frame or `causal_tbl`
#' @param treatment column name of treatment (tidy-selected)
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
    data
}

#' @rdname set_treatment
#' @return For `get_treatment()` the column name of the treatment variable
#' @export
get_treatment <- function(data) {
    causal_cols(data)$treatment
}


single_col_name <- function(expr, data, arg) {
    idx <- tidyselect::eval_select(expr, data, allow_rename=FALSE, allow_empty=FALSE)
    if (length(idx) > 1) {
        cli_abort("Only one column name is allowed for {.arg {arg}}", call=parent.frame())
    }
    names(data)[idx]
}
