#' Use `outcome` Attribute for a `causal_tbl`
#'
#' @param x a `causal_tbl`
#' @param .outcome column name of outcome as a string.
#'
#' @return for `set_outcome()`, a `causal_tbl`, otherwise the column name of the outcome
#' @export
#'
#' @examples
#' causal_tbl(
#'   milk_first = c(0, 1, 0, 1, 1, 0, 0, 1),
#'   guess = c(0, 1, 0, 1, 1, 0, 0, 1),
#'   correct = c(1, 1, 1, 1, 1, 1, 1, 1)
#' ) |>
#' set_outcome("correct")
set_outcome <- function(x, .outcome) {
    attr(x, "causal_cols")[["outcome"]] <- .outcome
}

#' @rdname set_outcome
#' @export
get_outcome <- function(x) {
    attr(x, "causal_cols")[["outcome"]]
}

#' Use `treatment` Attribute for a `causal_tbl`
#'
#' @param x a `causal_tbl`
#' @param .treatment column name of treatment as a string.
#'
#' @return for `set_treatment()`, a `causal_tbl`, otherwise the column name of the treatment
#' @export
#'
#' @examples
#' causal_tbl(
#'   milk_first = c(0, 1, 0, 1, 1, 0, 0, 1),
#'   guess = c(0, 1, 0, 1, 1, 0, 0, 1)
#' ) |>
#' set_treatment("milk_first")
set_treatment <- function(x, .treatment) {
    attr(x, "causal_cols")[["treatment"]] <- .treatment
}

#' @rdname set_treatment
#' @export
get_treatment <- function(x) {
    attr(x, "causal_cols")[["treatment"]]
}
