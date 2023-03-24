# Main constructor
#' @describeIn causal_tbl Construct a `causal_tbl` with no checks
#' @export
new_causal_tbl <- function(..., .outcome=NULL, .treatment=NULL) {
    out = vctrs::new_data_frame(
        ...,
        class=c("causal_tbl", "tbl_df", "tbl")
    )

    # set attributes for .outcome, .treatment

    out
}

validate_causal_tbl <- function(data, call = parent.frame()) {
    # checks of attributes

    data
}

reconstruct.causal_tbl <- function(data, old) {
    classes <- c("tbl_df", "tbl", "data.frame")
    if (!is.data.frame(data)) {
        cli_abort("{.arg data} must be a data frame.", call=parent.frame())
    }

    if (inherits(data, "grouped_df"))
        classes <- c("grouped_df", classes)
    if (inherits(data, "rowwise_df"))
        classes <- c("rowwise_df", classes)

    if (!missing(old)) {
        # fix attributes
    }

    class(data) <- c("causal_tbl", classes)
    data
}

#' Build a causal data frame
#'
#' A `causal_tbl` is a tibble with additional attribute information stored
#' in `causal_cols`.  See the 'Details' for more on the structure of this
#' attribute.
#'
#' @param ... passed on to [tibble()]
#' @param .outcome the column containing the outcome variable (tidy-selected).
#'   Can be set later with [set_outcome()].
#' @param .treatment the column containing the treatment variable (tidy-selected).
#'   Can be set later with [set_treatment()].
#'
#' @return A `causal_tbl` object
#'
#' @examples
#' causal_tbl(
#'   milk_first = c(0, 1, 0, 1, 1, 0, 0, 1),
#'   guess = c(0, 1, 0, 1, 1, 0, 0, 1)
#' )
#'
#' @export
causal_tbl <- function(..., .outcome=NULL, .treatment=NULL) {
    new_causal_tbl(
        vctrs::df_list(...),
        .outcome=.outcome,
        .treatment=.treatment
    )
}


#' @describeIn causal_tbl Coerce a data frame to a `causal_tbl`
#' @param x A data frame to be coerced
#' @export
as_causal_tbl <- function(x) {
    reconstruct.causal_tbl(x)
}

#' @importFrom pillar tbl_sum
#' @method tbl_sum causal_tbl
#' @export
tbl_sum.causal_tbl <- function(x, ...) {
    c("A causal_tbl" = pillar::dim_desc(x))
}
