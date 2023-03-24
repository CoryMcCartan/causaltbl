# Constructors and coercion ---------------------------------------------------

# Main constructor
#' @describeIn causal_tbl Construct a `causal_tbl` with no checks
#' @export
new_causal_tbl <- function(..., .outcome=NULL, .treatment=NULL) {
    out = vctrs::new_data_frame(
        ...,
        class=c("causal_tbl", "tbl_df", "tbl")
    )

    # set attributes for .outcome, .treatment
    attr(out, "causal_cols") <- list(outcome = .outcome, treatment = .treatment)

    out
}

validate_causal_tbl <- function(data, call = parent.frame()) {
    cols <- attr(data, "causal_cols")
    if (!is.list(cols)) {
        cli_abort("{.arg {deparse(substitute(data))}} must have a
                  {.code causal_cols} attribute which is a list.", call=call)
    }

    if (!"outcome" %in% names(cols))
        cli_abort("Missing `outcome` in causal_cols", call=call)
    if (!"treatment" %in% names(cols))
        cli_abort("Missing `outcome` in causal_cols", call=call)
    if (!is.null(cols$outcome)) {
        if (!is.character(cols$outcome))
            cli_abort("The `outcome` causal_cols must be stored as a string.", call=call)
        if (!is.numeric(data[[cols$outcome]]))
            cli_abort("The `outcome` column must be numeric.", call=call)
    }
    if (!is.null(cols$treatment)) {
        if (!is.character(cols$treatment))
            cli_abort("The `treatment` causal_cols must be stored as a string.", call=call)
        if (!is.numeric(data[[cols$treatment]]))
            cli_abort("The `treatment` column must be numeric.", call=call)
    }

    data
}

reconstruct.causal_tbl <- function(data, old) {
    classes <- c("tbl_df", "tbl", "data.frame")
    if (!is.data.frame(data)) {
        cli_abort("{.arg {deparse(substittue(data))}} must be a data frame.",
                  call=parent.frame())
    }

    if (inherits(data, "grouped_df"))
        classes <- c("grouped_df", classes)
    if (inherits(data, "rowwise_df"))
        classes <- c("rowwise_df", classes)

    # initialize blank core causal_col if none exists
    if (is.null(causal_cols(data))) {
        causal_cols(data) = list(outcome = NULL, treatment = NULL)
    }

    # copy causal_col from old object as needed/available
    if (!missing(old)) {
        if (!is.null(col <- get_outcome(old)) && col %in% names(data)) {
            set_outcome(data, col)
        }
        if (!is.null(col <- get_treatment(old)) && col %in% names(data)) {
            set_treatment(data, col)
        }

        other_csl_cols <- setdiff(names(causal_cols(old)), names(causal_cols(data)))
        if (length(other_csl_cols) > 1) {
            for (i in seq_len(length(other_csl_cols))) {
                causal_cols(data)[[other_csl_cols[i]]] <- causal_cols(old)[[other_csl_cols[i]]]
            }
        }
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
    data = vctrs::df_list(...)
    if (!missing(.outcome)) {
        outcome <- single_col_name(enquo(.outcome), data, "outcome")
    }
    if (!missing(.treatment)) {
        treatment <- single_col_name(enquo(.treatment), data, "treatment")
    }

    new_causal_tbl(data, .outcome=.outcome, .treatment=.treatment)
}


#' @describeIn causal_tbl Coerce a data frame to a `causal_tbl`
#' @param x A data frame to be coerced
#' @export
as_causal_tbl <- function(x) {
    if (inherits(x, "causal_tbl")) {
        x
    } else if (is.data.frame(x)) {
        reconstruct.causal_tbl(x)
    } else {
        new_causal_tbl(x)
    }
}

assert_causal_tbl <- function(data, arg) {
    if (!inherits(data, "causal_tbl")) {
        cli_abort("{.arg {deparse(substitute(data))}} must be a {.cls causal_tbl}.",
                  call=parent.frame())
    }
}
assert_df <- function(data, arg) {
    if (!is.data.frame(data)) {
        cli_abort("{.arg {deparse(substitute(data))}} must be a data frame.",
                  call=parent.frame())
    }
}

# Slicing and renaming handlers --------------------------------------------

#' @export
`[.causal_tbl` <- function(x, i) {
    old_names <- names(x)
    out <- NextMethod()

    cols <- causal_cols(x)
    for (col in names(cols)) {
        if (is.null(cols[[col]])) next
        if (!cols[[col]] %in% old_names[i]) {
            causal_cols(out)[[col]] = NULL
        }
    }

    out
}

#' @export
`names<-.causal_tbl` <- function(x, value) {
    old_names <- names(x)
    out <- NextMethod()

    cols <- causal_cols(x)
    for (col in names(cols)) {
        if (is.null(cols[[col]])) next
        causal_cols(out)[[col]] = value[which(cols[[col]] == old_names)]
    }

    out
}


# Printing -----------------------------------------------------------------

#' @importFrom pillar tbl_sum
#' @method tbl_sum causal_tbl
#' @export
tbl_sum.causal_tbl <- function(x, ...) {
    lines <- c("A causal_tbl" = pillar::dim_desc(x),
               NextMethod())
    lines[-2] # remove tbl line
}
