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
    cols <- list(outcome = .outcome, treatment = .treatment)
    if (!is.null(.treatment))
        names(cols$treatment) = cols$outcome
    attr(out, "causal_cols") <- cols

    out
}

validate_causal_tbl <- function(data, call = parent.frame()) {
    cols <- attr(data, "causal_cols")
    if (!is.list(cols)) {
        cli_abort("{.arg {deparse(substitute(data))}} must have a
                  {.code causal_cols} attribute which is a list.", call=call)
    }

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
        cli_abort("{.arg {deparse(substitute(data))}} must be a data frame.",
                  call=parent.frame())
    }

    if (inherits(data, "grouped_df"))
        classes <- c("grouped_df", classes)
    if (inherits(data, "rowwise_df"))
        classes <- c("rowwise_df", classes)

    # initialize blank core causal_col if none exists
    if (is.null(causal_cols(data))) {
        if (missing(old)) {
            causal_cols(data) = list(outcome = NULL, treatment = NULL)
        } else {
            causal_cols(data) = causal_cols(old)
        }
    }

    class(data) <- c("causal_tbl", classes)
    data
}

#' Build a causal data frame
#'
#' A `causal_tbl` is a tibble with additional attribute information stored
#' in [causal_cols].  See the 'Internal structure' for more on the structure of this
#' attribute.
#'
#' At its core, a `causal_tbl` is just a tibble, and it should behave like
#' a tibble in every meaningful way.
#' What sets a `causal_tbl` apart is that it keeps track of *causal columns*:
#' variables or objects which play a particular causal role.
#' These can be accessed with the various getter and setter functions included
#' in this package, like [get_outcome()] and [set_outcome()].
#'
#' # Internal structure
#' The [causal_cols] attribute is considered mostly internal, and end users
#' do not have to worry about its internal structure. However, for those
#' developing packages based off of `causal_tbl`, it is useful to understand the
#' underlying structure of `causal_cols`.
#'
#' The `causal_cols` attribute is a named list, with each element corresponding
#' to a type of causal variable or object: `outcome`, `treatment`, `unit`, but
#' also potentially `pscore`, `matches`, `model`, etc.
#' Each of these elements is a character vector, with each element being a name
#' of a column in the data frame.
#' For some variables, this vector should be of length 1, but for other
#' variables, there may be multiple columns of that type.
#' So, for example, if a package author was developing methods for causal
#' inference with multiple continuous treatments, the `treatment` element
#' of `causal_cols` could have an entry for each treatment column.
#'
#' The optional [names()] of the columns within a particular element of
#' `causal_cols` convey information on any associated variable.  For example,
#' the treatment variable is by default associated with a particular outcome.
#' And a propensity score or outcome model is associated with a particular
#' treatment or outcome variable.
#'
#' The column names stored within any part of `causal_cols` will be automatically
#' updated if columns are renamed, or set to `NULL` if columns are dropped.
#' This reassignment happens automatically and silently in all cases.
#' It is the responsibility of implementers of particular methods to check
#' that a `causal_tbl` has the necessary columns set via helpers like
#' [has_treatment()], [has_outcome()], etc.
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
#' data <- causal_tbl(
#'   milk_first = c(0, 1, 0, 1, 1, 0, 0, 1),
#'   guess = c(0, 1, 0, 1, 1, 0, 0, 1)
#' )
#' is_causal_tbl(data)
#' print(data)
#'
#' @export
causal_tbl <- function(..., .outcome=NULL, .treatment=NULL) {
    data = vctrs::df_list(...)
    if (!missing(.outcome)) {
        .outcome <- single_col_name(enquo(.outcome), data, "outcome")
    }
    if (!missing(.treatment)) {
        .treatment <- single_col_name(enquo(.treatment), data, "treatment")
    }

    new_causal_tbl(data, .outcome=.outcome, .treatment=.treatment)
}


#' @describeIn causal_tbl Coerce a data frame to a `causal_tbl`
#' @param x A data frame to be checked or coerced
#' @export
as_causal_tbl <- function(x) {
    if (is_causal_tbl(x)) {
        x
    } else if (is.data.frame(x)) {
        reconstruct.causal_tbl(x)
    } else {
        new_causal_tbl(x)
    }
}

#' @describeIn causal_tbl Return `TRUE` if a data frame is a `causal_tbl`
#' @export
is_causal_tbl <- function(x) {
    inherits(x, "causal_tbl")
}

assert_causal_tbl <- function(data, arg) {
    if (!is_causal_tbl(data)) {
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
    new_names <- names(x)[i]
    out <- NextMethod()

    cols <- causal_cols(x)
    for (col in names(cols)) {
        if (is.null(cols[[col]])) next
        # figure out what to subset to
        keep = cols[[col]] %in% new_names
        new_col = cols[[col]][keep]
        # handle subsetting
        if (length(new_col) == 0) { # causal_col removed (set to NULL)
            new_col = NULL
        } else if (!is.null(names(new_col))) {
            nms <- new_names[match(names(new_col), new_names)]
            if (all(is.na(nms))) {
                nms = NULL
            } else {
                nms[is.na(nms)] = ""
            }
            names(new_col) = nms
        }
        causal_cols(out)[[col]] = new_col
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
        new_col = value[which(cols[[col]] == old_names)]
        if (!is.null(names(cols[[col]]))) {
            names(new_col) = value[which(names(cols[[col]]) == old_names)]
        }
        causal_cols(out)[[col]] = new_col
    }

    out
}


# Printing -----------------------------------------------------------------

#' @importFrom pillar tbl_sum
#' @method tbl_sum causal_tbl
#' @export
tbl_sum.causal_tbl <- function(x, ...) {
    lines <- c("A causal_tbl " = pillar::dim_desc(x),
               NextMethod())
    lines[-2] # remove tbl line
}

#' @importFrom pillar tbl_format_header
#' @method tbl_format_header causal_tbl
#' @export
tbl_format_header.causal_tbl <- function(x, setup, ...) {
    default_header <- NextMethod()
    new_header <- paste0(
        pillar::style_subtle(cli::format_inline("# A {.cls causal_tbl}")),
        pillar::style_subtle(paste0(" [", pillar::dim_desc(x), "]"))
    )
    c(new_header, default_header[-1])
}

#' @importFrom pillar tbl_format_setup
#' @method tbl_format_setup causal_tbl
#' @export
tbl_format_setup.causal_tbl <- function(x, width, ..., n, max_extra_cols, max_footer_lines, focus) {
    NextMethod(focus=unique(unlist(causal_cols(x))))
}


#' @importFrom pillar ctl_new_pillar
#' @method ctl_new_pillar causal_tbl
#' @export
ctl_new_pillar.causal_tbl <- function(controller, x, width, ..., title = NULL) {
    out <- NextMethod()
    cols <- causal_cols(controller)
    matched_types = vapply(cols, function(y) match(title, y)[1], 0L)
    marker_type = names(which(!is.na(matched_types)))[1] # first match only
    marker = c(
        outcome="[out]", treatment="[trt]",
        panel_unit="[unit]", panel_time="[time]"
    )[marker_type]
    marker = if (length(marker) == 0 || is.na(marker)) {
        ""
    } else {
        pillar::style_subtle(marker)
    }

    pillar::new_pillar(list(
        marker = pillar::new_pillar_component(list(marker), width = nchar(marker)),
        title = out$title,
        type = out$type,
        data = out$data
    ))
}
