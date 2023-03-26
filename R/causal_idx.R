#' @describeIn causal_idx Construct a `causal_idx` list with minimal checks
#' @export
new_causal_idx <- function(x = list()) {
    if (!is.list(x)) {
        cli_abort("{.arg x} must be a list.")
    }

    vctrs::new_vctr(x, class="causal_idx")
}

#' Construct a list of indices of type `causal_idx`
#'
#' A `causal_idx` list is a list with integer vector entries which represent
#' indices in some other object (like a data frame). Generically it can
#' be used to represent a graph structure, such as an interference network
#' or a collection of matched objects.
#' The main feature of `causal_idx` is that the index references are preserved
#' through slicing and reordering. Indices that no longer refer to elements
#' (because of subsetting) are set to NA.
#'
#' @param x
#' * For `causal_idx()` and `new_causal_idx()`: A list of indices
#' * For `is_causal_idx()`: An object to test
#' * For `as_causal_idx()`: An object to coerce
#'
#' @returns A `causal_idx` object. For `is_causal_idx()`, a logical value.
#'
#' @examples
#' idx <- causal_idx(list(2, c(1, NA, 3), 2))
#' print(idx)
#' idx[1:2] # subsetting
#' idx[c(2, 1, 3)] # reordering
#'
#' @order 1
#' @export
causal_idx <- function(x = list()) {
    # convert each element to an integer
    x <- utils::relist(vctrs::vec_cast(unlist(x), integer(), x_arg="x"), x)
    new_causal_idx(x)
}

#' @describeIn causal_idx Return `TRUE` if an object is an `causal_idx` list
#' @export
is_causal_idx <- function(x) {
    inherits(x, "causal_idx")
}

#' @describeIn causal_idx Coerce an object to an `causal_idx` list
#' @export
as_causal_idx <- function(x) {
    vctrs::vec_cast(x, new_causal_idx())
}


# printing
#' @export
format.causal_idx <- function(x, ...) {
    vapply(vctrs::vec_data(x), format_index_line, "")
}
format_index_line <- function(y) {
    paste0("(", paste0(formatC(y[!is.na(y)]), collapse=","), ")")
}

# vctrs -------------------------------------------------------------------

#' @export
`[.causal_idx` <- function(x, i) {
    lookup <- match(seq_along(x), i)
    out <- NextMethod()
    for (j in seq_along(out)) {
        out[[j]] = lookup[out[[j]]]
    }
    out
}

#' @importFrom vctrs vec_ptype_abbr
#' @method vec_ptype_abbr causal_idx
#' @export
vec_ptype_abbr.causal_idx <- function(x, ...) {
    "idx" # nocov
}

#' @importFrom vctrs vec_ptype2
#' @export
vec_ptype2.list.causal_idx <- function(x, y, ...) list() # nocov
#' @export
vec_ptype2.causal_idx.list <- function(x, y, ...) list()
#' @importFrom vctrs vec_cast
#' @export
vec_cast.list.causal_idx <- function(x, to, ...) as.list(x)
#' @export
vec_cast.causal_idx.list <- function(x, to, ...) causal_idx(x)
