#' @describeIn indices Construct an `indices` list with minimal checks
#' @export
new_indices <- function(x = list()) {
    if (!is.list(x)) {
        cli_abort("{.arg x} must be a list.")
    }

    vctrs::new_vctr(x, class="indices")
}

#' Construct a list of indices
#'
#' An `indices` list is a list with integer vector entries which represent
#' indices in some other object (like a data frame). Generically it can
#' be used to represent a graph structure, such as an interference network
#' or a collection of matched objects.
#' The main feature of `indices` is that the index references are preserved
#' through slicing and reordering. Indices that no longer refer to elements
#' (because of subsetting) are set to NA.
#'
#' @param x
#' * For `indices()` and `new_indices()`: A list of indices
#' * For `is_indices()`: An object to test
#' * For `as_indices()`: An object to coerce
#'
#' @returns An `indices` object.
#'
#' @examples
#' idx <- indices(list(2, c(1, NA, 3), 2))
#' print(idx)
#' idx[1:2] # subsetting
#' idx[c(2, 1, 3)] # reordering
#' @export
indices <- function(x = list()) {
    # convert each element to an integer
    x <- relist(vctrs::vec_cast(unlist(x), integer(), x_arg="x"), x)
    new_indices(x)
}

#' @describeIn indices Return `TRUE` if an object is an `indices` list
#' @export
is_indices <- function(x) {
    inherits(x, "indices")
}

#' @describeIn indices Coerce an object to an `indices` list
#' @export
as_indices <- function(x) {
    vctrs::vec_cast(x, new_indices())
}


# printing
#' @export
format.indices <- function(x, ...) {
    vapply(vctrs::vec_data(x), format_index_line, "")
}
format_index_line <- function(y) {
    paste0("(", paste0(formatC(y[!is.na(y)]), collapse=","), ")")
}

# vctrs -------------------------------------------------------------------

#' @export
`[.indices` <- function(x, i) {
    lookup <- match(seq_along(x), i)
    out <- NextMethod()
    for (j in seq_along(out)) {
        out[[j]] = lookup[out[[j]]]
    }
    out
}

#' @importFrom vctrs vec_ptype_abbr
#' @method vec_ptype_abbr indices
#' @export
vec_ptype_abbr.indices <- function(x, ...) {
    "idx" # nocov
}

#' @importFrom vctrs vec_ptype2
#' @export
vec_ptype2.list.indices <- function(x, y, ...) list()
#' @export
vec_ptype2.indices.list <- function(x, y, ...) list()
#' @importFrom vctrs vec_cast
#' @export
vec_cast.list.indices <- function(x, y, ...) as.list(x)
#' @export
vec_cast.indices.list <- function(x, y, ...) indices(x)
