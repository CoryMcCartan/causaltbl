#' @describeIn causal_mod Construct a `causal_mod` with minimal checks
#' @export
new_causal_mod <- function(x = list(), fitted = double(0), idx = seq_along(fitted)) {
    if (is.atomic(x)) {
        cli_abort("{.arg x} cannot be an atomic type.")
    }

    names(fitted) <- NULL
    vctrs::new_vctr(fitted[idx], model=x, idx=idx, class="causal_mod")
}

#' Construct a fitted model column
#'
#' The `causal_mod` class acts like a vector of fitted values, but it also
#' stores the fitted model for later predictions and summaries, and keeps track
#' of observations that have been dropped (e.g. due to missingness) or subsetted.
#'
#' @param x
#' * For `causal_mod()` and `new_causal_mod()`: A fitted model object.
#'   For `causal_mod()` this should support the [fitted()] generic.
#' * For `is_causal_mod()`: An object to test
#' @param idx A set of indices that connect observations fed into the model with
#'   fitted values. Should contain values from 1 to the number of fitted values,
#'   and may contain `NA` values for observations with no corresponding fitted
#'   value (such as those with missing data). Defaults to a sequence over the
#'   fitted values, with `NA`s determined by [na.action()].
#' @param fitted A vector of fitted values.  Extracted automatically from the
#'   model object in `causal_mod()`.
#'
#' @returns A `causal_mod` object. For `is_causal_mod()`, a logical value.
#'
#' @examples
#' m <- lm(yield ~ block + N*P*K, data=npk)
#' causal_mod(m)
#'
#' d <- rbind(NA, npk)
#' m_mis <- lm(yield ~ block + N*P*K, data=d)
#' causal_mod(m_mis) # NA for missing value
#'
#' @order 1
#' @export
causal_mod <- function(x, idx = NULL) {
    if (is.atomic(x)) {
        cli_abort("{.arg x} cannot be an atomic type.")
    }

    fitted <- stats::fitted(x)
    if (is.null(fitted)) {
        cli_abort("{.arg x} does not have a {.fn fitted} method.")
    }
    if (is.null(idx)) {
        nas <- stats::na.action(x)
        if (is.null(nas)) {
            idx = seq_along(fitted)
        } else {
            idx = integer(length(fitted) + length(nas))
            idx[nas] = NA_integer_
            idx[-nas] = seq_along(fitted)
        }
    }

    new_causal_mod(x, fitted, idx)
}

#' @describeIn causal_mod Return `TRUE` if an object is an `causal_mod` list
#' @export
is_causal_mod <- function(x) {
    inherits(x, "causal_mod")
}


# printing
#' @export
format.causal_mod <- function(x, ...) {
    formatC(vctrs::vec_data(x))
}
#' @export
str.causal_mod <- function(object, max.level=2, ...) {
    NextMethod(max.level=max.level, ...) # nocov
}

# vctrs -------------------------------------------------------------------

#' @export
`[.causal_mod` <- function(x, i) {
    out <- NextMethod()
    attr(out, "idx") <- attr(out, "idx")[i]
    out
}

#' @importFrom vctrs vec_ptype_abbr
#' @method vec_ptype_abbr causal_mod
#' @export
vec_ptype_abbr.causal_mod <- function(x, ...) {
    "mod" # nocov
}

#' @importFrom vctrs vec_ptype2
#' @export
vec_ptype2.double.causal_mod <- function(x, y, ...) double() # nocov
#' @export
vec_ptype2.causal_mod.double <- function(x, y, ...) double()
#' @importFrom vctrs vec_cast
#' @export
vec_cast.double.causal_mod <- function(x, to, ...) vctrs::vec_data(x)
