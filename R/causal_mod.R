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
#' A `causal_mod` object can be treated and will behave as a numeric vector of
#' fitted values. The original model object is stored in the `"model"`
#' attribute, which can be accessed with [get_model_fit()]. The `idx` indices
#' (which may have been computed automatically) are stored in the `"idx"`
#' attribute, which can be accessed with [get_model_idx()].
#'
#' A `causal_mod` object additionally supports forwarding generic model
#' functions. So if you call `fitted()`, `predict()`, etc. on a `causal_mod`,
#' object, you will get the same result as if you had called these functions on
#' the original model object.
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
#' @returns
#'  * For `causal_mod()` and `new_causal_mod()`: A `causal_mod` object.
#'  * For `is_causal_mod()`, a logical value.
#'
#' @examples
#' m <- lm(yield ~ block + N*P*K, data=npk)
#' causal_mod(m)
#'
#' d <- rbind(NA, npk)
#' m_mis <- lm(yield ~ block + N*P*K, data=d)
#' fitted(m_mis) # length doesn't match rows of `d`
#' causal_mod(m_mis) # NA for missing value
#' attr(causal_mod(m_mis), "idx")
#' attr(causal_mod(m_mis), "mod")
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
    if (!is.numeric(fitted)) {
        cli_abort("Fitted values for {.arg x} must be numeric.")
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

#' @describeIn causal_mod Return `TRUE` if an object is a `causal_mod`
#' @export
is_causal_mod <- function(x) {
    inherits(x, "causal_mod")
}


#' Extract the model object and observation indices from a `causal_mod`
#'
#' @param x The `causal_mod` object
#'
#' @returns
#' * For `get_model_fit()`: A fitted model object
#' * For `get_model_idx()`: An integer vector of indices
#'
#' @examples
#' m <- lm(yield ~ block + N*P*K, data=npk)
#' x <- causal_mod(m)
#' identical(get_model_fit(x), m) # TRUE
#' get_model_idx(x)
#'
#' @export
get_model_fit <- function(x) {
    if (!is_causal_mod(x)) {
        cli_abort("{.arg x} must be a {.cls causal_mod}.", call=parent.frame())
    }
    attr(x, "model")
}

#' @rdname get_model_fit
#' @export
get_model_idx <- function(x) {
    if (!is_causal_mod(x)) {
        cli_abort("{.arg x} must be a {.cls causal_mod}.", call=parent.frame())
    }
    attr(x, "idx")
}


# printing
# (tests skipped on CI)
# nocov start
#' @export
format.causal_mod <- function(x, ...) {
    formatC(vctrs::vec_data(x))
}
#' @export
str.causal_mod <- function(object, max.level=2, ...) {
    NextMethod(max.level=max.level, ...)
}
# nocov end

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


# model generics -------------------------------------------------------------------

#' @importFrom stats fitted
#' @export
fitted.causal_mod <- function(object, ...) {
    fitted(get_model_fit(object), ...)
}
#' @importFrom stats residuals
#' @export
residuals.causal_mod <- function(object, ...) {
    residuals(get_model_fit(object), ...)
}
#' @importFrom stats predict
#' @export
predict.causal_mod <- function(object, ...) {
    predict(get_model_fit(object), ...)
}
#' @importFrom stats simulate
#' @export
simulate.causal_mod <- function(object, nsim = 1, seed = NULL, ...) {
    simulate(get_model_fit(object), nsim=nsim, seed=seed, ...)
}

#' @export
summary.causal_mod <- function(object, ...) {
    summary(get_model_fit(object), ...)
}
#' @importFrom stats vcov
#' @export
vcov.causal_mod <- function(object, ...) {
    vcov(get_model_fit(object), ...)
}
#' @importFrom stats nobs
#' @export
nobs.causal_mod <- function(object, ...) {
    nobs(get_model_fit(object), ...)
}
#' @importFrom stats logLik
#' @export
logLik.causal_mod <- function(object, ...) {
    logLik(get_model_fit(object), ...)
}
#' @importFrom stats confint
#' @export
confint.causal_mod <- function(object, parm, level=0.95, ...) {
    confint(get_model_fit(object), parm=parm, level=level, ...)
}
#' @importFrom stats formula
#' @export
formula.causal_mod <- function(x,  ...) {
    formula(get_model_fit(x), ...)
}

# don't want to Suggest broom for now
# nocov start
#' @importFrom generics tidy
#' @export
tidy.causal_mod <- function(x,  ...) {
    tidy(get_model_fit(x), ...)
}
#' @importFrom generics glance
#' @export
glance.causal_mod <- function(x,  ...) {
    glance(get_model_fit(x), ...)
}
# nocov end
