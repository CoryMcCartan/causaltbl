set_outcome <- function(x, .outcome) {
    attr(x, "causal_cols")[["outcome"]] <- .outcome
}

get_outcome <- function(x) {
    attr(x, "causal_cols")[["outcome"]]
}

set_treatment <- function(x, .treatment) {
    attr(x, "causal_cols")[["treatment"]] <- .treatment
}

get_treatment <- function(x) {
    attr(x, "causal_cols")[["treatment"]]
}
