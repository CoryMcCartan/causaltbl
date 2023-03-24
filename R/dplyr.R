group_by.causal_tbl <- function(.data, ..., .add=FALSE) {
    reconstruct.causal_tbl(NextMethod(), data)
}
ungroup.causal_tbl <- function(.data, ...) {
    reconstruct.causal_tbl(NextMethod(), data)
}
rowwise.causal_tbl <- function(.data, ...) {
    reconstruct.causal_tbl(NextMethod(), data)
}

dplyr_reconstruct.causal_tbl <- function(data, template) {
    reconstruct.causal_tbl(data, template)
}

register_s3_dplyr <- function() {
    vctrs::s3_register("dplyr::dplyr_reconstruct", "causal_tbl")
    vctrs::s3_register("dplyr::group_by", "causal_tbl")
    vctrs::s3_register("dplyr::ungroup", "causal_tbl")
    vctrs::s3_register("dplyr::rowwise", "causal_tbl")
}
