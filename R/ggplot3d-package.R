#' 3D Plots in the Grammar of Graphics
#'
#' Internal columnar scene construction and native projection for 3D panels.
#' @useDynLib ggplot3d, .registration = TRUE
#' @importFrom Rcpp sourceCpp
#' @keywords internal
"_PACKAGE"

utils::globalVariables("scene_compile_cpp")
