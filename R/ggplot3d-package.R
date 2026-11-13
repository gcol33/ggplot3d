#' 3D Plots in the Grammar of Graphics
#'
#' Three-dimensional layers, z position scales and camera controls for ggplot2.
#' Panel scenes resolve visibility across layers for vector and raster output.
#' @useDynLib ggplot3d, .registration = TRUE
#' @importFrom Rcpp sourceCpp
#' @importFrom utils head tail
#' @importFrom isoband isobands isolines
#' @keywords internal
"_PACKAGE"

utils::globalVariables("scene_compile_cpp")
