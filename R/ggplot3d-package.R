#' 3D Plots in the Grammar of Graphics
#'
#' Three-dimensional layers, z position scales and camera controls for ggplot2.
#' Panel scenes resolve visibility across layers for vector and raster output.
#'
#' @section Options:
#' \describe{
#'   \item{`ggplot3d.threads`}{Number of threads the scene compiler uses to
#'     find which pieces of a panel occlude each other. Defaults to every
#'     logical core, or 2 when `_R_CHECK_LIMIT_CORES_` is set. The compiled
#'     scene is the same for every thread count.}
#'   \item{`ggplot3d.cache_bytes`}{Memory, in bytes, kept for recently compiled
#'     scenes, so that redrawing a plot at the same size and view skips the
#'     compiler. Defaults to 256 MiB; 0 turns the cache off.}
#' }
#' @useDynLib ggplot3d, .registration = TRUE
#' @importFrom Rcpp sourceCpp
#' @importFrom utils head tail
#' @importFrom isoband isobands isolines
#' @keywords internal
"_PACKAGE"

utils::globalVariables("scene_compile_cpp")
