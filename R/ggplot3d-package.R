#' 3D Plots in the Grammar of Graphics
#'
#' Three-dimensional layers, z position scales and camera controls for ggplot2.
#' Panel scenes resolve visibility across layers for vector and raster output.
#' When attaching ggcube or ggrgl together with ggplot3d, use qualified calls
#' such as `ggplot3d::coord_3d()` for names shared by the packages.
#'
#' @section Options:
#' \describe{
#'   \item{`ggplot3d.threads`}{Number of threads the scene compiler and the
#'     raster backend use. The default `0` uses every logical core; at most 2
#'     are used when `_R_CHECK_LIMIT_CORES_` is set. The compiled scene is the
#'     same for every thread count. Accepts representable nonnegative integers;
#'     see [backend_3d()].}
#'   \item{`ggplot3d.cache_bytes`}{Memory, in bytes, kept for recently compiled
#'     scenes, so that redrawing a plot at the same size and view skips the
#'     compiler. Accepts one nonnegative number or `Inf`. Defaults to 256 MiB;
#'     0 turns the cache off. See [coord_3d()].}
#'   \item{`ggplot3d.max.overlaps`}{Default `max.overlaps` of
#'     [geom_text_repel_3d()] and [geom_label_repel_3d()]: a label that
#'     overlaps more labels and points than this where it starts is not drawn.
#'     Accepts one nonnegative number or `Inf`. Defaults to 10.}
#'   \item{`ggplot3d.vector_markers`}{Maximum markers per vector panel: one
#'     nonnegative whole number or `Inf` for unlimited. Default 50000.
#'     See [geom_point_3d()].}
#'   \item{`ggplot3d.shading_tolerance`}{One positive finite number for the
#'     encoded sRGB tolerance of vector shading. Default `2 / 255`.
#'     See [geom_surface_3d()].}
#'   \item{`ggplot3d.smooth_vector`}{`"auto"` (default) or `"subdivide"`
#'     for smooth faces in vector output. See [geom_surface_3d()].}
#'   \item{`ggplot3d.smooth_dpi`}{One positive finite number for the minimum
#'     resolution of smooth shading images. Default 150. See [geom_surface_3d()].}
#'   \item{`ggplot3d.quadric_segments`}{One representable integer of at least
#'     3 for quadric meshes in exports and shadows. Default 32.
#'     See [geom_sphere_3d()].}
#' }
#' @useDynLib ggplot3d, .registration = TRUE
#' @importFrom Rcpp sourceCpp
#' @importFrom utils head tail
#' @importFrom isoband isobands isolines
#' @keywords internal
"_PACKAGE"

utils::globalVariables("scene_compile_cpp")
