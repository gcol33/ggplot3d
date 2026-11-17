utils::globalVariables(c("splat_covariance_cpp", "splat_resolve_cpp", "splat_project_cpp"))

#' Gaussian splats in three dimensions
#'
#' Draw point clouds with Gaussian footprints, or render Gaussian captures
#' read by [read_points_3d()]. Each row is one Gaussian.
#' @param mapping,data,...,show.legend,inherit.aes As in [geom_point_3d()].
#' @param sh_degree Maximum spherical-harmonic degree, from 0 to 3. `NULL`
#'   uses all available coefficients.
#' @details
#' `size` is an isotropic standard deviation in data units (default 0.02).
#' `scale_x`, `scale_y`, `scale_z` give anisotropic standard deviations;
#' `qw`, `qx`, `qy`, `qz` give a rotation quaternion, normalised internally.
#' Mapped `size` follows the usual size scale; use `I(size)` to keep data units.
#' The other covariance aesthetics are used directly.
#'
#' Unmapped properties are read from INRIA-format 3D Gaussian splatting
#' columns: `scale_0` through `scale_2` are exponentiated, `opacity` is
#' logistic-transformed, and `rot_0` through `rot_3` are normalised.
#' `f_dc_0` through `f_dc_2` and optional channel-major `f_rest_*`
#' coefficients give camera-dependent RGB colour. Explicit colour or fill
#' overrides these coefficients. Ordinary colour and fill scales and
#' `colour = I(hex)` work as for points.
#'
#' Raster, vector and hybrid panels use depth-composited EWA footprints
#' truncated at three standard deviations. Path tracing uses Gaussian
#' responses along rays with the colour as surface albedo. The viewer and
#' scene exports display coloured points. The stroke-only plotter omits
#' splats. Splats require an affine Cartesian frame and position scales.
#' @references
#' Kerbl et al. (2023). 3D Gaussian Splatting for Real-Time Radiance Field
#' Rendering. \doi{10.1145/3592433}.
#'
#' Zwicker et al. (2002). EWA Splatting. \doi{10.1109/TVCG.2002.1021576}.
#'
#' Moenne-Loccoz et al. (2024). 3D Gaussian Ray Tracing: Fast Tracing of
#' Particle Scenes. \doi{10.1145/3687934}.
#' @examples
#' d <- expand.grid(x = seq(-1, 1, length.out = 12),
#'                  y = seq(-1, 1, length.out = 12))
#' d$z <- with(d, 0.2 * cos(3 * x) * sin(3 * y))
#' ggplot2::ggplot(d, ggplot2::aes(x, y, z = z, colour = z)) +
#'   geom_splat_3d(size = 0.12) + coord_3d()
#' @return A ggplot2 layer.
#' @export
geom_splat_3d <- function(mapping = NULL, data = NULL, ..., sh_degree = NULL,
                          show.legend = NA, inherit.aes = TRUE) {
  if (!is.null(sh_degree) && (!is.numeric(sh_degree) || length(sh_degree) != 1L ||
      !is.finite(sh_degree) || !sh_degree %in% 0:3))
    stop("sh_degree must be NULL or an integer from 0 to 3.", call. = FALSE)
  make_layer_3d(GeomSplat3D, mapping, data, "identity", "identity",
    c(list(sh_degree = sh_degree), list(...)), show.legend, inherit.aes)
}

GeomSplat3D <- ggplot2::ggproto("GeomSplat3D", ggplot2::Geom,
  required_aes = c("x", "y", "z"),
  default_aes = ggplot2::aes(colour = "black", fill = NA, alpha = 1, size = 0.02,
    scale_x = NA_real_, scale_y = NA_real_, scale_z = NA_real_,
    qw = 1, qx = 0, qy = 0, qz = 0),
  draw_key = function(data, params, size) {
    if (!is.na(data$fill)) data$colour <- data$fill
    data$size <- 2; data$shape <- 19
    ggplot2::draw_key_point(data, params, size)
  }
)

splat_aesthetics_3d <- function(out, data, plot, params) {
  explicit <- union(names(out), names(params))
  for (axis in c("x", "y", "z"))
    if (!axis %in% explicit && axis %in% names(data)) out[[axis]] <- data[[axis]]
  for (k in 0:2) {
    name <- c("scale_x", "scale_y", "scale_z")[k + 1L]
    raw <- paste0("scale_", k)
    if (!any(c(name, "size") %in% explicit) && raw %in% names(data)) out[[name]] <- exp(data[[raw]])
  }
  for (k in 0:3) {
    name <- c("qw", "qx", "qy", "qz")[k + 1L]
    raw <- paste0("rot_", k)
    if (!name %in% explicit && raw %in% names(data)) out[[name]] <- data[[raw]]
  }
  if (!"alpha" %in% explicit && "opacity" %in% names(data)) out$alpha <- I(stats::plogis(data$opacity))
  if (!any(c("colour", "fill") %in% explicit) && all(paste0("f_dc_", 0:2) %in% names(data))) {
    rest <- intersect(paste0("f_rest_", 0:44), names(data))
    if (!length(rest) %in% c(0L, 9L, 24L, 45L) ||
        (length(rest) > 0L && !identical(rest, paste0("f_rest_", seq_len(length(rest)) - 1L))))
      stop("SH coefficients must be complete channel-major degrees 0 through 3.", call. = FALSE)
    for (name in c(paste0("f_dc_", 0:2), rest)) out[[paste0(".splat_", name)]] <- data[[name]]
  }
  out
}

splat_position_check_3d <- function(position, scales) {
  for (axis in c("x", "y", "z")) {
    trans <- scales[[axis]]$get_transformation()$name
    if (!is.null(trans) && !trans %in% c("identity", "reverse"))
      stop("Gaussian splats require affine position scales.", call. = FALSE)
  }
}

emit_splat_3d <- function(data, params, panel_params) {
  if (identical(panel_params$coord_params$backend$type, "raymarch"))
    stop("Gaussian splats use Euclidean cameras; select raster, vector, hybrid or pathtrace.", call. = FALSE)
  if (!isTRUE(frame_of_3d(panel_params)$identity) || !is.null(panel_params$space_context))
    stop("Gaussian splats require an affine Cartesian frame.", call. = FALSE)
  if (params$occlusion != "depth" || length(params$clip_planes))
    stop("Gaussian splats require depth occlusion and the panel clipping box.", call. = FALSE)
  n <- nrow(data)
  scales <- as.matrix(data[c("scale_x", "scale_y", "scale_z")])
  for (k in 1:3) scales[is.na(scales[, k]), k] <- data$size[is.na(scales[, k])]
  quaternion <- as.matrix(data[c("qw", "qx", "qy", "qz")])
  span <- vapply(panel_params$ranges[c("x", "y", "z")], diff, numeric(1))
  for (axis in c("x", "y", "z"))
    if (identical(panel_params[[axis]]$get_transformation()$name, "reverse")) span[axis] <- -span[axis]
  covariance <- splat_covariance_cpp(scales, quaternion, 1 / span)
  colour <- data$colour
  take <- !is.na(data$fill); colour[take] <- data$fill[take]
  rgba <- t(colour_channels_3d(colour, TRUE) / 255)
  rgba[, 4L] <- rgba[, 4L] * data$alpha * !is.na(colour)
  sh_names <- paste0(".splat_f_dc_", 0:2)
  sh <- matrix(numeric(), n, 0L)
  if (all(sh_names %in% names(data))) {
    rest <- intersect(paste0(".splat_f_rest_", 0:44), names(data))
    available <- as.integer(sqrt(length(rest) / 3L + 1L) - 1L)
    degree <- min(params$sh_degree %or3d% available, available)
    count <- (degree + 1L)^2L
    sh <- matrix(0, n, count * 3L)
    for (k in 0:2) {
      sh[, k * count + 1L] <- data[[sh_names[k + 1L]]]
      if (count > 1L) sh[, k * count + 1L + seq_len(count - 1L)] <-
        as.matrix(data[rest[k * (length(rest) / 3L) + seq_len(count - 1L)]])
    }
  }
  packet <- new_scene_packet(panel_id = panel_params$panel_id)
  attr(packet, "splats") <- list(list(position = as.matrix(data[c("x", "y", "z")]),
    covariance = covariance, rgba = rgba, sh = sh, span = span, layer_id = data$.layer[1L]))
  attr(packet, "model_space") <- TRUE
  grid::gTree(packet = packet, cl = "scene_layer_grob")
}

register_geom_3d(GeomSplat3D, emit_splat_3d, prepare_aesthetics = splat_aesthetics_3d,
  position_check = splat_position_check_3d, parameters = list(sh_degree = NULL), tween = "row")

# Point representations share the ordinary marker export and viewer paths.
splat_points_packet_3d <- function(packet, camera, viewport) {
  specs <- attr(packet, "splats")
  if (!length(specs)) return(packet)
  attr(packet, "splats") <- NULL
  points <- lapply(specs, function(spec) {
    resolved <- splat_resolve_cpp(spec, camera, viewport)
    rgba <- resolved$rgba
    data <- data.frame(x = spec$position[, 1L], y = spec$position[, 2L], z = spec$position[, 3L],
      colour = grDevices::rgb(rgba[, 1L], rgba[, 2L], rgba[, 3L]), alpha = rgba[, 4L],
      shape = 19, size = 1, stroke = 0, fill = NA_character_,
      .source = seq_len(nrow(rgba)), .layer = spec$layer_id)
    emit_points_3d(data, list(occlusion = "depth", hidden = "hide"), list(panel_id = packet$panel_id))
  })
  merge_scene_packets(c(list(packet), points), packet$panel_id)
}
