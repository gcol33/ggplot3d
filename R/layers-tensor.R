tensor_shape_3d <- function(values, gamma) {
  total <- sum(values)
  cl <- if (total > 0) (values[1L] - values[2L]) / total else 0
  cp <- if (total > 0) 2 * (values[2L] - values[3L]) / total else 0
  linear <- cl >= cp
  c(alpha = (1 - if (linear) cp else cl)^gamma,
    beta = (1 - if (linear) cl else cp)^gamma, linear = linear)
}

superquadric_mesh_3d <- function(shape, detail) {
  unit <- glyph_mesh_3d("sphere", detail)
  alpha <- shape[1L];beta <- shape[2L]
  if (alpha == 1 && beta == 1) return(unit)
  polar <- if (shape[3L]) 1L else 3L
  equator <- setdiff(1:3, polar)
  v <- unit$vertices
  radius <- sqrt(rowSums(v[, equator, drop = FALSE]^2))
  signed_power <- function(x, exponent) sign(x) * abs(x)^exponent
  v[, polar] <- signed_power(v[, polar], beta)
  for (j in equator) v[, j] <- signed_power(ifelse(radius > 0, unit$vertices[, j] / radius, 0), alpha) * radius^beta
  new_glyph_mesh_3d(v, orient_outward_3d(v, unit$faces))
}

tensor_geometry_3d <- function(data, params) {
  scene_assert(is.list(data$tensor) && length(data$tensor) == nrow(data), "tensor must be a list-column of 3 x 3 matrices")
  maps <- array(NA_real_, c(3L, 3L, nrow(data)))
  shapes <- matrix(NA_real_, nrow(data), 3L)
  for (i in seq_len(nrow(data))) {
    s <- data$tensor[[i]]
    if (is.null(s) || (length(s) == 1L && is.na(s))) next
    scene_assert(is.numeric(s) && identical(dim(s), c(3L, 3L)), "tensor must be a list-column of 3 x 3 matrices")
    if (any(!is.finite(s))) next
    e <- symmetric_eigen_3d(s, "tensor")
    if (det(e$vectors) < 0) e$vectors[, 3L] <- -e$vectors[, 3L]
    maps[, , i] <- e$vectors %*% diag(e$values * params$scale, 3L)
    shapes[i, ] <- if (params$glyph == "ellipsoid") c(1, 1, 1) else tensor_shape_3d(e$values, params$gamma)
  }
  keys <- apply(shapes, 1L, function(s) paste(formatC(s, digits = 17L, format = "g"), collapse = ":"))
  units <- lapply(which(!duplicated(keys)), function(i) {
    if (anyNA(shapes[i, ])) glyph_mesh_3d("sphere", params$detail) else superquadric_mesh_3d(shapes[i, ], params$detail)
  })
  list(maps = maps, units = units, index = match(keys, unique(keys)))
}

GeomTensor3D <- ggplot2::ggproto("GeomTensor3D", ggplot2::Geom,
  required_aes = c("x", "y", "z", "tensor"),
  default_aes = solid_defaults_3d(),
  optional_aes = c("xmin", "xmax", "ymin", "ymax", "zmin", "zmax"),
  draw_key = draw_key_glyph_3d,
  setup_data = function(data, params) {
    geometry <- tensor_geometry_3d(data, params)
    do.call(rbind, lapply(seq_len(nrow(data)), function(i) glyph_limits_3d(data[i, , drop = FALSE],
      geometry$maps[, , i, drop = FALSE], geometry$units[[geometry$index[i]]], all(is.finite(geometry$maps[, , i])))))
  }
)

emit_tensors_3d <- function(data, params, panel_params) {
  geometry <- tensor_geometry_3d(data, params)
  emit_glyph_instances_3d(data, normalise_maps_3d(geometry$maps, panel_params), rep("data", nrow(data)),
    geometry$units, params, panel_params, geometry$index)
}
register_geom_3d(GeomTensor3D, emit_tensors_3d,
  parameters = list(glyph = "ellipsoid", gamma = 3, scale = 1, detail = 2L), tween = "row")

#' Symmetric tensor glyphs
#'
#' Eigen-decomposes each symmetric positive semidefinite tensor. Its eigenvalues
#' times `scale` are the three semi-axis lengths, in scale-transformed position
#' units, and its eigenvectors give their directions. Kindlmann superquadrics
#' vary continuously between spherical, linear and planar tensor shapes.
#' `gamma = 0` gives ellipsoids; increasing gamma sharpens anisotropic shapes.
#' Position scales expand to contain the tessellated glyphs.
#' @inheritParams geom_glyph_3d
#' @param glyph `"ellipsoid"` or `"superquadric"`.
#' @param gamma Nonnegative edge-sharpness exponent.
#' @param scale Nonnegative multiplier of tensor eigenvalues.
#' @return A ggplot2 layer.
#' @references Kindlmann (2004). Superquadric tensor glyphs.
#'   \doi{10.2312/VisSym/VisSym04/147-154}
#' @examples
#' d <- data.frame(x = 1:3, y = 0, z = 0)
#' d$tensor <- list(diag(c(1, 0.2, 0.2)), diag(c(1, 1, 0.2)), diag(3))
#' ggplot2::ggplot(d, ggplot2::aes(x, y, z = z, tensor = tensor)) +
#'   geom_tensor_3d(glyph = "superquadric", scale = 0.3) + coord_3d(aspect = "data")
#' @export
geom_tensor_3d <- function(mapping = NULL, data = NULL, stat = "identity", position = "identity", ...,
                           glyph = c("ellipsoid", "superquadric"), gamma = 3, scale = 1,
                           detail = 2L, shade = TRUE, na.rm = FALSE, show.legend = NA, inherit.aes = TRUE) {
  for (name in c("gamma", "scale")) {
    value <- get(name)
    scene_assert(is.numeric(value) && length(value) == 1L && is.finite(value) && value >= 0, paste(name, "must be nonnegative"))
  }
  make_layer_3d(GeomTensor3D, mapping, data, stat, position,
    stats_params_3d(list(glyph = match.arg(glyph), gamma = gamma, scale = scale,
      detail = check_detail_3d(detail), shade = shade, na.rm = na.rm), list(...)), show.legend, inherit.aes)
}
