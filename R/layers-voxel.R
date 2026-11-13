utils::globalVariables("voxel_faces_cpp")

voxel_axes_3d <- c(x = "width", y = "depth", z = "height")

GeomVoxel3D <- ggplot2::ggproto("GeomVoxel3D", ggplot2::Geom,
  required_aes = c("x", "y", "z"), default_aes = solid_defaults_3d(),
  optional_aes = c("xmin", "xmax", "ymin", "ymax", "zmin", "zmax"),
  draw_key = ggplot2::draw_key_polygon,
  extra_params = c("na.rm", "width", "depth", "height", "greedy"),
  setup_data = function(data, params) {
    for (axis in names(voxel_axes_3d)) {
      lo <- paste0(axis, "min"); hi <- paste0(axis, "max")
      if (!is.null(data[[lo]]) && !is.null(data[[hi]])) next
      size <- params[[voxel_axes_3d[[axis]]]]
      if (!is.null(size) && (length(size) != 1L || !is.finite(size) || size <= 0)) stop("width, depth and height must be positive scalars.", call. = FALSE)
      size <- size %or3d% unname(vapply(split(data[[axis]], data$PANEL), lattice_spacing_3d, numeric(1))[as.character(data$PANEL)])
      data[[lo]] <- data[[axis]] - size / 2; data[[hi]] <- data[[axis]] + size / 2
    }
    data
  }
)

emit_voxels_3d <- function(data, params, panel_params) {
  data <- data[valid_rows_3d(data, params, c("x", "y", "z", "xmin", "xmax", "ymin", "ymax", "zmin", "zmax")), , drop = FALSE]
  if (!nrow(data)) return(new_scene_packet(panel_id = panel_params$panel_id))
  axes <- names(voxel_axes_3d)
  size <- vapply(axes, function(a) stats::median(data[[paste0(a, "max")]] - data[[paste0(a, "min")]]), numeric(1))
  low <- vapply(axes, function(a) min(data[[paste0(a, "min")]]), numeric(1))
  cell <- vapply(axes, function(a) as.integer(round((data[[paste0(a, "min")]] - low[[a]]) / size[[a]])), integer(nrow(data)))
  cell <- matrix(cell, ncol = 3L)
  dims <- apply(cell, 2L, max) + 1L
  index <- 1L + cell[, 1L] + dims[1L] * (cell[, 2L] + dims[2L] * cell[, 3L])
  if (anyDuplicated(index)) stop("Voxel cells must be unique within a panel.", call. = FALSE)
  if (prod(as.double(dims)) > 2^28) stop("The voxel lattice is too large; check that voxel centres lie on a regular grid.", call. = FALSE)
  styles <- intersect(c("fill", "colour", "alpha", "linewidth", "linetype"), names(data))
  key <- do.call(paste, c(lapply(data[styles], as.character), sep = "\r"))
  material <- match(key, unique(key))
  first <- match(seq_len(max(material)), material)
  opacity <- (data$alpha[first] %or3d% rep(1, length(first)))
  opacity[is.na(opacity)] <- 1
  fill_alpha <- ifelse(is.na(data$fill[first]), 1, grDevices::col2rgb(data$fill[first], alpha = TRUE)[4L, ] / 255)
  translucent <- opacity * fill_alpha < 1
  lattice <- integer(prod(dims)); lattice[index] <- material
  rect <- voxel_faces_cpp(as.integer(dims), lattice, c(FALSE, translucent), !isFALSE(params$greedy))
  q <- nrow(rect)
  if (!q) return(new_scene_packet(panel_id = panel_params$panel_id))
  corner_u <- rbind(rect[, "u0"], rect[, "u1"], rect[, "u1"], rect[, "u0"])
  corner_v <- rbind(rect[, "v0"], rect[, "v0"], rect[, "v1"], rect[, "v1"])
  # Faces on the negative side run the other way round so all face outwards.
  back <- rect[, "side"] < 0
  corner_u[, back] <- corner_u[4:1, back]; corner_v[, back] <- corner_v[4:1, back]
  lattice_point <- matrix(0L, 4L * q, 3L)
  for (d in 0:2) {
    rows <- rep(rect[, "axis"] == d, each = 4L)
    lattice_point[rows, d + 1L] <- rep(rect[rect[, "axis"] == d, "plane"], each = 4L)
    lattice_point[rows, (d + 1L) %% 3L + 1L] <- as.vector(corner_u[, rect[, "axis"] == d])
    lattice_point[rows, (d + 2L) %% 3L + 1L] <- as.vector(corner_v[, rect[, "axis"] == d])
  }
  corners <- data[first[rep(rect[, "material"], each = 4L)], , drop = FALSE]
  for (a in 1:3) corners[[axes[a]]] <- low[[a]] + lattice_point[, a] * size[[a]]
  corners$.vertex <- 1L + lattice_point[, 1L] + (dims[1L] + 1L) * (lattice_point[, 2L] + (dims[2L] + 1L) * lattice_point[, 3L])
  faces <- matrix(seq_len(4L * q), ncol = 4L, byrow = TRUE)
  edges <- cbind(as.vector(t(faces)), as.vector(t(faces[, c(2L, 3L, 4L, 1L), drop = FALSE])))
  edges <- unique_edges_3d(edges, corners$.vertex, corners$group)
  packet_from_faces_edges_3d(corners, faces, edges, params, panel_params)
}

register_geom_3d(GeomVoxel3D, emit_voxels_3d,
  parameters = list(height = NULL, greedy = TRUE), tween = "row")

#' Voxels
#'
#' Draws one box per row, centred on x, y, z. Box sizes are `width`, `depth`
#' and `height` along x, y and z, by default the smallest spacing between
#' distinct centres in each panel; explicit xmin/xmax, ymin/ymax and zmin/zmax
#' take precedence. All voxels of a panel share one lattice.
#'
#' Only faces that can be seen are drawn: a face between two filled voxels is
#' kept only when the voxel behind it is translucent (alpha below one) and
#' differs in fill, colour, alpha, linewidth or linetype. With `greedy = TRUE`
#' the visible faces of one material are merged, per lattice slice and
#' direction, into maximal rectangles (greedy meshing), so the visibility
#' engine receives far fewer polygons. Merged rectangles meet smaller ones in
#' T-junctions; vector output covers the edges between opaque faces with the
#' geometric union of the faces, which does not depend on shared vertices.
#' @param mapping,data,stat,position,...,na.rm,show.legend,inherit.aes Standard layer arguments.
#' @param width,depth,height Voxel sizes along x, y and z.
#' @param greedy Whether to merge coplanar faces of one material.
#' @param shade Whether to apply lighting.
#' @return A ggplot2 layer.
#' @references Lysenko (2012). Meshing in a Minecraft game.
#'   <https://0fps.net/2012/06/30/meshing-in-a-minecraft-game/>
#' @export
#' @examples
#' d <- expand.grid(x = 1:4, y = 1:4, z = 1:4)
#' d <- d[(d$x - 2.5)^2 + (d$y - 2.5)^2 + (d$z - 2.5)^2 < 4, ]
#' d$class <- ifelse(d$z > 2, "top", "bottom")
#' ggplot2::ggplot(d, ggplot2::aes(x, y, z = z, fill = class)) +
#'   geom_voxel_3d() + coord_3d()
geom_voxel_3d <- function(mapping = NULL, data = NULL, stat = "identity", position = "identity", ...,
                          width = NULL, depth = NULL, height = NULL, greedy = TRUE, shade = TRUE,
                          na.rm = FALSE, show.legend = NA, inherit.aes = TRUE) {
  make_layer_3d(GeomVoxel3D, mapping, data, stat, position,
    c(list(width = width, depth = depth, height = height, greedy = greedy, shade = shade, na.rm = na.rm), list(...)),
    show.legend, inherit.aes)
}

StatBin3D <- ggplot2::ggproto("StatBin3D", ggplot2::Stat,
  required_aes = c("x", "y", "z"), optional_aes = "weight",
  default_aes = ggplot2::aes(fill = ggplot2::after_stat(count)),
  compute_group = function(data, scales, bins = 10L, binwidth = NULL) {
    if (any(vapply(scales[c("x", "y", "z")], function(s) !is.null(s) && s$is_discrete(), logical(1)))) stop("stat_bin_3d() requires continuous positions.", call. = FALSE)
    axes <- c("x", "y", "z")
    args <- check_bin_args_3d(bins, binwidth, axes)
    breaks <- lapply(seq_along(axes), function(a)
      bin_breaks_3d(range(data[[axes[a]]], finite = TRUE), args$bins[a], args$binwidth[a]))
    index <- vapply(seq_along(axes), function(a) findInterval(data[[axes[a]]], breaks[[a]], rightmost.closed = TRUE, all.inside = TRUE), integer(nrow(data)))
    index <- matrix(index, ncol = 3L)
    key <- paste(index[, 1L], index[, 2L], index[, 3L])
    weight <- data$weight %or3d% rep(1, nrow(data))
    count <- rowsum(weight, key, reorder = FALSE)[, 1L]
    cells <- index[match(names(count), key), , drop = FALSE]
    out <- data.frame(count = unname(count))
    for (a in seq_along(axes)) {
      b <- breaks[[a]]; i <- cells[, a]
      out[[paste0(axes[a], "min")]] <- b[i]; out[[paste0(axes[a], "max")]] <- b[i + 1L]
      out[[axes[a]]] <- (b[i] + b[i + 1L]) / 2
    }
    volume <- (out$xmax - out$xmin) * (out$ymax - out$ymin) * (out$zmax - out$zmin)
    out$density <- out$count / sum(out$count) / volume
    out$ncount <- out$count / max(out$count)
    out
  }
)

#' Three-dimensional bin counts
#'
#' Divides the x, y and z ranges into `bins` equal intervals (or intervals of
#' `binwidth` aligned to multiples of it) and counts observations, optionally
#' weighted, in each occupied box. Computed variables are `count`, `density`
#' (count divided by the total and by the box volume) and `ncount` (count
#' relative to the largest). Empty boxes are not returned, so they draw as
#' empty space.
#' @inheritParams geom_voxel_3d
#' @param geom Geom used to draw the boxes.
#' @param bins Number of bins, scalar or one per axis.
#' @param binwidth Bin widths, scalar or one per axis; overrides bins.
#' @return A ggplot2 layer.
#' @export
#' @examples
#' set.seed(1)
#' d <- data.frame(x = rnorm(300), y = rnorm(300), z = rnorm(300))
#' ggplot2::ggplot(d, ggplot2::aes(x, y, z = z)) +
#'   stat_bin_3d(bins = 6, alpha = 0.6) + coord_3d()
stat_bin_3d <- function(mapping = NULL, data = NULL, geom = GeomVoxel3D, position = "identity", ...,
                        bins = 10L, binwidth = NULL, greedy = TRUE, shade = TRUE, na.rm = FALSE,
                        show.legend = NA, inherit.aes = TRUE) {
  make_layer_3d(geom, mapping, data, StatBin3D, position,
    c(list(bins = bins, binwidth = binwidth, greedy = greedy, shade = shade, na.rm = na.rm), list(...)),
    show.legend, inherit.aes)
}
