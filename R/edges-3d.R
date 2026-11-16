#' Mesh lines over surfaces
#'
#' `edges_3d()` draws the edges of a surface's cells over its faces, passed
#' as the `edges` argument of a surface or mesh layer, or to
#' [coord_3d()] for every such layer that sets none. The cells of a surface on
#' a grid ([geom_surface_3d()], the surface stats, [geom_parametric_3d()],
#' [geom_spatraster_3d()]) are the quadrilaterals between neighbouring grid
#' points, so the lines run along the grid and leave out the diagonals that
#' split each cell into triangles; the cells of a mesh are its polygons; a
#' Delaunay surface has triangles as cells. `every` keeps every n-th grid line
#' and the last one, as the strides of matplotlib's `plot_surface()` or
#' Mathematica's `Mesh -> n` thin a dense mesh. The lines are strokes of the
#' scene, hidden by nearer surfaces like any other line.
#'
#' Thin light lines with some transparency over a dark fill trace the grid
#' without hiding the shading; dark lines over a light fill give the look of
#' MATLAB's `surf()`.
#' @param type `"cells"` for the edges of the cells, `"triangles"` for every
#'   triangle edge.
#' @param every Positive integer stride of the grid lines: one value for both
#'   directions or two, for the lines along x (or u) and along y (or v). A
#'   stride above 1 needs a surface on a grid.
#' @param colour,linewidth,linetype,alpha Stroke of the lines.
#' @param hidden Hidden stroke policy of the lines: `"hide"`, `"dashed"` or
#'   `"show"`.
#' @return An edges specification for the `edges` argument of
#'   [geom_surface_3d()], [geom_mesh_3d()], the surface stats and [coord_3d()].
#' @seealso [outline_3d()] for silhouettes, creases and boundary lines.
#' @examples
#' d <- expand.grid(x = seq(-2, 2, length.out = 36), y = seq(-2, 2, length.out = 36))
#' d$z <- with(d, exp(-(x^2 + y^2)))
#' ggplot2::ggplot(d, ggplot2::aes(x, y, z = z)) +
#'   geom_surface_3d(fill = "#2E6F9E", shading = "smooth", edges = edges_3d()) +
#'   coord_3d()
#' ggplot2::ggplot(d, ggplot2::aes(x, y, z = z)) +
#'   geom_surface_3d(fill = "grey90", shading = "smooth") +
#'   coord_3d(edges = edges_3d(every = 5, colour = "grey20", alpha = 1),
#'            outline = outline_3d(silhouette = FALSE, crease = NULL))
#' @export
edges_3d <- function(type = c("cells", "triangles"), every = 1L, colour = "white", linewidth = 0.2,
                     linetype = 1, alpha = 0.6, hidden = c("hide", "dashed", "show")) {
  if (!is.numeric(every) || !length(every) %in% 1:2 || any(!is.finite(every) | every < 1 | every != trunc(every)))
    stop("every must be one or two positive integers.", call. = FALSE)
  linewidth <- mesh_number_3d(linewidth, "linewidth", function(x) is.finite(x) && x >= 0, "nonnegative")
  alpha <- mesh_number_3d(alpha, "alpha", function(x) is.finite(x) && x >= 0 && x <= 1, "between 0 and 1")
  if (length(colour) != 1L) stop("colour must be a single colour.", call. = FALSE)
  grDevices::col2rgb(colour)
  structure(list(type = match.arg(type), every = as.integer(rep_len(every, 2L)), colour = colour,
    linewidth = linewidth, linetype = linetype, alpha = alpha, hidden = match.arg(hidden)), class = "ggplot3d_edges")
}

# Vertex ids of the rows of a layer of triangles: rows share a vertex when
# they share .mesh, group and .vertex, or group and .source without .vertex.
stroke_vertex_ids_3d <- function(data) {
  key <- if (!is.null(data$.vertex)) paste(data$.mesh %or3d% 1L, data$group %or3d% 1L, data$.vertex)
    else paste(data$group %or3d% 1L, data$.source)
  match(key, unique(key))
}

# Grid coordinates of rows, in grid steps from the first grid point, as the
# columns .lattice_i and .lattice_j that mark a surface on a grid: whole
# numbers at the grid points, fractions at points added inside cells.
lattice_columns_3d <- function(out, i, j) {
  out$.lattice_i <- as.double(i)
  out$.lattice_j <- as.double(j)
  out
}

# Edges (rows of two data rows) of the triangles `faces`, each once. With
# type "cells" on a surface on a grid (.lattice_i and .lattice_j columns),
# only the edges along a grid line, on the lines whose index is a multiple of
# `every` or the last; otherwise, and for edges with an end off the grid (NA
# coordinates, as on the walls bound to a fitted surface), every triangle
# edge.
surface_edges_3d <- function(data, faces, type = "cells", every = c(1L, 1L)) {
  if (!nrow(faces)) return(matrix(integer(), ncol = 2L))
  edges <- matrix(as.vector(t(faces[, c(1L, 2L, 2L, 3L, 3L, 1L), drop = FALSE])), ncol = 2L, byrow = TRUE)
  edges <- unique_edges_3d(edges, stroke_vertex_ids_3d(data))
  lattice <- all(c(".lattice_i", ".lattice_j") %in% names(data))
  if (type == "triangles") return(edges)
  if (!lattice) {
    if (any(every > 1L)) stop("edges_3d(every = ) needs a surface on a grid.", call. = FALSE)
    return(edges)
  }
  i <- data$.lattice_i; j <- data$.lattice_j
  a <- edges[, 1L]; b <- edges[, 2L]
  on_grid <- is.finite(i[a]) & is.finite(i[b]) & is.finite(j[a]) & is.finite(j[b])
  if (!any(on_grid)) {
    if (any(every > 1L)) stop("edges_3d(every = ) needs a surface on a grid.", call. = FALSE)
    return(edges)
  }
  line <- function(k, step) {
    whole <- is.finite(k) & abs(k - round(k)) < 1e-9
    last <- max(round(k[whole]), 0)
    whole & (round(k) %% step == 0L | round(k) == last)
  }
  keep <- !on_grid | (on_grid & ((i[a] == i[b] & line(i[a], every[1L])) | (j[a] == j[b] & line(j[a], every[2L]))))
  edges[keep %in% TRUE, , drop = FALSE]
}

# Adds the mesh lines of params$edges to a packet of triangles: faces holds
# rows of data, three per triangle; `cells` the polygon edges of a mesh whose
# polygons were split into those triangles, or NULL.
edges_merge_3d <- function(packet, data, faces, params, panel_params, cells = NULL) {
  spec <- params$edges
  if (is.null(spec) || !nrow(faces)) return(packet)
  if (!inherits(spec, "ggplot3d_edges")) stop("edges must be created by edges_3d().", call. = FALSE)
  edges <- if (spec$type == "cells" && !is.null(cells) && !all(c(".lattice_i", ".lattice_j") %in% names(data))) {
    if (any(spec$every > 1L)) stop("edges_3d(every = ) needs a surface on a grid.", call. = FALSE)
    cells
  } else surface_edges_3d(data, faces, spec$type, spec$every)
  n <- nrow(edges)
  if (!n) return(packet)
  rows <- as.vector(t(edges))
  strokes <- data.frame(x = data$x[rows], y = data$y[rows], z = data$z[rows], group = rep(seq_len(n), each = 2L),
    colour = spec$colour, linewidth = spec$linewidth, linetype = spec$linetype, alpha = spec$alpha,
    .source = data$.source[rows], .layer = data$.layer[rows])
  style <- utils::modifyList(params, list(hidden = spec$hidden, lineend = "butt", linejoin = "round", shade = FALSE,
    shading = "flat", arrow = NULL, .vertex_key = NULL))
  lines <- packet_from_primitives_3d(strokes, seq_len(2L * n), rep("polylines", n), style, panel_params$panel_id, rep.int(2L, n))
  merge_scene_packets(list(packet, lines), panel_params$panel_id)
}

# A stroke specification of a surface layer: its own, coord_3d()'s when the
# layer sets none (NULL), or none when the layer sets FALSE.
layer_stroke_default_3d <- function(value, default) {
  if (isFALSE(value)) return(NULL)
  value %or3d% default
}
