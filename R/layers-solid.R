utils::globalVariables("count")

prepare_solid_3d <- function(out, data, plot, params) {
  solid_heights_3d(prepare_spatial_aesthetics_3d(out, data, plot, params), params)
}

solid_heights_3d <- function(out, params) {
  height <- out$height %or3d% params$height
  baseline <- out$zmin %or3d% params$zmin %or3d% params$base %or3d% 0
  if (is.null(out[["z"]]) && !is.null(height)) out$z <- baseline + height
  endpoint <- out$zmax %or3d% params$zmax
  if (is.null(out[["z"]]) && !is.null(endpoint)) out$z <- endpoint
  if (is.null(out$zmin)) out$zmin <- rep_len(params$base %or3d% 0, nrow(out))
  out
}

GeomCol3D <- ggplot2::ggproto("GeomCol3D", ggplot2::Geom,
  required_aes = c("x", "y", "z"), default_aes = solid_defaults_3d(),
  optional_aes = c("xmin", "xmax", "ymin", "ymax", "zmin", "zmax"),
  draw_key = ggplot2::draw_key_polygon,
  setup_data = function(data, params) {
    if (anyDuplicated(data[c("PANEL", "group", "x", "y")])) stop("Column cells must be unique within each group.", call. = FALSE)
    for (axis in c("x", "y")) {
      size <- params[[if (axis == "x") "width" else "depth"]]
      if (!is.null(size) && (length(size) != 1L || !is.finite(size) || size <= 0)) stop("width and depth must be positive scalars.", call. = FALSE)
      widths <- vapply(split(data[[axis]], data$PANEL), lattice_spacing_3d, numeric(1)) * 0.9
      size <- size %or3d% unname(widths[as.character(data$PANEL)])
      if (is.null(data[[paste0(axis, "min")]])) data[[paste0(axis, "min")]] <- data[[axis]] - size / 2
      if (is.null(data[[paste0(axis, "max")]])) data[[paste0(axis, "max")]] <- data[[axis]] + size / 2
      if (any(data[[paste0(axis, "min")]] > data[[paste0(axis, "max")]], na.rm = TRUE)) stop("Cell lower bounds must not exceed upper bounds.", call. = FALSE)
    }
    data
  }
)

StatCount3D <- ggplot2::ggproto("StatCount3D", ggplot2::Stat,
  required_aes = c("x", "y"), optional_aes = "weight",
  default_aes = ggplot2::aes(z = ggplot2::after_stat(count)),
  setup_params = function(data, params) {
    if (!is.null(data[["z"]])) stop("stat_count_3d() requires x/y and no input z mapping.", call. = FALSE)
    params
  },
  compute_group = function(data, scales) {
    cells <- interaction(data$x, data$y, drop = TRUE)
    rows <- !duplicated(cells)
    out <- data[rows, , drop = FALSE]
    sums <- rowsum(data$weight %or3d% rep(1, nrow(data)), as.integer(cells), reorder = FALSE)
    out$count <- as.double(sums[match(as.character(as.integer(cells[rows])), rownames(sums)), 1L])
    out$prop <- out$count / sum(abs(out$count))
    out
  }
)

#' Columns and counts on an x/y lattice
#' @param mapping,data,stat,position,...,na.rm,show.legend,inherit.aes Standard layer arguments.
#' @param width,depth Cell dimensions in transformed x/y units; defaults to 0.9 times spacing.
#' @param base Baseline in original z units, overridden by zmin.
#' @param shade Whether to apply lighting.
#' @return A ggplot2 layer. Columns accept z or zmax and optional zmin.
#' @examples
#' d <- expand.grid(x = 1:3, y = 1:3)
#' d$z <- seq_len(nrow(d)) - 4
#' ggplot2::ggplot(d, ggplot2::aes(x, y, z = z, fill = z)) +
#'   geom_col_3d() + coord_3d()
#' ggplot2::ggplot(mtcars, ggplot2::aes(factor(cyl), factor(gear))) +
#'   geom_bar_3d() + coord_3d()
#' ggplot2::ggplot(mtcars, ggplot2::aes(factor(cyl), factor(gear))) +
#'   stat_count_3d() + coord_3d()
#' @export
geom_col_3d <- function(mapping = NULL, data = NULL, stat = "identity", position = "identity", ...,
                        width = NULL, depth = NULL, base = 0, shade = TRUE,
                        na.rm = FALSE, show.legend = NA, inherit.aes = TRUE) {
  make_layer_3d(GeomCol3D, mapping, data, stat, position,
    c(list(width = width, depth = depth, base = base, shade = shade, na.rm = na.rm), list(...)), show.legend, inherit.aes)
}

#' @rdname geom_col_3d
#' @export
geom_bar_3d <- function(mapping = NULL, data = NULL, stat = StatCount3D, position = "identity", ...,
                        width = NULL, depth = NULL, base = 0, shade = TRUE,
                        na.rm = FALSE, show.legend = NA, inherit.aes = TRUE) {
  geom_col_3d(mapping, data, stat, position, ..., width = width, depth = depth, base = base,
              shade = shade, na.rm = na.rm, show.legend = show.legend, inherit.aes = inherit.aes)
}

#' @rdname geom_col_3d
#' @param geom Registered geometry prototype.
#' @export
stat_count_3d <- function(mapping = NULL, data = NULL, geom = GeomCol3D, position = "identity", ...,
                          na.rm = FALSE, show.legend = NA, inherit.aes = TRUE) {
  make_layer_3d(geom, mapping, data, StatCount3D, position,
                c(list(base = 0, na.rm = na.rm), list(...)), show.legend, inherit.aes)
}

prism_topology_3d <- function(data, rings) {
  flat <- data; flat$z <- 0
  topology <- triangulate_polygon_3d(flat, rings)
  xy <- as.matrix(flat[c("x", "y")])
  triangles <- matrix(unlist(topology$triangles), ncol = 3L, byrow = TRUE)
  a <- triangles[, 1L]; b <- triangles[, 2L]; c <- triangles[, 3L]
  clockwise <- (xy[b, 1L] - xy[a, 1L]) * (xy[c, 2L] - xy[a, 2L]) -
    (xy[b, 2L] - xy[a, 2L]) * (xy[c, 1L] - xy[a, 1L]) < 0
  triangles[clockwise, ] <- triangles[clockwise, c(1L, 3L, 2L), drop = FALSE]
  rings <- topology$rings
  depth <- vapply(seq_along(rings), function(i) sum(vapply(setdiff(seq_along(rings), i),
    function(j) inside_ring_3d(xy[rings[[i]][1L], ], xy, rings[[j]]), logical(1))), integer(1))
  rings <- lapply(seq_along(rings), function(i) {
    r <- rings[[i]]
    if ((ring_area_3d(xy, r) > 0) != (depth[i] %% 2L == 0L)) rev(r) else r
  })
  edges <- do.call(rbind, lapply(rings, function(r) cbind(r, c(r[-1L], r[1L]))))
  list(triangles = triangles, edges = edges)
}

# Directed boundaries put the solid on their left, including hole boundaries.
emit_prism_buffers_3d <- function(data, base_faces, edges, params, panel_params) {
  n <- nrow(data)
  endpoint <- data$zmax %or3d% data$z
  all <- structure(lapply(data, rep, times = 2L), class = "data.frame", row.names = .set_row_names(2L * n))
  all$z <- c(pmin(data$zmin, endpoint), pmax(data$zmin, endpoint))
  a <- edges[, 1L]; b <- edges[, 2L]
  walls <- cbind(a, b, b + n, a + n)
  cap <- params$cap %or3d% "both"
  upward <- endpoint >= data$zmin
  if (is.matrix(base_faces)) base_faces <- list(base_faces)
  caps <- if (cap == "none") list() else lapply(base_faces, function(ids) {
    lower <- ids[, rev(seq_len(ncol(ids))), drop = FALSE]
    top <- ids + n
    if (cap == "both") return(rbind(top, lower))
    rows <- upward[ids[, 1L]]
    if (cap == "bottom") rows <- !rows
    lower[rows, ] <- top[rows, , drop = FALSE]
    lower
  })
  faces <- c(caps, list(walls))
  strokes <- rbind(edges, edges + n, cbind(unique(as.vector(edges)), unique(as.vector(edges)) + n))
  packet_from_faces_edges_3d(all, faces, strokes, params, panel_params)
}

emit_columns_3d <- function(data, params, panel_params) {
  data <- data[valid_rows_3d(data, params, c("xmin", "xmax", "ymin", "ymax", "z", "zmin")) &
    (data$zmax %or3d% data$z) != data$zmin, , drop = FALSE]
  n <- nrow(data)
  if (!n) return(new_scene_packet(panel_id = panel_params$panel_id))
  if (is.null(data$zmax)) data$zmax <- data$z
  corners <- data[rep(seq_len(n), each = 4L), , drop = FALSE]
  corners$x <- as.vector(t(cbind(data$xmin, data$xmax, data$xmax, data$xmin)))
  corners$y <- as.vector(t(cbind(data$ymin, data$ymin, data$ymax, data$ymax)))
  faces <- matrix(seq_len(4L * n), ncol = 4L, byrow = TRUE)
  edges <- cbind(seq_len(4L * n), rep(c(2L, 3L, 4L, 1L), n) + rep((seq_len(n) - 1L) * 4L, each = 4L))
  emit_prism_buffers_3d(corners, faces, edges, params, panel_params)
}

GeomExtrude3D <- ggplot2::ggproto("GeomExtrude3D", ggplot2::Geom,
  required_aes = c("x", "y", "z", "zmin"), default_aes = solid_defaults_3d(),
  optional_aes = c("height", "subgroup", ".part", ".feature", ".origin"),
  draw_key = ggplot2::draw_key_polygon
)

#' Extrude polygon rings along z
#' @param mapping,data,stat,position,...,na.rm,show.legend,inherit.aes Standard layer arguments.
#' @param base Baseline in original z units, overridden by zmin.
#' @param cap Caps to draw: both, top, bottom or none.
#' @param shade Whether to apply lighting.
#' @details Input is ordered x/y rings with group/subgroup, or projected sf
#'   POLYGON/MULTIPOLYGON features. Map z as the endpoint or height as an offset
#'   from zmin. Holes retain their interior walls and open cap interiors.
#' @return A ggplot2 layer.
#' @examples
#' d <- data.frame(x = c(0, 2, 2, 0), y = c(0, 0, 1, 1), z = 2)
#' ggplot2::ggplot(d, ggplot2::aes(x, y, z = z)) +
#'   geom_extrude_3d(colour = "grey30") + coord_3d()
#' @export
geom_extrude_3d <- function(mapping = NULL, data = NULL, stat = "identity", position = "identity", ...,
                            base = 0, cap = "both", shade = TRUE,
                            na.rm = FALSE, show.legend = NA, inherit.aes = TRUE) {
  cap <- match.arg(cap, c("both", "top", "bottom", "none"))
  make_layer_3d(GeomExtrude3D, mapping, data, stat, position,
    c(list(base = base, cap = cap, shade = shade, na.rm = na.rm), list(...)), show.legend, inherit.aes)
}

emit_extrusions_3d <- function(data, params, panel_params) {
  valid <- valid_rows_3d(data, params, c("x", "y", "z", "zmin"))
  keys <- data[c("group", intersect(c(".feature", ".part"), names(data)))]
  groups <- match(do.call(paste, c(keys, sep = ":")), unique(do.call(paste, c(keys, sep = ":"))))
  rows <- split(seq_len(nrow(data)), groups)
  first <- vapply(rows, `[`, integer(1), 1L)
  group_index <- match(groups, as.integer(names(rows)))
  valid_group <- vapply(rows, function(r) all(valid[r]), logical(1))
  for (field in c("z", "zmin", intersect(c("fill", "colour", "alpha", "linewidth", "linetype"), names(data)))) {
    value <- match(data[[field]], unique(data[[field]]))
    if (any(value != value[first[group_index]] & valid_group[group_index])) stop(if (field %in% c("z", "zmin"))
      "Extrusion heights must be constant within each feature." else
      "Extrusion materials must be constant within each feature.", call. = FALSE)
  }
  keep <- valid_group & data$z[first] != data$zmin[first]
  keep[is.na(keep)] <- FALSE
  rows <- rows[keep]
  if (!length(rows)) return(new_scene_packet(panel_id = panel_params$panel_id))
  topology <- convex_prisms_3d(data, rows)
  if (!is.null(topology)) return(emit_prism_buffers_3d(data, topology$faces, topology$edges, params, panel_params))
  groups <- factor(groups)
  chunks <- triangles <- edges <- vector("list", nlevels(groups))
  offset <- 0L; i <- 0L
  for (rows in split(seq_len(nrow(data)), groups)) {
    if (!all(valid[rows])) next
    d <- data[rows, , drop = FALSE]
    if (d$z[1L] == d$zmin[1L]) next
    topology <- prism_topology_3d(d, split(seq_len(nrow(d)), d$subgroup %or3d% rep(1L, nrow(d))))
    i <- i + 1L; chunks[[i]] <- d
    triangles[[i]] <- topology$triangles + offset; edges[[i]] <- topology$edges + offset
    offset <- offset + nrow(d)
  }
  if (!i) return(new_scene_packet(panel_id = panel_params$panel_id))
  emit_prism_buffers_3d(do.call(rbind, chunks[seq_len(i)]), do.call(rbind, triangles[seq_len(i)]),
    do.call(rbind, edges[seq_len(i)]), params, panel_params)
}

# Convex single-ring features share vectorised half-plane validation and cap topology.
convex_prisms_3d <- function(data, rows) {
  faces <- edges <- list()
  for (size in unique(lengths(rows))) {
    if (size < 3L) return(NULL)
    ids <- matrix(unlist(rows[lengths(rows) == size], use.names = FALSE), ncol = size, byrow = TRUE)
    x <- matrix(data$x[ids], nrow(ids)); y <- matrix(data$y[ids], nrow(ids))
    if (!is.null(data$subgroup) && any(matrix(data$subgroup[ids], nrow(ids)) != data$subgroup[ids[, 1L]])) return(NULL)
    closed <- x[, 1L] == x[, size] & y[, 1L] == y[, size]
    if (any(closed) && !all(closed)) return(NULL)
    if (all(closed)) { ids <- ids[, -size, drop = FALSE]; x <- x[, -size, drop = FALSE]; y <- y[, -size, drop = FALSE]; size <- size - 1L }
    if (size < 3L) return(NULL)
    for (j in seq_len(size - 1L)) for (k in seq.int(j + 1L, size))
      if (any(x[,j] == x[,k] & y[,j] == y[,k])) stop("Polygon rings require at least three distinct vertices.", call. = FALSE)
    next_id <- c(seq.int(2L, size), 1L)
    area <- rowSums(x * y[, next_id, drop = FALSE] - y * x[, next_id, drop = FALSE])
    sign <- sign(area)
    tolerance <- 256 * .Machine$double.eps
    if (any(area == 0)) return(NULL)
    for (j in seq_len(size)) {
      cross <- (x[, next_id[j]] - x[, j]) * (y - y[, j]) - (y[, next_id[j]] - y[, j]) * (x - x[, j])
      if (any(cross * sign < -tolerance)) return(NULL)
    }
    reverse <- area < 0
    ids[reverse, ] <- ids[reverse, rev(seq_len(size)), drop = FALSE]
    edges[[length(edges) + 1L]] <- cbind(as.vector(t(ids)), as.vector(t(ids[, next_id, drop = FALSE])))
    faces[[length(faces) + 1L]] <- ids
  }
  list(faces = faces, edges = do.call(rbind, edges))
}

register_geom_3d(GeomCol3D, emit_columns_3d, prepare_aesthetics = prepare_solid_3d,
  positions = c("nudge", "jitter", "dodge", "stack"),
  extents = list(x = extent_3d("xmin", "xmax", "x"), y = extent_3d("ymin", "ymax", "y"),
                 z = extent_3d("zmin", "zmax", "z")))
register_geom_3d(GeomExtrude3D, emit_extrusions_3d,
  prepare_data = function(data) spatial_corners_3d(data, extrusion = TRUE),
  prepare_aesthetics = prepare_solid_3d)
