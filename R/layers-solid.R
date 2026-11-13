utils::globalVariables("count")

prepare_solid_3d <- function(out, data, plot, params) {
  out <- prepare_spatial_aesthetics_3d(out, data, plot, params)
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
    spacing <- function(v) {d <- diff(sort(unique(v[is.finite(v)]))); if (length(d)) min(d[d > 0]) else 1}
    for (axis in c("x", "y")) {
      size <- params[[if (axis == "x") "width" else "depth"]]
      if (!is.null(size) && (length(size) != 1L || !is.finite(size) || size <= 0)) stop("width and depth must be positive scalars.", call. = FALSE)
      widths <- vapply(split(data[[axis]], data$PANEL), spacing, numeric(1)) * 0.9
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
emit_prism_buffers_3d <- function(data, triangles, edges, params, panel_params) {
  n <- nrow(data)
  endpoint <- data$zmax %or3d% data$z
  upper <- data
  upper$z <- pmax(data$zmin, data$zmax %or3d% data$z)
  data$z <- pmin(data$zmin, data$zmax %or3d% data$z)
  all <- rbind(data, upper)
  lower <- triangles[, c(1L, 3L, 2L), drop = FALSE]
  top <- triangles + n
  a <- edges[, 1L]; b <- edges[, 2L]
  walls <- rbind(cbind(a, b, b + n), cbind(a, b + n, a + n))
  cap <- params$cap %or3d% "both"
  caps <- matrix(integer(), ncol = 3L)
  upward <- endpoint >= data$zmin
  if (cap %in% c("both", "top")) caps <- rbind(caps, if (cap == "both") top else {
    rows <- upward[triangles[, 1L]]; result <- lower; result[rows, ] <- top[rows, ]; result
  })
  if (cap %in% c("both", "bottom")) caps <- rbind(caps, if (cap == "both") lower else {
    rows <- upward[triangles[, 1L]]; result <- top; result[rows, ] <- lower[rows, ]; result
  })
  faces <- rbind(caps, walls)
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
  offsets <- rep((seq_len(n) - 1L) * 4L, each = 2L)
  triangles <- matrix(rep(c(1L, 2L, 3L, 1L, 3L, 4L), n), ncol = 3L, byrow = TRUE) + offsets
  edges <- cbind(seq_len(4L * n), rep(c(2L, 3L, 4L, 1L), n) + rep((seq_len(n) - 1L) * 4L, each = 4L))
  emit_prism_buffers_3d(corners, triangles, edges, params, panel_params)
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
  groups <- interaction(data$group, data$.feature %or3d% 1L, data$.part %or3d% 1L, drop = TRUE)
  chunks <- triangles <- edges <- vector("list", nlevels(groups))
  offset <- 0L; i <- 0L
  for (rows in split(seq_len(nrow(data)), groups)) {
    if (!all(valid[rows])) next
    d <- data[rows, , drop = FALSE]
    if (length(unique(d$z)) != 1L || length(unique(d$zmin)) != 1L) stop("Extrusion heights must be constant within each feature.", call. = FALSE)
    if (d$z[1L] == d$zmin[1L]) next
    for (field in intersect(c("fill", "colour", "alpha", "linewidth", "linetype"), names(d))) {
      if (length(unique(d[[field]])) != 1L) stop("Extrusion materials must be constant within each feature.", call. = FALSE)
    }
    topology <- prism_topology_3d(d, split(seq_len(nrow(d)), d$subgroup %or3d% rep(1L, nrow(d))))
    i <- i + 1L; chunks[[i]] <- d
    triangles[[i]] <- topology$triangles + offset; edges[[i]] <- topology$edges + offset
    offset <- offset + nrow(d)
  }
  if (!i) return(new_scene_packet(panel_id = panel_params$panel_id))
  emit_prism_buffers_3d(do.call(rbind, chunks[seq_len(i)]), do.call(rbind, triangles[seq_len(i)]),
    do.call(rbind, edges[seq_len(i)]), params, panel_params)
}

register_geom_3d(GeomCol3D, emit_columns_3d, prepare_aesthetics = prepare_solid_3d,
                 position_check = function(position, scales) check_position_3d(position, scales, stack = TRUE))
register_geom_3d(GeomExtrude3D, emit_extrusions_3d,
  prepare_data = function(data) spatial_corners_3d(data, extrusion = TRUE),
  prepare_aesthetics = prepare_solid_3d,
  position_check = function(position, scales) check_position_3d(position, scales))
