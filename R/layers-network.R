# Network layers: graph layouts from igraph as node and edge tables, edges as
# lines, arcs or tubes, multilayer networks on stacked planes, and dendrograms
# drawn in any coordinate plane.

utils::globalVariables(c("x", "y", "z", "xend", "yend", "zend"))

#' Fortify a graph for three-dimensional plotting
#'
#' Turns an igraph graph into a node table with x, y and z positions. The edge
#' table, with start and end positions taken from the node table, is attached
#' to it and read by [geom_edge_3d()] through [get_edges_3d()]; `ggplot(graph)`
#' therefore supports node and edge layers from one plot data set. Calling
#' `ggplot(graph, layout = )` passes the layout through to this method.
#'
#' The layout is computed by igraph, not by this package. With `layout = NULL`
#' the vertex attributes `x`, `y` and `z` are used when the graph carries them
#' (as for [igraph::plot.igraph()]), otherwise the three-dimensional
#' Fruchterman-Reingold layout [igraph::layout_with_fr()] with `dim = 3`. A
#' layout function is called with the graph, and with `dim` when it accepts
#' it. A two-column layout places every node at z = 0.
#' @param model An igraph graph (including tidygraph `tbl_graph` objects).
#' @param data Unused, for compatibility with the generic.
#' @param layout `NULL`, a numeric matrix with one row per vertex and two or
#'   three columns, or a layout function such as [igraph::layout_with_kk()].
#' @param dim Number of layout dimensions requested from a layout function.
#' @param ... Unused.
#' @return A data frame with one row per vertex: `x`, `y`, `z`, `node` (the
#'   vertex index), `name` and the vertex attributes. Its attribute `"edges"`
#'   holds one row per edge: `from`, `to` (vertex indices), `x`, `y`, `z`,
#'   `xend`, `yend`, `zend` and the edge attributes.
#' @references Fruchterman and Reingold (1991). Graph drawing by force-directed
#'   placement. Software: Practice and Experience 21, 1129-1164.
#'   \doi{10.1002/spe.4380211102}
#' @seealso [geom_edge_3d()], [stat_layers_3d()]
#' @examples
#' if (requireNamespace("igraph", quietly = TRUE)) {
#'   set.seed(1)
#'   g <- igraph::sample_gnp(30, 0.1)
#'   nodes <- fortify(g, layout = igraph::layout_with_fr(g, dim = 3))
#'   head(nodes)
#'   head(get_edges_3d()(nodes))
#' }
#' @export
fortify.igraph <- function(model, data = NULL, layout = NULL, dim = 3L, ...) {
  require_geometry_package_3d("igraph")
  n <- igraph::vcount(model)
  attrs <- igraph::vertex_attr(model)
  if (is.null(layout) && all(c("x", "y") %in% names(attrs))) {
    layout <- cbind(attrs$x, attrs$y, attrs$z %or3d% rep(0, n))
  }
  if (is.null(layout)) layout <- igraph::layout_with_fr
  if (is.function(layout)) layout <- if ("dim" %in% names(formals(layout))) layout(model, dim = dim) else layout(model)
  layout <- as.matrix(layout)
  if (!is.numeric(layout) || nrow(layout) != n || !ncol(layout) %in% 2:3) {
    stop("layout must give two or three coordinates for each of the ", n, " vertices.", call. = FALSE)
  }
  if (ncol(layout) == 2L) layout <- cbind(layout, 0)
  nodes <- data.frame(x = layout[, 1L], y = layout[, 2L], z = layout[, 3L], node = seq_len(n),
                      name = as.character(attrs$name %or3d% seq_len(n)))
  for (field in setdiff(names(attrs), c(names(nodes), "x", "y", "z"))) nodes[[field]] <- attrs[[field]]
  ends <- igraph::as_edgelist(model, names = FALSE)
  edges <- data.frame(from = as.integer(ends[, 1L]), to = as.integer(ends[, 2L]))
  edges[c("x", "y", "z")] <- nodes[edges$from, c("x", "y", "z")]
  edges[c("xend", "yend", "zend")] <- nodes[edges$to, c("x", "y", "z")]
  edge_attrs <- igraph::edge_attr(model)
  for (field in setdiff(names(edge_attrs), names(edges))) edges[[field]] <- edge_attrs[[field]]
  rownames(edges) <- NULL
  attr(nodes, "edges") <- edges
  nodes
}

#' Edge table of a fortified graph
#'
#' Returns a function that ggplot2 calls on the plot data, as the `data`
#' argument of a layer. For a node table made by [fortify.igraph()] it returns
#' the attached edge table; any other data frame is returned unchanged.
#' @return A function of the plot data returning a data frame.
#' @examples
#' if (requireNamespace("igraph", quietly = TRUE)) {
#'   nodes <- fortify(igraph::make_ring(5))
#'   get_edges_3d()(nodes)
#' }
#' @export
get_edges_3d <- function() {
  function(data) {
    edges <- attr(data, "edges")
    if (is.null(edges)) data else edges
  }
}

network_mapping_3d <- function(mapping, defaults) {
  for (name in names(mapping)) defaults[[name]] <- mapping[[name]]
  defaults
}

# Positions are normalised to the unit cube; the model box has side lengths
# proportional to the camera aspect, with the longest side of length one.
model_scale_3d <- function(panel_params) {
  aspect <- panel_params$camera$aspect %or3d% c(1, 1, 1)
  aspect / max(aspect)
}

cross_3d <- function(a, b) {
  cbind(a[, 2L] * b[, 3L] - a[, 3L] * b[, 2L],
        a[, 3L] * b[, 1L] - a[, 1L] * b[, 3L],
        a[, 1L] * b[, 2L] - a[, 2L] * b[, 1L])
}

unit_rows_3d <- function(m) {
  length <- sqrt(rowSums(m^2))
  m / ifelse(length > 0, length, 1)
}

# Points along each edge, as an array of edges x points x 3. A nonzero
# curvature bends the edge into a circular arc in the plane spanned by the
# chord and the horizontal direction perpendicular to it (the x direction for
# vertical chords). The arc's sagitta is curvature times half the chord, so
# curvature 1 gives a half circle; positive values bend to the right of the
# direction of travel seen from above, as in grid::curveGrob().
edge_points_3d <- function(start, end, curvature, n) {
  m <- nrow(start)
  chord <- end - start
  length <- sqrt(rowSums(chord^2))
  steps <- if (curvature == 0) 2L else as.integer(n)
  t <- seq(0, 1, length.out = steps)
  out <- array(0, c(m, steps, 3L))
  for (k in seq_len(steps)) out[, k, ] <- start + chord * t[k]
  if (curvature == 0) return(out)
  side <- cross_3d(chord, matrix(c(0, 0, 1), m, 3L, byrow = TRUE))
  vertical <- sqrt(rowSums(side^2)) <= 1e-9 * pmax(length, 1e-300)
  side[vertical, ] <- cross_3d(chord[vertical, , drop = FALSE], matrix(rep(c(1, 0, 0), each = sum(vertical)), ncol = 3L))
  bend <- unit_rows_3d(side) * sign(curvature)
  along <- unit_rows_3d(chord)
  half <- length / 2
  sagitta <- abs(curvature) * half
  radius <- (half^2 + sagitta^2) / (2 * sagitta)
  centre <- (start + end) / 2 + bend * (sagitta - radius)
  span <- atan2(half, radius - sagitta)
  curved <- length > 0
  for (k in seq_len(steps)) {
    phi <- span * (2 * t[k] - 1)
    point <- centre + radius * (cos(phi) * bend + sin(phi) * along)
    out[curved, k, ] <- point[curved, ]
  }
  out
}

# Triangulated tubes around polylines (edges x points x 3 array), with
# parallel-transport frames so the tube does not twist, and closing caps.
# Returns ring vertex coordinates, the edge of each vertex and face indices.
tube_mesh_3d <- function(path, radius, sides) {
  m <- dim(path)[1L]; steps <- dim(path)[2L]
  tangent <- array(0, dim(path))
  for (k in seq_len(steps)) {
    a <- max(1L, k - 1L); b <- min(steps, k + 1L)
    tangent[, k, ] <- unit_rows_3d(matrix(path[, b, ] - path[, a, ], m, 3L))
  }
  first <- matrix(tangent[, 1L, ], m, 3L)
  seed <- matrix(c(0, 0, 1), m, 3L, byrow = TRUE)
  seed[abs(first[, 3L]) > 0.9, ] <- matrix(c(1, 0, 0), 1L, 3L)[rep(1L, sum(abs(first[, 3L]) > 0.9)), ]
  normal <- unit_rows_3d(cross_3d(first, seed))
  angles <- 2 * pi * (seq_len(sides) - 1L) / sides
  vertices <- array(0, c(m, steps, sides, 3L))
  for (k in seq_len(steps)) {
    t <- matrix(tangent[, k, ], m, 3L)
    if (k > 1L) normal <- unit_rows_3d(normal - rowSums(normal * t) * t)
    binormal <- cross_3d(t, normal)
    for (j in seq_len(sides)) {
      vertices[, k, j, ] <- matrix(path[, k, ], m, 3L) + radius * (cos(angles[j]) * normal + sin(angles[j]) * binormal)
    }
  }
  per_edge <- steps * sides
  id <- function(edge, k, j) (edge - 1L) * per_edge + (k - 1L) * sides + j
  grid <- expand.grid(j = seq_len(sides), k = seq_len(steps - 1L), edge = seq_len(m))
  j2 <- grid$j %% sides + 1L
  a <- id(grid$edge, grid$k, grid$j); b <- id(grid$edge, grid$k + 1L, grid$j)
  c <- id(grid$edge, grid$k + 1L, j2); d <- id(grid$edge, grid$k, j2)
  triangles <- rbind(cbind(a, b, c), cbind(a, c, d))
  start_cap <- outer(seq_len(m), rev(seq_len(sides)), function(e, j) id(e, 1L, j))
  end_cap <- outer(seq_len(m), seq_len(sides), function(e, j) id(e, steps, j))
  coords <- matrix(aperm(vertices, c(3L, 2L, 1L, 4L)), ncol = 3L)
  list(coords = coords, edge = rep(seq_len(m), each = per_edge), triangles = triangles, caps = rbind(start_cap, end_cap))
}

emit_edges_3d <- function(data, params, panel_params) {
  data <- data[valid_rows_3d(data, params, c("x", "y", "z", "xend", "yend", "zend")), , drop = FALSE]
  if (!nrow(data)) return(new_scene_packet(panel_id = panel_params$panel_id))
  shape <- params$shape %or3d% "line"
  curvature <- params$curvature %or3d% if (shape == "arc") 0.5 else 0
  if (shape == "line") curvature <- 0
  scale <- model_scale_3d(panel_params)
  start <- sweep(as.matrix(data[c("x", "y", "z")]), 2L, scale, "*")
  end <- sweep(as.matrix(data[c("xend", "yend", "zend")]), 2L, scale, "*")
  if (shape == "tube") {
    # A tube needs a direction; self-loops and coincident ends have none.
    keep <- rowSums((end - start)^2) > 0
    data <- data[keep, , drop = FALSE]; start <- start[keep, , drop = FALSE]; end <- end[keep, , drop = FALSE]
    if (!nrow(data)) return(new_scene_packet(panel_id = panel_params$panel_id))
  }
  path <- edge_points_3d(start, end, curvature, params$n %or3d% 32L)
  unscale <- function(coords) sweep(coords, 2L, scale, "/")
  if (shape != "tube") {
    steps <- dim(path)[2L]
    points <- data[rep(seq_len(nrow(data)), each = steps), , drop = FALSE]
    points[c("x", "y", "z")] <- as.data.frame(unscale(matrix(aperm(path, c(2L, 1L, 3L)), ncol = 3L)))
    return(packet_from_primitives_3d(points, seq_len(nrow(points)), rep("polylines", nrow(data)),
      params, panel_params$panel_id, rep.int(steps, nrow(data))))
  }
  width <- (data$linewidth %or3d% rep(0.5, nrow(data))) / 0.5
  mesh <- tube_mesh_3d(path, (params$radius %or3d% 0.01) * width, params$sides %or3d% 12L)
  vertices <- data[mesh$edge, , drop = FALSE]
  vertices[c("x", "y", "z")] <- as.data.frame(unscale(mesh$coords))
  vertices$fill <- vertices$colour
  faces <- list(mesh$triangles, mesh$caps)
  counts <- c(rep.int(3L, nrow(mesh$triangles)), rep.int(ncol(mesh$caps), nrow(mesh$caps)))
  packet_from_primitives_3d(vertices, unlist(lapply(faces, function(f) as.vector(t(f))), use.names = FALSE),
    rep("faces", length(counts)), params, panel_params$panel_id, counts)
}

GeomEdge3D <- ggplot2::ggproto("GeomEdge3D", ggplot2::Geom,
  required_aes = c("x", "y", "z", "xend", "yend", "zend"),
  default_aes = ggplot2::aes(colour = "grey35", linewidth = 0.5, linetype = 1, alpha = 1),
  draw_key = ggplot2::draw_key_path
)

register_geom_3d(GeomEdge3D, emit_edges_3d,
  parameters = list(shape = "line", curvature = NULL, n = 32L, radius = 0.01, sides = 12L))

check_edge_params_3d <- function(shape, curvature, n, radius, sides, arrow) {
  shape <- match.arg(shape, c("line", "arc", "tube"))
  if (!is.null(curvature) && (!is.numeric(curvature) || length(curvature) != 1L || !is.finite(curvature))) stop("curvature must be a finite number.", call. = FALSE)
  if (!is.numeric(n) || length(n) != 1L || !is.finite(n) || n < 3) stop("n must be a number of at least 3.", call. = FALSE)
  if (!is.numeric(radius) || length(radius) != 1L || !is.finite(radius) || radius <= 0) stop("radius must be a positive number.", call. = FALSE)
  if (!is.numeric(sides) || length(sides) != 1L || !is.finite(sides) || sides < 3) stop("sides must be a number of at least 3.", call. = FALSE)
  if (shape == "tube" && !is.null(arrow)) stop("Arrows require shape = \"line\" or \"arc\".", call. = FALSE)
  list(shape = shape, curvature = curvature, n = as.integer(n), radius = radius, sides = as.integer(sides))
}

#' Network edges and nodes
#'
#' `geom_edge_3d()` draws one edge per row from (x, y, z) to (xend, yend,
#' zend), as a straight line, a circular arc or a shaded tube. `geom_node_3d()`
#' draws nodes as points. Both map the position columns of a node or edge
#' table by default, so after `ggplot(graph)` (see [fortify.igraph()]) they
#' need no mapping; `geom_edge_3d()` reads the edge table with
#' [get_edges_3d()].
#'
#' Arcs and tubes are built in the model box, after the panel aspect is
#' applied, so they keep their shape for any `coord_3d(aspect = )`. An arc
#' bends in the plane spanned by the edge and the horizontal direction
#' perpendicular to it; its sagitta is `curvature` times half the edge length,
#' so `curvature = 1` is a half circle and negative values bend the other way,
#' as in [ggplot2::geom_curve()]. Tubes follow the straight edge, or the arc
#' when `curvature` is set; their cross-section is a regular polygon with
#' `sides` corners, carried along the edge with rotation-minimising frames,
#' and they are closed at both ends. The tube colour is the edge `colour`.
#' @param mapping,data,stat,position,...,na.rm,show.legend,inherit.aes Standard
#'   layer arguments. The default mapping of `geom_edge_3d()` is
#'   `aes(x, y, z = z, xend = xend, yend = yend, zend = zend)` and that of
#'   `geom_node_3d()` is `aes(x, y, z = z)`; a supplied mapping overrides
#'   these aesthetic by aesthetic.
#' @param shape `"line"`, `"arc"` or `"tube"`.
#' @param curvature Arc curvature; defaults to 0.5 for arcs and 0 (straight)
#'   for tubes. Ignored for lines.
#' @param n Number of points along a curved edge.
#' @param radius Tube radius as a fraction of the longest side of the panel
#'   box, for linewidth 0.5; the radius scales with linewidth.
#' @param sides Number of corners of the tube cross-section.
#' @param shade Whether to light faces; defaults to `TRUE` for tubes.
#' @return A ggplot2 layer.
#' @references Bishop (1975). There is more than one way to frame a curve.
#'   American Mathematical Monthly 82, 246-251. \doi{10.2307/2319846}
#' @seealso [fortify.igraph()], [stat_layers_3d()]
#' @examples
#' if (requireNamespace("igraph", quietly = TRUE)) {
#'   set.seed(2)
#'   g <- igraph::sample_pa(40, directed = FALSE)
#'   ggplot2::ggplot(g, layout = igraph::layout_with_fr(g, dim = 3)) +
#'     geom_edge_3d() + geom_node_3d(colour = "steelblue", size = 2) + coord_3d()
#'   ggplot2::ggplot(g) +
#'     geom_edge_3d(shape = "tube", colour = "grey60") +
#'     geom_node_3d(ggplot2::aes(colour = igraph::degree(g)), size = 2) + coord_3d()
#' }
#' d <- data.frame(x = 0, y = 0, z = 0, xend = 1, yend = 1, zend = 1)
#' ggplot2::ggplot(d) + geom_edge_3d(shape = "arc", curvature = 0.8) + coord_3d()
#' @export
geom_edge_3d <- function(mapping = NULL, data = get_edges_3d(), stat = "identity", position = "identity", ...,
                         shape = "line", curvature = NULL, n = 32L, radius = 0.01, sides = 12L, shade = NULL,
                         na.rm = FALSE, show.legend = NA, inherit.aes = TRUE) {
  checked <- check_edge_params_3d(shape, curvature, n, radius, sides, list(...)$arrow)
  mapping <- network_mapping_3d(mapping, ggplot2::aes(x = x, y = y, z = z, xend = xend, yend = yend, zend = zend))
  make_layer_3d(GeomEdge3D, mapping, data, stat, position,
    c(checked, list(shade = shade %or3d% (checked$shape == "tube"), na.rm = na.rm), list(...)),
    show.legend, inherit.aes)
}

#' @rdname geom_edge_3d
#' @export
geom_node_3d <- function(mapping = NULL, data = NULL, stat = "identity", position = "identity", ...,
                         na.rm = FALSE, show.legend = NA, inherit.aes = TRUE) {
  mapping <- network_mapping_3d(mapping, ggplot2::aes(x = x, y = y, z = z))
  make_layer_3d(ggplot2::GeomPoint, mapping, data, stat, position, c(list(na.rm = na.rm), list(...)), show.legend, inherit.aes)
}

# Evaluates expr with the random number generator seeded, then restores the
# caller's generator state.
with_seed_3d <- function(seed, expr) {
  if (is.null(seed)) return(expr)
  had <- exists(".Random.seed", envir = globalenv(), inherits = FALSE)
  old <- if (had) get(".Random.seed", envir = globalenv(), inherits = FALSE)
  on.exit(if (had) assign(".Random.seed", old, envir = globalenv()) else rm(".Random.seed", envir = globalenv()))
  set.seed(seed)
  expr
}

layout_2d_3d <- function(from, to, nodes, layout) {
  graph <- igraph::graph_from_data_frame(data.frame(from = from, to = to), directed = FALSE,
                                         vertices = data.frame(name = nodes))
  xy <- as.matrix(if ("dim" %in% names(formals(layout))) layout(graph, dim = 2L) else layout(graph))
  if (!is.numeric(xy) || nrow(xy) != length(nodes) || ncol(xy) < 2L) stop("layout must return two coordinates per node.", call. = FALSE)
  xy[, 1:2, drop = FALSE]
}

StatLayers3D <- ggplot2::ggproto("StatLayers3D", ggplot2::Stat,
  required_aes = c("from", "to", "layer"),
  optional_aes = "layer_end",
  default_aes = ggplot2::aes(z = ggplot2::after_stat(z)),
  dropped_aes = c("from", "to", "layer", "layer_end", "weight"),
  compute_panel = function(data, scales, layout = NULL, align = TRUE, what = "edges", margin = 0.1, seed = 1L) {
    require_geometry_package_3d("igraph")
    layout <- layout %or3d% igraph::layout_with_fr
    from <- as.character(data$from); to <- as.character(data$to)
    layer_end <- if (is.null(data$layer_end)) data$layer else data$layer_end
    levels <- if (is.factor(data$layer)) levels(droplevels(factor(c(as.character(data$layer), as.character(layer_end)), levels = levels(data$layer))))
              else sort(unique(c(data$layer, layer_end)))
    height <- if (is.numeric(data$layer)) as.double(levels) else seq_along(levels)
    a <- match(as.character(data$layer), as.character(levels)); b <- match(as.character(layer_end), as.character(levels))
    if (anyNA(a) || anyNA(b)) stop("layer and layer_end must contain non-missing layer values.", call. = FALSE)
    key <- function(node, l) paste(l, node, sep = "\r")
    state_node <- unique(data.frame(node = c(from, to), layer = c(a, b), stringsAsFactors = FALSE))
    within <- a == b
    position <- matrix(NA_real_, nrow(state_node), 2L)
    with_seed_3d(seed, {
      if (isTRUE(align)) {
        names <- unique(state_node$node)
        xy <- layout_2d_3d(from, to, names, layout)
        position <- xy[match(state_node$node, names), , drop = FALSE]
      } else {
        for (l in unique(state_node$layer)) {
          here <- which(state_node$layer == l)
          names <- state_node$node[here]
          rows <- within & a == l
          position[here, ] <- layout_2d_3d(from[rows], to[rows], names, layout)
        }
      }
    })
    state_key <- key(state_node$node, state_node$layer)
    if (what == "edges") {
      i <- match(key(from, a), state_key); j <- match(key(to, b), state_key)
      data$x <- position[i, 1L]; data$y <- position[i, 2L]; data$z <- height[a]
      data$xend <- position[j, 1L]; data$yend <- position[j, 2L]; data$zend <- height[b]
      data$interlayer <- !within
      return(data)
    }
    panel <- data$PANEL[1L]
    if (what == "nodes") {
      degree <- tabulate(c(match(key(from, a), state_key), match(key(to, b), state_key)), nrow(state_node))
      return(data.frame(x = position[, 1L], y = position[, 2L], z = height[state_node$layer],
        node = state_node$node, layer = levels[state_node$layer], degree = degree,
        PANEL = panel, group = -1L, stringsAsFactors = FALSE))
    }
    range_x <- range(position[, 1L]); range_y <- range(position[, 2L])
    pad <- margin * max(diff(range_x), diff(range_y), 1e-9)
    corners_x <- c(range_x[1L] - pad, range_x[2L] + pad)[c(1L, 2L, 2L, 1L)]
    corners_y <- c(range_y[1L] - pad, range_y[2L] + pad)[c(1L, 1L, 2L, 2L)]
    data.frame(x = rep(corners_x, length(levels)), y = rep(corners_y, length(levels)),
      z = rep(height, each = 4L), layer = rep(levels, each = 4L),
      PANEL = panel, group = rep(seq_along(levels), each = 4L), stringsAsFactors = FALSE)
  }
)

#' Multilayer networks
#'
#' Lays out a multilayer network given as an edge list, with each layer's
#' two-dimensional layout on its own z plane. Each row is an edge from node
#' `from` in layer `layer` to node `to` in layer `layer_end` (by default the
#' same layer); rows whose two layers differ are inter-layer edges, such as the
#' couplings that join a node to its copy in the next layer. A numeric layer
#' sits at z equal to its value, a discrete layer at z equal to its level
#' index.
#'
#' The layout is computed by igraph. With `align = TRUE` one layout of the
#' aggregated network places every node at the same x and y in all layers,
#' so a node's copies stack vertically and the layers can be compared, as in
#' multiplex network visualisation (De Domenico et al. 2015). With
#' `align = FALSE` each layer is laid out from its own intra-layer edges.
#' Layout functions draw random starting positions; `seed` fixes them so the
#' plot is the same at every rendering, without changing the caller's random
#' number stream.
#'
#' `what` selects the output: `"edges"` (one row per input row, with x, y, z,
#' xend, yend, zend and `interlayer`), `"nodes"` (one row per node and layer,
#' with `node`, `layer` and `degree`) or `"planes"` (one rectangle per layer
#' around the layout, extended by `margin` times its larger side).
#' @inheritParams ggplot2::geom_point
#' @inheritParams geom_edge_3d
#' @param geom Geom to draw the output; by default [geom_edge_3d()] for
#'   edges, points for nodes and polygons for planes.
#' @param layout A layout function of an igraph graph returning one row per
#'   node with at least two columns; `NULL` uses [igraph::layout_with_fr()].
#' @param align Whether all layers share one layout.
#' @param what `"edges"`, `"nodes"` or `"planes"`.
#' @param margin Margin of the layer planes, as a fraction of the layout size.
#' @param seed Random seed for the layout, or `NULL` to use the current
#'   random number stream.
#' @return A ggplot2 layer.
#' @references De Domenico, Porter and Arenas (2015). MuxViz: a tool for
#'   multilayer analysis and visualization of networks. Journal of Complex
#'   Networks 3, 159-176. \doi{10.1093/comnet/cnu038}
#' @examples
#' if (requireNamespace("igraph", quietly = TRUE)) {
#'   set.seed(3)
#'   layers <- do.call(rbind, lapply(c("a", "b", "c"), function(l) {
#'     e <- igraph::as_edgelist(igraph::sample_gnp(12, 0.25))
#'     data.frame(from = e[, 1], to = e[, 2], layer = l, layer_end = l)
#'   }))
#'   couplings <- data.frame(from = 1:12, to = 1:12, layer = "a", layer_end = "b")
#'   d <- rbind(layers, couplings)
#'   m <- ggplot2::aes(from = from, to = to, layer = layer, layer_end = layer_end)
#'   ggplot2::ggplot(d, m) +
#'     stat_layers_3d(what = "planes", fill = "grey85", alpha = 0.4) +
#'     stat_layers_3d(ggplot2::aes(colour = ggplot2::after_stat(interlayer))) +
#'     stat_layers_3d(what = "nodes", size = 1.5) +
#'     coord_3d()
#' }
#' @export
stat_layers_3d <- function(mapping = NULL, data = NULL, geom = NULL, position = "identity", ...,
                           layout = NULL, align = TRUE, what = "edges", margin = 0.1, seed = 1L,
                           na.rm = FALSE, show.legend = NA, inherit.aes = TRUE) {
  what <- match.arg(what, c("edges", "nodes", "planes"))
  if (!is.null(layout) && !is.function(layout)) stop("layout must be a function or NULL.", call. = FALSE)
  if (!is.numeric(margin) || length(margin) != 1L || !is.finite(margin) || margin < 0) stop("margin must be a nonnegative number.", call. = FALSE)
  geom <- geom %or3d% switch(what, edges = GeomEdge3D, nodes = ggplot2::GeomPoint, planes = ggplot2::GeomPolygon)
  params <- list(layout = layout, align = align, what = what, margin = margin, seed = seed, na.rm = na.rm)
  if (inherits(geom, "GeomEdge3D")) {
    extra <- list(...)
    checked <- check_edge_params_3d(extra$shape %or3d% "line", extra$curvature, extra$n %or3d% 32L,
      extra$radius %or3d% 0.01, extra$sides %or3d% 12L, extra$arrow)
    params <- c(params, list(shade = extra$shade %or3d% (checked$shape == "tube")), checked)
  }
  make_layer_3d(geom, mapping, data, StatLayers3D, position, stats_params_3d(params, list(...)), show.legend, inherit.aes)
}

# Segment coordinates of a dendrogram: leaves at `positions` in the order of
# the tree, merges at their heights, each merge centred over its two children
# (as stats::plot.hclust draws it).
dendrogram_segments_3d <- function(tree, positions, type) {
  merge <- tree$merge
  n <- nrow(merge) + 1L
  leaf_position <- numeric(n); leaf_position[tree$order] <- positions
  centre <- height <- numeric(n - 1L)
  child <- function(id, field) if (id < 0) c(position = leaf_position[-id], height = 0)[[field]] else c(position = centre[id], height = height[id])[[field]]
  segments <- vector("list", n - 1L)
  for (i in seq_len(n - 1L)) {
    a <- merge[i, 1L]; b <- merge[i, 2L]
    xa <- child(a, "position"); xb <- child(b, "position")
    ha <- child(a, "height"); hb <- child(b, "height")
    h <- tree$height[i]
    centre[i] <- (xa + xb) / 2; height[i] <- h
    segments[[i]] <- if (type == "rectangle") {
      data.frame(leaf = c(xa, xb, xa), base = c(ha, hb, h), leaf_end = c(xa, xb, xb), base_end = c(h, h, h),
                 node = i, child = c(a, b, NA))
    } else {
      data.frame(leaf = c(xa, xb), base = c(ha, hb), leaf_end = centre[i], base_end = h, node = i, child = c(a, b))
    }
  }
  do.call(rbind, segments)
}

# Cluster of every merge whose leaves all fall in one of the k clusters of
# stats::cutree(); NA for merges joining clusters.
merge_clusters_3d <- function(tree, k) {
  membership <- unname(stats::cutree(tree, k = k))
  cluster <- integer(nrow(tree$merge))
  of <- function(id) if (id < 0) membership[[-id]] else cluster[[id]]
  for (i in seq_len(nrow(tree$merge))) {
    a <- of(tree$merge[i, 1L]); b <- of(tree$merge[i, 2L])
    cluster[i] <- if (!is.na(a) && !is.na(b) && a == b) a else NA_integer_
  }
  list(merge = cluster, leaf = membership)
}

StatDendrogram3D <- ggplot2::ggproto("StatDendrogram3D", ggplot2::Stat,
  dropped_aes = ".grid",
  compute_panel = function(data, scales, hclust, plane = "xz", at = Inf, positions = NULL,
                           height = NULL, type = "rectangle", k = NULL) {
    segments <- dendrogram_segments_3d(hclust, positions, type)
    tree_height <- max(hclust$height, 0)
    if (!is.null(height)) {
      rescale <- function(h) height[1L] + (height[2L] - height[1L]) * h / if (tree_height > 0) tree_height else 1
      segments$base <- rescale(segments$base); segments$base_end <- rescale(segments$base_end)
    }
    axes <- check_plane_axes_3d(plane)
    fixed <- axes[3L]
    out <- data.frame(segments[c("leaf", "leaf_end", "base", "base_end")])
    names(out) <- c(axes[1L], paste0(axes[1L], "end"), axes[2L], paste0(axes[2L], "end"))
    at <- plane_at_3d(at, scales, fixed)
    out[[fixed]] <- at; out[[paste0(fixed, "end")]] <- at
    out$height <- hclust$height[segments$node]
    out$node <- segments$node
    out$cluster <- if (is.null(k)) factor(NA) else {
      clusters <- merge_clusters_3d(hclust, k)
      own <- clusters$merge[segments$node]
      below <- ifelse(is.na(segments$child), NA_integer_,
        ifelse(segments$child < 0, clusters$leaf[pmax(-segments$child, 1L)], clusters$merge[pmax(segments$child, 1L)]))
      factor(ifelse(is.na(segments$child), own, below))
    }
    out$PANEL <- data$PANEL[1L]
    out$group <- -1L
    out
  }
)

#' Dendrograms on a plane
#'
#' Draws the tree of a hierarchical clustering as segments in a coordinate
#' plane of the panel, such as a wall beside a heatmap drawn on the floor. The
#' first axis of `plane` carries the leaves, the second the merge heights; the
#' remaining axis is fixed at `at`, where `Inf` and `-Inf` place the tree on
#' the box sides at the upper and lower end of that axis (0 when no other
#' layer trains that axis). Leaves sit at
#' 1, 2, ..., n in the order of the tree (`hclust$order`), which matches a
#' discrete axis whose levels are `hclust$labels[hclust$order]`; `positions`
#' moves them. Each merge is centred over its two children, as in
#' [stats::plot.hclust()]; `type = "triangle"` joins the children directly to
#' the merge point.
#'
#' Computed variables are `height` (the merge height), `node` (the merge
#' index in `hclust$merge`) and, with `k`, `cluster`: the cluster of
#' [stats::cutree()] with `k` groups that the branch belongs to, `NA` above
#' the cut, for colouring branches by cluster as in dendextend.
#' @inheritParams ggplot2::geom_point
#' @inheritParams geom_edge_3d
#' @param geom Geom drawing the segments.
#' @param hclust A hierarchical clustering, an object with a
#'   [stats::as.hclust()] method such as an `hclust` or a `dendrogram`.
#' @param plane Two distinct axis letters: the leaf axis then the height axis,
#'   for example `"xz"`, `"yz"` or `"xy"`.
#' @param at Position on the remaining axis.
#' @param positions Leaf positions in tree order; by default `seq_len(n)`.
#' @param height `NULL` to draw merge heights as they are, or two numbers: the
#'   positions of height zero and of the root. A reversed pair draws the tree
#'   hanging down.
#' @param type `"rectangle"` or `"triangle"`.
#' @param k Number of clusters for the computed `cluster` variable.
#' @return A ggplot2 layer.
#' @references de Vries and Ripley (2024). ggdendro: create dendrograms and
#'   tree diagrams using ggplot2. \url{https://CRAN.R-project.org/package=ggdendro}
#' @examples
#' m <- as.matrix(scale(mtcars[1:12, c("mpg", "hp", "wt", "qsec", "disp")]))
#' rows <- stats::hclust(stats::dist(m))
#' d <- data.frame(car = factor(rep(rownames(m), ncol(m)), levels = rownames(m)[rows$order]),
#'                 variable = rep(colnames(m), each = nrow(m)), value = as.vector(m))
#' ggplot2::ggplot(d, ggplot2::aes(car, variable)) +
#'   ggplot2::geom_tile(ggplot2::aes(fill = value)) +
#'   stat_dendrogram_3d(ggplot2::aes(colour = ggplot2::after_stat(cluster)), hclust = rows,
#'     plane = "xz", height = c(0, 3), k = 3, linewidth = 0.6) +
#'   coord_3d()
#' @export
stat_dendrogram_3d <- function(mapping = NULL, data = NULL, geom = ggplot2::GeomSegment, position = "identity", ...,
                               hclust, plane = "xz", at = Inf, positions = NULL, height = NULL,
                               type = "rectangle", k = NULL, na.rm = FALSE, show.legend = NA, inherit.aes = FALSE) {
  tree <- stats::as.hclust(hclust)
  n <- length(tree$order)
  check_plane_axes_3d(plane)
  if (!is.numeric(at) || length(at) != 1L || is.na(at)) stop("at must be a number.", call. = FALSE)
  positions <- positions %or3d% seq_len(n)
  if (!is.numeric(positions) || length(positions) != n || any(!is.finite(positions))) stop("positions must give one finite position per leaf.", call. = FALSE)
  if (!is.null(height) && (!is.numeric(height) || length(height) != 2L || any(!is.finite(height)))) stop("height must be NULL or two finite numbers.", call. = FALSE)
  if (!is.null(k) && (!is.numeric(k) || length(k) != 1L || k < 1 || k > n)) stop("k must be a number of clusters between 1 and the number of leaves.", call. = FALSE)
  type <- match.arg(type, c("rectangle", "triangle"))
  make_layer_3d(geom, mapping, stats_seed_data_3d(data), StatDendrogram3D, position,
    stats_params_3d(list(hclust = tree, plane = plane, at = at, positions = as.double(positions),
      height = height, type = type, k = k, na.rm = na.rm), list(...)), show.legend, inherit.aes)
}
