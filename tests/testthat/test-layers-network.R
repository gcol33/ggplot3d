edge_vertices <- function(packet, kind) {
  table <- packet[[kind]]
  lapply(seq_along(table$offset), function(i) {
    corners <- seq.int(table$offset[i], length.out = table$count[i])
    v <- packet$corners$vertex[corners]
    cbind(packet$vertices$x[v], packet$vertices$y[v], packet$vertices$z[v])
  })
}

test_that("fortify.igraph returns nodes with the edge table attached", {
  skip_if_not_installed("igraph")
  g <- igraph::make_graph(c(1, 2, 2, 3, 3, 1, 3, 4), directed = FALSE)
  igraph::V(g)$name <- c("a", "b", "c", "d")
  igraph::E(g)$weight <- 1:4
  layout <- cbind(1:4, c(0, 1, 0, 1), c(2, 2, 3, 3))
  nodes <- fortify(g, layout = layout)
  expect_equal(nodes$x, 1:4); expect_equal(nodes$z, c(2, 2, 3, 3))
  expect_equal(nodes$name, c("a", "b", "c", "d"))
  edges <- get_edges_3d()(nodes)
  expect_equal(nrow(edges), 4L)
  expect_equal(edges$weight, 1:4)
  expect_equal(edges$xend, nodes$x[edges$to])
  expect_equal(edges$zend, nodes$z[edges$to])
  expect_equal(fortify(g, layout = layout[, 1:2])$z, rep(0, 4))
  expect_error(fortify(g, layout = layout[1:3, ]), "two or three coordinates")
  set.seed(1)
  three <- fortify(g, layout = igraph::layout_with_fr)
  expect_true(all(is.finite(as.matrix(three[c("x", "y", "z")]))))
  expect_true(stats::var(three$z) > 0)
  igraph::V(g)$x <- 4:1; igraph::V(g)$y <- 1:4; igraph::V(g)$z <- 0
  expect_equal(fortify(g)$x, 4:1)
  plain <- data.frame(x = 1)
  expect_identical(get_edges_3d()(plain), plain)
})

test_that("ggplot(graph) draws edges and nodes without mappings", {
  skip_if_not_installed("igraph")
  g <- igraph::make_ring(6)
  layout <- cbind(cos(1:6), sin(1:6), 1:6)
  scene <- stats_scene(ggplot2::ggplot(g, layout = layout) + geom_edge_3d() + geom_node_3d() + coord_3d())
  expect_equal(length(scene$packet$polylines$primitive_id), 6L)
  expect_equal(scene$packet$polylines$count, rep(2L, 6))
  expect_equal(length(scene$packet$markers$primitive_id), 6L)
})

test_that("arcs bend by the sagitta set by curvature", {
  d <- data.frame(x = 0.2, y = 0.5, z = 0.5, xend = 0.8, yend = 0.5, zend = 0.5)
  p <- ggplot2::ggplot(d) + ggplot2::lims(x = c(0, 1), y = c(0, 1)) + zlim(0, 1) + coord_3d(expand = FALSE)
  arc <- edge_vertices(stats_scene(p + geom_edge_3d(shape = "arc", curvature = 1, n = 33))$packet, "polylines")[[1L]]
  expect_equal(nrow(arc), 33L)
  expect_equal(arc[1L, ], c(0.2, 0.5, 0.5)); expect_equal(arc[33L, ], c(0.8, 0.5, 0.5))
  expect_equal(sqrt(rowSums(sweep(arc, 2L, c(0.5, 0.5, 0.5))^2)), rep(0.3, 33))
  expect_equal(arc[17L, ], c(0.5, 0.2, 0.5))
  flipped <- edge_vertices(stats_scene(p + geom_edge_3d(shape = "arc", curvature = -0.5, n = 9))$packet, "polylines")[[1L]]
  expect_equal(flipped[5L, ], c(0.5, 0.65, 0.5))
  vertical <- edge_points_3d(rbind(c(0, 0, 0)), rbind(c(0, 0, 1)), 1, 5L)
  expect_equal(vertical[1L, 3L, ], c(0, 0.5, 0.5))
  line <- edge_vertices(stats_scene(p + geom_edge_3d(curvature = 1))$packet, "polylines")[[1L]]
  expect_equal(nrow(line), 2L)
})

test_that("tubes are closed polygonal cylinders of the given radius", {
  d <- data.frame(x = c(0.2, 0.5, 0.3), y = c(0.5, 0.2, 0.3), z = c(0.5, 0.2, 0.3),
                  xend = c(0.8, 0.5, 0.3), yend = c(0.5, 0.8, 0.3), zend = c(0.5, 0.8, 0.3))
  p <- ggplot2::ggplot(d) + ggplot2::lims(x = c(0, 1), y = c(0, 1)) + zlim(0, 1) + coord_3d(expand = FALSE)
  packet <- stats_scene(p + geom_edge_3d(shape = "tube", radius = 0.05, sides = 6, colour = "red"))$packet
  expect_equal(length(packet$faces$primitive_id), 2L * (6L * 2L + 2L))
  expect_equal(sort(unique(packet$faces$count)), c(3L, 6L))
  expect_true(all(packet$styles$fill == "#FF0000"))
  expect_true(all(packet$styles$shade))
  single <- ggplot2::ggplot(d[1L, ]) + ggplot2::lims(x = c(0, 1), y = c(0, 1)) + zlim(0, 1) + coord_3d(expand = FALSE)
  ring <- do.call(rbind, edge_vertices(stats_scene(single + geom_edge_3d(shape = "tube", radius = 0.05))$packet, "faces"))
  expect_equal(sqrt((ring[, 2L] - 0.5)^2 + (ring[, 3L] - 0.5)^2), rep(0.05, nrow(ring)))
  expect_equal(range(ring[, 1L]), c(0.2, 0.8))
  thick <- do.call(rbind, edge_vertices(stats_scene(single + geom_edge_3d(shape = "tube", radius = 0.05, linewidth = 1))$packet, "faces"))
  expect_equal(sqrt((thick[, 2L] - 0.5)^2 + (thick[, 3L] - 0.5)^2), rep(0.1, nrow(thick)))
  curved <- stats_scene(p + geom_edge_3d(shape = "tube", curvature = 0.5, n = 10, sides = 5))$packet
  expect_equal(length(curved$faces$primitive_id), 2L * (5L * 9L * 2L + 2L))
  expect_error(geom_edge_3d(shape = "tube", arrow = grid::arrow()), "Arrows require")
  expect_error(geom_edge_3d(shape = "spline"))
  expect_error(geom_edge_3d(radius = 0), "radius")
})

layer_edges <- function() {
  data.frame(from = c("a", "b", "c", "a", "b", "a"), to = c("b", "c", "a", "c", "c", "a"),
             layer = c("x", "x", "x", "y", "y", "x"), layer_end = c("x", "x", "x", "y", "y", "y"))
}

test_that("stat_layers_3d stacks aligned layer layouts on z planes", {
  skip_if_not_installed("igraph")
  d <- layer_edges()
  m <- ggplot2::aes(from = from, to = to, layer = layer, layer_end = layer_end)
  set.seed(10); before <- stats::runif(1); set.seed(10)
  built <- ggplot2::ggplot_build(ggplot2::ggplot(d, m) + stat_layers_3d() + stat_layers_3d(what = "nodes") +
    stat_layers_3d(what = "planes") + coord_3d())
  expect_equal(stats::runif(1), before)
  edges <- built$data[[1L]]
  expect_equal(nrow(edges), 6L)
  expect_equal(edges$interlayer, c(FALSE, FALSE, FALSE, FALSE, FALSE, TRUE))
  expect_equal(edges$z, c(1, 1, 1, 2, 2, 1)); expect_equal(edges$zend, c(1, 1, 1, 2, 2, 2))
  expect_equal(edges$x[6L], edges$xend[6L]); expect_equal(edges$y[6L], edges$yend[6L])
  nodes <- built$data[[2L]]
  expect_equal(nrow(nodes), 6L)
  expect_equal(nodes$degree[nodes$node == "a" & nodes$layer == "x"], 3L)
  a <- nodes[nodes$node == "a", ]
  expect_equal(a$x[1L], a$x[2L])
  planes <- built$data[[3L]]
  expect_equal(nrow(planes), 8L)
  expect_equal(sort(unique(planes$z)), c(1, 2))
  expect_true(min(planes$x) < min(nodes$x) && max(planes$x) > max(nodes$x))
  again <- ggplot2::ggplot_build(ggplot2::ggplot(d, m) + stat_layers_3d() + coord_3d())$data[[1L]]
  expect_equal(again$x, edges$x)
  free <- ggplot2::ggplot_build(ggplot2::ggplot(d, m) + stat_layers_3d(align = FALSE, what = "nodes") + coord_3d())$data[[1L]]
  expect_equal(nrow(free), 6L)
  numeric <- transform(d, layer = ifelse(layer == "x", 0, 5), layer_end = ifelse(layer_end == "x", 0, 5))
  z <- ggplot2::ggplot_build(ggplot2::ggplot(numeric, m) + stat_layers_3d() + coord_3d())$data[[1L]]$zend
  expect_equal(z, c(0, 0, 0, 5, 5, 5))
  scene <- stats_scene(ggplot2::ggplot(d, m) + stat_layers_3d(shape = "tube") + coord_3d())
  expect_gt(length(scene$packet$faces$primitive_id), 0L)
  expect_error(stat_layers_3d(layout = "fr"), "layout must be a function")
})

test_that("stat_dendrogram_3d draws the tree in the chosen plane", {
  hc <- stats::hclust(stats::dist(c(1, 2, 6, 7, 20)))
  built <- ggplot2::ggplot_build(ggplot2::ggplot() + stat_dendrogram_3d(hclust = hc, at = 3) + coord_3d())
  seg <- built$data[[1L]]
  expect_equal(nrow(seg), 3L * 4L)
  expect_true(all(seg$y == 3 & seg$yend == 3))
  leaves <- seg[seg$z == 0, ]
  expect_setequal(leaves$x, 1:5)
  expect_equal(max(seg$zend), max(hc$height))
  root <- seg[seg$node == 4L & seg$z == seg$zend, ]
  expect_equal(root$z, max(hc$height))
  scaled <- ggplot2::ggplot_build(ggplot2::ggplot() + stat_dendrogram_3d(hclust = hc, plane = "yz", height = c(10, 0)) + coord_3d())$data[[1L]]
  expect_equal(range(c(scaled$z, scaled$zend)), c(0, 10))
  expect_setequal(scaled$y[scaled$z == 10], 1:5)
  expect_true(all(scaled$x == 0))
  tri <- ggplot2::ggplot_build(ggplot2::ggplot() + stat_dendrogram_3d(hclust = as.dendrogram(hc), type = "triangle", plane = "xy") + coord_3d())$data[[1L]]
  expect_equal(nrow(tri), 2L * 4L)
  k <- ggplot2::ggplot_build(ggplot2::ggplot() + stat_dendrogram_3d(hclust = hc, k = 2) + coord_3d())$data[[1L]]
  groups <- stats::cutree(hc, 2)
  leaf_rows <- k[k$z == 0, ]
  expect_equal(as.integer(as.character(leaf_rows$cluster)), unname(groups[hc$order[leaf_rows$x]]))
  root <- k[k$node == 4L, ]
  expect_true(is.na(root$cluster[root$z == root$zend]))
  expect_setequal(as.character(root$cluster[root$z != root$zend]), c("1", "2"))
  expect_error(stat_dendrogram_3d(hclust = hc, plane = "xx"), "two distinct")
  expect_error(stat_dendrogram_3d(hclust = hc, positions = 1:3), "one finite position")
  scene <- stats_scene(ggplot2::ggplot() + stat_dendrogram_3d(hclust = hc) + coord_3d())
  expect_equal(length(scene$packet$polylines$primitive_id), 12L)
})
