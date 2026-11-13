face_rings <- function(packet) {
  table <- packet$faces
  lapply(seq_along(table$offset), function(i) {
    v <- packet$corners$vertex[seq.int(table$offset[i], length.out = table$count[i])]
    cbind(packet$vertices$x[v], packet$vertices$y[v], packet$vertices$z[v])
  })
}

test_that("plane axes and positions follow the dendrogram convention", {
  expect_equal(check_plane_axes_3d("xz"), c("x", "z", "y"))
  expect_equal(check_plane_axes_3d("zy"), c("z", "y", "x"))
  expect_error(check_plane_axes_3d("xx"), "two distinct axis letters")
  expect_error(check_plane_axes_3d("xw"), "two distinct axis letters")
  expect_equal(plane_at_3d(Inf, list(), "y"), 0)
  expect_equal(plane_at_3d(2, list(), "y"), 2)
})

test_that("grob decomposition measures every primitive in the unit square", {
  g <- grid::gTree(children = grid::gList(
    grid::rectGrob(0.25, 0.5, 0.5, 0.2, just = c("left", "bottom"), gp = grid::gpar(fill = "red", col = NA)),
    grid::polylineGrob(c(0, 0.5, NA, 0.6, 1), c(0, 0.5, NA, 0.6, 1), gp = grid::gpar(col = "blue", lwd = 2)),
    grid::segmentsGrob(0, 1, 1, 0),
    grid::polygonGrob(c(0.1, 0.2, 0.15), c(0.1, 0.1, 0.2), gp = grid::gpar(fill = "green")),
    grid::pointsGrob(0.5, 0.5, pch = 21, gp = grid::gpar(col = "black", fill = "white", fontsize = 12, lwd = 1)),
    grid::textGrob("a", 0.2, 0.8, hjust = 0, rot = 30),
    grid::circleGrob(0.8, 0.2, 0.05)),
    gp = grid::gpar(alpha = 0.5))
  p <- grob_primitives_3d(g)
  expect_equal(unlist(p$rects[c("xmin", "xmax", "ymin", "ymax")]), c(xmin = 0.25, xmax = 0.75, ymin = 0.5, ymax = 0.7))
  expect_equal(p$rects$alpha, 0.5)
  expect_equal(length(unique(p$paths$group)), 3L)
  expect_false(anyNA(p$paths$x))
  expect_equal(p$paths$linewidth[1L], 2 / ggplot2::.pt)
  expect_equal(nrow(p$points), 1L)
  expect_equal(p$points$shape, 21)
  expect_equal(p$text$label, "a")
  expect_equal(p$text$angle, 30)
  expect_equal(p$text$hjust, 0)
  expect_equal(length(unique(p$polygons$group)), 2L)
  orders <- c(p$rects$.order, p$paths$.order[1L], p$polygons$.order[1L], p$points$.order, p$text$.order)
  expect_equal(orders, sort(orders))
  expect_error(grob_primitives_3d(structure(list(name = "x", gp = NULL, vp = NULL), class = c("weird", "grob"))), "Cannot place")
})

test_that("raster cells join runs of equal colour and drop transparent cells", {
  image <- as.raster(matrix(c("red", "red", "blue", "red", "red", "transparent"), 2, byrow = TRUE))
  cells <- raster_cells_3d(image)
  expect_equal(nrow(cells), 2L)
  red <- cells[cells$fill == "red", ]
  expect_equal(unlist(red[c("xmin", "xmax", "ymin", "ymax")]), c(xmin = 0, xmax = 2 / 3, ymin = 0, ymax = 1))
  blue <- cells[cells$fill == "blue", ]
  expect_equal(unlist(blue[c("xmin", "xmax", "ymin", "ymax")]), c(xmin = 2 / 3, xmax = 1, ymin = 0.5, ymax = 1))
})

test_that("strokes become convex faces with dashes and simplified curves", {
  x <- seq(0, 1, length.out = 512)
  kept <- simplify_path_3d(x, 2 * x, 1e-6)
  expect_equal(kept, c(1L, 512L))
  wave <- simplify_path_3d(x, sin(8 * x) / 10, 1e-4)
  expect_true(length(wave) < 200 && length(wave) > 10)
  expect_null(linetype_dashes_3d(1))
  expect_equal(linetype_dashes_3d("dashed"), c(4, 4))
  expect_equal(linetype_dashes_3d(3), c(1, 3))
  expect_length(linetype_dashes_3d("blank"), 0L)
  pieces <- dash_pieces_3d(c(0, 10), c(0, 0), c(1, 1))
  expect_length(pieces, 5L)
  expect_equal(pieces[[2L]]$x, c(2, 3))
  quads <- stroke_quads_3d(c(0, 1, 1), c(0, 0, 1), 0.1)
  expect_equal(quads$count, c(4L, 4L, 3L))
  faces <- split(data.frame(x = quads$x, y = quads$y), rep(seq_along(quads$count), quads$count))
  for (face in faces) {
    xy <- as.matrix(face); k <- nrow(xy)
    turns <- vapply(seq_len(k), function(i) {
      a <- xy[i, ]; b <- xy[i %% k + 1L, ]; c <- xy[(i + 1L) %% k + 1L, ]
      (b[1] - a[1]) * (c[2] - b[2]) - (b[2] - a[2]) * (c[1] - b[1])
    }, numeric(1))
    expect_true(all(turns >= -1e-12) || all(turns <= 1e-12))
  }
})

test_that("polygon triangulation keeps its strict checks and relaxes them on request", {
  bow <- data.frame(x = c(0, 1, 0, 1), y = c(0, 1, 1, 0), z = 0)
  expect_error(triangulate_polygon_3d(bow, list(1:4)), "simple")
  expect_type(triangulate_polygon_3d(bow, list(1:4), strict = FALSE)$triangles, "list")
  repeated <- data.frame(x = c(0, 1, 1, 1, 0), y = c(0, 0, 0, 1, 1), z = 0)
  expect_error(triangulate_polygon_3d(repeated, list(1:5)), "distinct")
  expect_length(triangulate_polygon_3d(repeated, list(1:5), strict = FALSE)$triangles, 2L)
  ring <- seq(0, 2 * pi, length.out = 801)[-801]
  wavy <- data.frame(x = cos(ring) * (1 + 0.2 * sin(9 * ring)), y = sin(ring) * (1 + 0.2 * sin(9 * ring)), z = 0)
  timing <- system.time(topology <- triangulate_polygon_3d(wavy, list(seq_len(800))))
  expect_length(topology$triangles, 798L)
  expect_lt(timing[["elapsed"]], 2)
})

test_that("annotation_plot_3d lays a whole 2D plot on a wall of the box", {
  set.seed(1)
  d <- data.frame(x = rnorm(40), y = rnorm(40), z = rnorm(40))
  side <- ggplot2::ggplot(d, ggplot2::aes(x, z)) + ggplot2::geom_point() +
    ggplot2::geom_smooth(method = "lm", formula = y ~ x) + ggplot2::geom_hline(yintercept = 0, linetype = 2) +
    ggplot2::annotate("text", x = 0, y = 1, label = "note")
  scene <- stats_scene(ggplot2::ggplot(d, ggplot2::aes(x, y, z = z)) + annotation_plot_3d(side, plane = "xz", at = Inf) +
    geom_point_3d() + coord_3d())
  packet <- scene$packet
  rings <- face_rings(packet)
  expect_gt(length(rings), 40L)
  on_wall <- vapply(rings, function(r) all(abs(r[, 2L] - 1) < 1e-12), logical(1))
  expect_true(all(on_wall))
  expect_equal(sum(packet$markers$primitive_id > 0), 40L)
  expect_equal(packet$text$partition, 2L)
  params <- scene$built$layout$panel_params[[1L]]
  wall <- do.call(rbind, rings)
  side_range <- ggplot2::layer_scales(side)$x$dimension(c(0.05, 0))
  expect_equal(range(wall[, 1L]), (side_range - params$x.range[1L]) / diff(params$x.range), tolerance = 1e-6)
})

test_that("annotation_plot_3d takes an extent, a floor plane, opacity and a raster route", {
  bars <- ggplot2::ggplot(mtcars, ggplot2::aes(factor(gear))) + ggplot2::geom_bar()
  base <- ggplot2::ggplot(mtcars, ggplot2::aes(wt, hp, z = mpg)) + geom_point_3d() + coord_3d()
  scene <- stats_scene(base + annotation_plot_3d(bars, plane = "xy", at = -Inf, extent = c(2, 4, 100, 300), alpha = 0.5))
  rings <- face_rings(scene$packet)
  floor <- do.call(rbind, rings)
  expect_true(all(abs(floor[, 3L]) < 1e-12))
  params <- scene$built$layout$panel_params[[1L]]
  expect_equal(range(floor[, 1L]), (c(2, 4) - params$x.range[1L]) / diff(params$x.range), tolerance = 1e-9)
  expect_true(all(scene$packet$faces$opacity <= 0.5 + 1e-12))
  skip_if_not_installed("ragg")
  raster <- stats_scene(base + annotation_plot_3d(bars, plane = "yz", route = "raster", resolution = 24))
  expect_gt(length(raster$packet$faces$offset), 2L)
  expect_true(all(abs(do.call(rbind, face_rings(raster$packet))[, 1L] - 1) < 1e-12))
})

test_that("annotation_plot_3d checks its plot and arguments", {
  flat <- ggplot2::ggplot(mtcars, ggplot2::aes(wt, mpg)) + ggplot2::geom_point()
  expect_error(annotation_plot_3d(flat + ggplot2::facet_wrap(~cyl)), "single panel")
  expect_error(annotation_plot_3d(flat + coord_3d()), "two-dimensional")
  expect_error(annotation_plot_3d("plot"), "must be a ggplot")
  expect_error(annotation_plot_3d(flat, plane = "x"), "two distinct")
  expect_error(annotation_plot_3d(flat, extent = c(1, 1, 0, 2)), "extent")
  expect_error(annotation_plot_3d(flat, alpha = 2), "alpha")
  expect_error(annotation_plot_3d(flat, at = NA), "at must be")
})

test_that("two-dimensional ggplot2 geoms draw on the plane of their height", {
  mt <- transform(mtcars, cyl = factor(cyl))
  for (name in c("GeomBoxplot", "GeomViolin", "GeomRibbon", "GeomSmooth", "GeomErrorbar", "GeomRaster")) {
    expect_false(is.null(find_emitter_3d(getExportedValue("ggplot2", name))))
  }
  scene <- stats_scene(ggplot2::ggplot(mt, ggplot2::aes(cyl, mpg)) + ggplot2::geom_boxplot() +
    ggplot2::geom_violin(alpha = 0.3) + coord_3d())
  rings <- face_rings(scene$packet)
  expect_gt(length(rings), 10L)
  z <- do.call(rbind, rings)[, 3L]
  expect_true(all(abs(z - z[1L]) < 1e-12))
  dens <- stats_scene(ggplot2::ggplot(mt, ggplot2::aes(mpg, z = 2)) + ggplot2::geom_density() +
    ggplot2::expand_limits(z = 0) + coord_3d())
  heights <- unique(round(do.call(rbind, face_rings(dens$packet))[, 3L], 12))
  zr <- dens$built$layout$panel_params[[1L]]$z.range
  expect_equal(heights, round((2 - zr[1L]) / diff(zr), 12))
  band <- data.frame(x = 1:5, lo = 0, hi = 1:5)
  expect_error(stats_scene(ggplot2::ggplot(band, ggplot2::aes(x, ymin = lo, ymax = hi, z = x)) + ggplot2::geom_ribbon() + coord_3d()),
    "constant within each group")
})
