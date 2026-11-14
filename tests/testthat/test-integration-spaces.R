test_that("Euclidean spaces preserve packets and expose their metric and actions", {
  d <- data.frame(x = c(1, 3, 2), y = c(1, 2, 3), z = c(2, 3, 1))
  p <- ggplot2::ggplot(d, ggplot2::aes(x, y, z = z)) + geom_path_3d() + geom_point_3d()
  a <- integration_packets(p + coord_3d())
  b <- integration_packets(p + coord_3d(space = SpaceEuclidean, model = "cartesian", input = "cartesian"))
  expect_identical(serialize(a$packet, NULL), serialize(b$packet, NULL))
  X <- rbind(c(1, 2, 3), c(0, 0, 0)); Y <- X + 1
  expect_equal(SpaceEuclidean$distance(X, Y), rep(sqrt(3), 2))
  expect_identical(SpaceEuclidean$geodesic(X, Y, 0), X)
  g <- diag(4); g[1:3, 4] <- c(2, 3, 4)
  expect_equal(SpaceEuclidean$act(g, X), sweep(X, 2, c(2, 3, 4), "+"))
  expect_error(SpaceEuclidean$act(matrix(1, 4, 4), X), "rigid")
  expect_error(coord_3d(space = "sphere"), "Space3D")
  expect_error(coord_3d(model = "missing"), "model")
  expect_error(coord_3d(input = "missing"), "chart")
  expect_error(coord_3d(munch_tolerance = 0), "positive")
  coord <- coord_3d()
  coord$isometry <- g
  expect_identical(coord$bind_space(a$params)$isometry, g)
})

test_that("sphere geodesics satisfy analytic distance and screen chord error", {
  p <- space_test_plot()
  params <- ggplot2::ggplot_build(p)$layout$panel_params[[1]]
  context <- params$space_context; space <- context$space
  X <- rbind(c(1, 0, 0)); Y <- rbind(c(0, 1, 0))
  expect_equal(space$distance(X, Y), pi / 2)
  expect_equal(space$geodesic(X, Y, 0.5), rbind(c(1, 1, 0) / sqrt(2)))
  sampler <- space_subdivision_3d(context)
  path <- polyline_model_3d(sampler, rbind(X, Y))
  expect_gt(nrow(path$model), 2L)
  expect_equal(rowSums(path$scale^2), rep(1, nrow(path$scale)), tolerance = 1e-12)
  s <- (head(path$s, -1) + tail(path$s, -1)) / 2
  midpoint <- sampler$metric(sampler$map(space$geodesic(X[rep(1, length(s)), , drop = FALSE], Y[rep(1, length(s)), , drop = FALSE], s)))
  projected <- sampler$metric(path$model)
  error <- chord_distance_3d(midpoint, head(projected, -1), tail(projected, -1))
  expect_lte(max(error), context$tolerance)
  reverse <- polyline_model_3d(sampler, rbind(Y, X))
  expect_identical(unname(path$model), unname(reverse$model[nrow(reverse$model):1, ]))
  expect_error(polyline_model_3d(sampler, rbind(X, -X)), "Antipodal")
  expect_error(polyline_model_3d(sampler, rbind(X, Y), max_level = 0L), "max_depth")
  expect_equal(p$coordinates$distance(c(1, 0), c(0, 1), params, c(0, 0)), pi / 2)
})

test_that("paths, segments, missing rows and attributes use the same munch", {
  d <- data.frame(x = c(1, 0), y = c(0, 1), z = 0, alpha = c(0.2, 0.8), group = 1L, .source = 1:2, .layer = 1L)
  params <- ggplot2::ggplot_build(space_test_plot())$layout$panel_params[[1]]
  context <- params$space_context
  result <- munch_3d(normalise_positions_3d(d, params), 1:2, "polylines", 2L, context)
  expect_gt(nrow(result$data), 2)
  expect_equal(range(result$data$alpha), c(0.2, 0.8))
  expect_true(any(abs(result$data$alpha - 0.5) < 1e-12))
  expect_true(all(result$data$.source == 1L))
  expect_equal(sum(result$start), 1L)
  expect_equal(sum(result$end), 1L)
  d <- d[setdiff(names(d), c(".source", ".layer"))]
  p <- ggplot2::ggplot(d, ggplot2::aes(x, y, z = z))
  coord <- space_test_plot()$coordinates
  path <- integration_packets(p + geom_path_3d() + coord)$packet
  segment <- integration_packets(ggplot2::ggplot(data.frame(x = 1, y = 0, z = 0, xend = 0, yend = 1, zend = 0),
    ggplot2::aes(x, y, z = z, xend = xend, yend = yend, zend = zend)) + geom_segment_3d() + coord)$packet
  expect_equal(path$vertices, segment$vertices)
  broken <- rbind(d[1, ], transform(d[1, ], x = NA_real_), d[2, ])
  packet <- suppressWarnings(integration_packets(ggplot2::ggplot(broken, ggplot2::aes(x, y, z = z)) + geom_path_3d() + coord)$packet)
  expect_length(packet$polylines$offset, 0)
})

test_that("spherical polygon faces and boundaries reach both renderers", {
  reference <- integration_packets(space_test_plot())$packet
  expect_gt(length(reference$faces$offset), 1)
  expect_true(all(reference$faces$count == 3L))
  expect_gt(reference$polylines$count[1], 4)
  xyz <- scene_vertices_xyz_3d(reference$vertices) * 2 - 1
  # Boundary lattice points lie on the shared chord subdivision.
  boundary <- rowSums(abs(xyz) < 1e-12) > 0
  expect_equal(rowSums(xyz[!boundary, , drop = FALSE]^2), rep(1, sum(!boundary)), tolerance = 1e-12)
  expect_lte(max(rowSums(xyz^2)), 1 + 1e-12)
  for (backend in c("vector", "raster")) {
    p <- space_test_plot(backend)
    packet <- integration_packets(p)$packet
    expect_identical(packet$vertices, reference$vertices)
    file <- tempfile(fileext = ".png")
    testthat::skip_if_not_installed("ragg")
    ragg::agg_png(file, width = 320, height = 320)
    tryCatch(expect_no_error(print(p)), finally = grDevices::dev.off())
    expect_gt(file.info(file)$size, 1000)
    unlink(file)
  }
})

test_that("canonical dimension, charts, projectivity and homogeneous output are independent", {
  lifted <- ggplot2::ggproto("Lifted", SpaceEuclidean, identity = FALSE,
    to_canonical = function(data, chart) cbind(as.matrix(data[c("x", "y", "z")]), 1),
    from_canonical = function(X, model) X[, 1:3, drop = FALSE])
  p <- ggplot2::ggplot(data.frame(x = 0:1, y = 0:1, z = 0:1), ggplot2::aes(x, y, z = z)) +
    geom_path_3d() + coord_3d(space = lifted)
  result <- integration_packets(p)
  expect_length(result$packet$vertices$x, 2L)
  context <- result$params$space_context
  expect_equal(space_packet_coordinates_3d(matrix(c(0, 0, 0, 1), 1), context), list(x = 1/22, y = 1/22, z = 1/22, w = 1))
  context$space <- ggplot2::ggproto(NULL, lifted, from_canonical = function(X, model) X)
  expect_error(space_packet_coordinates_3d(matrix(c(1, 0, 0, -1), 1), context), "w >= 0")
  expect_equal(homogeneous_xyz_3d(do.call(cbind, space_packet_coordinates_3d(matrix(c(1, 0, 0, 2), 1), context))),
               homogeneous_xyz_3d(do.call(cbind, space_packet_coordinates_3d(matrix(c(0.5, 0, 0, 1), 1), context))))
  ideal <- space_packet_coordinates_3d(matrix(c(1, 0, 0, 0), 1), context)
  expect_identical(ideal$w, 0)
  expect_equal(ideal$x, 1 / diff(context$ranges$x))
  sphere <- test_sphere_space()
  expect_equal(sphere$to_canonical(data.frame(x = 0, y = 0, z = 0), "lonlat"), matrix(c(1, 0, 0), 1))
})

test_that("screen tolerance responds to zoom and perspective at drawing time", {
  d <- data.frame(x = c(1, 0), y = c(0, 1), z = 0)
  p <- ggplot2::ggplot(d, ggplot2::aes(x, y, z = z)) + geom_path_3d() + space_test_plot()$coordinates
  result <- integration_packets(p)
  packet <- result$grobs[[1]]$packet
  camera <- result$params$camera
  camera$zoom <- 4 * camera$zoom
  resolved <- resolve_draw_packets_3d(list(packet), camera, new_scene_viewport(150, 80))[[1]]
  expect_gt(resolved$polylines$count, packet$polylines$count)
  context <- attr(resolved, "draw_recipe")$panel_params$space_context
  context$camera$projection <- "perspective"
  context$camera$distance <- 3
  sampler <- space_subdivision_3d(context)
  path <- polyline_model_3d(sampler, as.matrix(d))
  screen <- sampler$metric(path$model)
  mids <- context$space$geodesic(head(path$scale, -1), tail(path$scale, -1), 0.5)
  error <- chord_distance_3d(sampler$metric(sampler$map(mids)), head(screen, -1), tail(screen, -1))
  expect_lte(max(error), context$tolerance)
})

test_that("geodesic face boundaries agree with their stroked edges", {
  result <- integration_packets(space_test_plot())
  packet <- result$packet
  xyz <- scene_vertices_xyz_3d(packet$vertices)
  line <- packet$polylines
  edge <- xyz[packet$corners$vertex[seq.int(line$offset[1], length.out = line$count[1])], , drop = FALSE]
  face <- xyz[unique(packet$corners$vertex[unlist(lapply(packet$faces$offset, function(k) k + 0:2))]), , drop = FALSE]
  boundary <- face[rowSums(abs(face - 0.5) < 1e-12) > 0, , drop = FALSE]
  distance <- vapply(seq_len(nrow(edge) - 1L), function(i) {
    a <- edge[rep(i, nrow(boundary)), , drop = FALSE]
    b <- edge[rep(i + 1L, nrow(boundary)), , drop = FALSE]
    d <- b - a
    s <- pmax(0, pmin(1, rowSums((boundary - a) * d) / sum(d[1, ]^2)))
    sqrt(rowSums((boundary - a - s * d)^2))
  }, numeric(nrow(boundary)))
  expect_lt(max(apply(distance, 1, min)), 1e-12)
  context <- result$params$space_context
  sampler <- space_subdivision_3d(context)
  points <- diag(3)
  mesh <- munch_triangles_3d(sampler, points[1, , drop = FALSE], points[2, , drop = FALSE], points[3, , drop = FALSE], max_level = context$max_depth)
  tri <- mesh$triangles
  w <- (mesh$weights[tri[, 1], ] + mesh$weights[tri[, 2], ] + mesh$weights[tri[, 3], ]) / 3
  repeated <- function(i) points[rep(i, nrow(w)), , drop = FALSE]
  exact <- sampler$map(triangle_points_3d(sampler, repeated(1), repeated(2), repeated(3), w[, 2], w[, 3]))
  interpolated <- (mesh$model[tri[, 1], ] + mesh$model[tri[, 2], ] + mesh$model[tri[, 3], ]) / 3
  expect_lte(max(sqrt(rowSums((sampler$metric(exact) - sampler$metric(interpolated))^2))), context$tolerance)
})

test_that("continuous paint and custom packets share the coordinate adapter", {
  d <- data.frame(colour = c("#000000", "#FFFFFF"), fill = c("red", "blue"), alpha = c(0, 1), .source = 1:2)
  out <- space_attributes_3d(d, matrix(c(1L, 2L), 1), matrix(c(0.5, 0.5), 1))
  expect_equal(out$alpha, 0.5)
  expect_equal(as.numeric(grDevices::col2rgb(out$colour)), rep(128, 3), tolerance = 1)
  params <- ggplot2::ggplot_build(space_test_plot())$layout$panel_params[[1]]
  p <- new_scene_packet(vertices = list(x = c(1, 0.5), y = c(0.5, 1), z = c(0.5, 0.5)),
    corners = list(vertex = 1:2, source_row = 1:2), polylines = list(offset = 1L, count = 2L),
    styles = data.frame(style_id = 1L, colour = "black"))
  mapped <- space_packet_3d(p, params$space_context)
  expect_gt(mapped$polylines$count, 2)
  expect_equal(rowSums((scene_vertices_xyz_3d(mapped$vertices) * 2 - 1)^2), rep(1, length(mapped$vertices$x)), tolerance = 1e-12)
})

test_that("input charts and isometries act before display projection", {
  d <- data.frame(x = c(0, pi / 2), y = 0, z = 0)
  rotate <- matrix(c(0, 1, 0, -1, 0, 0, 0, 0, 1), 3)
  p <- ggplot2::ggplot(d, ggplot2::aes(x, y, z = z)) + geom_point_3d() +
    coord_3d(space = test_sphere_space(), input = "lonlat", isometry = rotate)
  result <- integration_packets(p)
  xyz <- scene_vertices_xyz_3d(result$packet$vertices)
  expect_equal(unname(xyz), rbind(c(0.5, 1, 0.5), c(0, 0.5, 0.5)), tolerance = 1e-12)
  projected <- result$params$space_context$space$to_canonical(d, "lonlat")
  expect_equal(projected[1, ], c(1, 0, 0))
  helper <- p$coordinates$transform(d, result$params)
  expect_equal(unname(as.matrix(helper[c(".x_model", ".y_model", ".z_model")])), unname(xyz), tolerance = 1e-12)
})

test_that("the homogeneous adapter delegates all four coordinates to the schema helper", {
  context <- list(space = ggplot2::ggproto(NULL, SpaceEuclidean,
    from_canonical = function(X, model) X), model = "cartesian",
    ranges = list(x = c(-1, 1), y = c(-2, 2), z = c(-3, 3)))
  adapter <- space_packet_coordinates_3d
  environment(adapter) <- list2env(list(normalise_homogeneous_3d = function(x, y, z, w, ranges) list(x, y, z, w, ranges)),
    parent = environment(adapter))
  X <- rbind(c(1, 2, 3, 0), c(4, 5, 6, 2))
  value <- adapter(X, context)
  expect_identical(value, list(X[, 1], X[, 2], X[, 3], X[, 4], context$ranges))
})
