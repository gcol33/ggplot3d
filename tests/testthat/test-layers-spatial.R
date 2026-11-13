spatial_features <- function(crs = 3857) {
  square <- rbind(c(0, 0), c(4, 0), c(4, 4), c(0, 4), c(0, 0))
  hole <- rbind(c(1, 1), c(1, 3), c(3, 3), c(3, 1), c(1, 1))
  sf::st_sf(id = 1:5, size = c(1, 2, 3, 4, 5), geometry = sf::st_sfc(
    sf::st_point(c(10, 10)),
    sf::st_multipoint(rbind(c(11, 11), c(12, 12))),
    sf::st_linestring(rbind(c(0, 6), c(3, 7), c(6, 6))),
    sf::st_polygon(list(square, hole)),
    sf::st_multipolygon(list(list(square + 6), list(square + 12))), crs = crs))
}

test_that("simple features become one row per vertex with feature, part, ring and kind", {
  skip_if_not_installed("sf")
  rows <- spatial_rows_3d(spatial_features())
  expect_equal(as.vector(table(rows$.kind)[c("point", "line", "polygon")]), c(3L, 3L, 16L))
  expect_equal(rows$.feature, rep(1:5, c(1, 2, 3, 8, 8)))
  expect_equal(rows$.part[rows$.feature == 2L], 1:2)
  expect_equal(rows$.ring[rows$.feature == 4L], rep(1:2, each = 4))
  expect_equal(rows$.part[rows$.feature == 5L], rep(1:2, each = 4))
  expect_true(all(is.na(rows$.z3d)))
  expect_equal(rows$size, rep(1:5, c(1, 2, 3, 8, 8)))
  collection <- sf::st_sfc(sf::st_geometrycollection(list(sf::st_point(c(1, 2, 3)),
    sf::st_linestring(rbind(c(0, 0, 1), c(1, 1, 2))))))
  rows <- spatial_rows_3d(collection)
  expect_equal(rows$.kind, c("point", "line", "line"))
  expect_equal(rows$.part, c(1L, 2L, 2L))
  expect_equal(rows$.z3d, c(3, 1, 2))
  expect_error(spatial_rows_3d(sf::st_as_sfc("CIRCULARSTRING (0 0, 1 1, 2 0)")), "Unsupported")
})

test_that("geom_sf() layers draw points, lines and polygons on the floor of coord_3d()", {
  skip_if_not_installed("sf")
  p <- ggplot2::ggplot(spatial_features()) + ggplot2::geom_sf(ggplot2::aes(colour = size)) + coord_3d()
  s <- stats_scene(p)
  expect_length(s$packet$markers$primitive_id, 3L)
  expect_length(s$packet$faces$primitive_id, 8L + 2L * 2L)
  expect_true(length(s$packet$polylines$primitive_id) >= 1L + 2L + 2L)
  expect_equal(sort(unique(s$packet$markers$source_row)), 1:2)
  expect_equal(sort(unique(s$packet$faces$source_row)), 4:5)
  expect_true(all(s$packet$vertices$z == 0))
  expect_true(all(s$built$data[[1L]]$z == -Inf))
  expect_equal(s$built$plot@layers[[1L]]$computed_geom_params$legend, "other")
  points <- ggplot2::ggplot(spatial_features()[1:2, ]) + ggplot2::geom_sf() + coord_3d()
  points <- ggplot2::ggplot_build(points)
  expect_equal(points$plot@layers[[1L]]$computed_geom_params$legend, "point")
  expect_equal(points$data[[1L]]$shape, rep(19, 3))
})

test_that("geometries with a Z dimension stand at their own heights", {
  skip_if_not_installed("sf")
  d <- sf::st_sf(id = 1:2, geometry = sf::st_sfc(sf::st_point(c(0, 0, 5)),
    sf::st_linestring(rbind(c(0, 0, 1), c(2, 1, 3))), crs = 3857))
  s <- stats_scene(ggplot2::ggplot(d) + geom_sf_3d() + coord_3d(expand = FALSE))
  expect_equal(sort(s$built$data[[1L]]$z), c(1, 3, 5))
  expect_equal(s$built$layout$panel_params[[1L]]$z.range, c(1, 5))
})

test_that("mapped height turns polygons into prisms and lines into walls", {
  skip_if_not_installed("sf")
  features <- spatial_features()
  prisms <- stats_scene(ggplot2::ggplot(features[4:5, ]) +
    geom_sf_3d(ggplot2::aes(height = size), colour = "black") + coord_3d())
  rings <- stats_scene(ggplot2::ggplot(features[4:5, ]) +
    geom_extrude_3d(ggplot2::aes(height = size), colour = "black") + coord_3d())
  expect_equal(length(prisms$packet$faces$primitive_id), length(rings$packet$faces$primitive_id))
  expect_equal(sort(unique(prisms$built$data[[1L]]$z)), c(4, 5))
  wall <- stats_scene(ggplot2::ggplot(features[3, ]) + geom_sf_3d(height = 2, base = 1) + coord_3d())
  expect_length(wall$packet$faces$primitive_id, 4L)
  expect_equal(unique(wall$built$data[[1L]]$zmin), 1)
  expect_equal(unique(wall$built$data[[1L]]$z), 3)
  expect_error(geom_sf_3d(cap = "side"), "should be one of")
})

test_that("geographic features need a projected target CRS, which reprojects them", {
  skip_if_not_installed("sf")
  geographic <- sf::st_transform(spatial_features(), 4326)
  expect_error(ggplot2::ggplot_build(ggplot2::ggplot(geographic) + geom_sf_3d() + coord_3d()), "projected")
  s <- stats_scene(ggplot2::ggplot(geographic) + geom_sf_3d() + coord_3d(crs = 3857))
  rows <- spatial_rows_3d(spatial_features())
  expect_equal(s$built$data[[1L]]$x, rows$.x3d, tolerance = 1e-6)
  expect_equal(s$built$data[[1L]]$y, rows$.y3d, tolerance = 1e-6)
})

test_that("arcs rise above their chord by height times its normalised length", {
  d <- data.frame(x = c(0, 0), y = c(0, 0), xend = c(1, 0.5), yend = c(0, 0), z = 0)
  p <- ggplot2::ggplot(d, ggplot2::aes(x, y, xend = xend, yend = yend, z = z, zend = z)) +
    geom_arc_3d(height = 0.5, n = 4) + ggplot2::expand_limits(y = 1, z = 1) + coord_3d(expand = FALSE)
  s <- stats_scene(p)
  expect_length(s$packet$polylines$primitive_id, 2L)
  expect_equal(s$packet$polylines$count, c(5L, 5L))
  apex <- s$packet$vertices$z[s$packet$corners$vertex[s$packet$polylines$offset + 2L]]
  expect_equal(apex, c(0.5, 0.25))
  ends <- s$packet$corners$vertex[s$packet$polylines$offset + 4L]
  expect_equal(s$packet$vertices$x[ends], c(1, 0.5))
  mapped <- stats_scene(ggplot2::ggplot(transform(d, h = c(1, 0)), ggplot2::aes(x, y, xend = xend, yend = yend, z = z, zend = z)) +
    geom_arc_3d(ggplot2::aes(height = h), n = 2) + ggplot2::expand_limits(y = 1, z = 1) + coord_3d(expand = FALSE))
  expect_equal(mapped$packet$vertices$z[mapped$packet$corners$vertex[mapped$packet$polylines$offset + 1L]], c(1, 0))
  floor <- stats_scene(ggplot2::ggplot(d, ggplot2::aes(x, y, xend = xend, yend = yend)) +
    geom_arc_3d(n = 2) + coord_3d())
  expect_true(all(floor$built$data[[1L]]$z == -Inf))
  expect_error(geom_arc_3d(n = 0), "positive integer")
})

test_that("lidR point clouds fortify to snake-case point attributes", {
  skip_if_not_installed("lidR")
  las <- lidR::readLAS(system.file("extdata", "MixedConifer.laz", package = "lidR"), filter = "-keep_random_fraction 0.01")
  points <- ggplot2::fortify(las)
  expect_true(all(c("x", "y", "z", "intensity", "classification", "return_number") %in% names(points)))
  expect_equal(nrow(points), lidR::npoints(las))
  expect_equal(points$z, las@data$Z)
  s <- stats_scene(ggplot2::ggplot(points, ggplot2::aes(x, y, z = z)) + geom_point_3d() + coord_3d())
  expect_length(s$packet$markers$primitive_id, nrow(points))
})

test_that("floor layers keep their sentinel out of date-time z scales", {
  skip_if_not_installed("sf")
  track <- data.frame(x = c(0, 2, 4), y = c(0, 1, 4),
    time = as.POSIXct("2026-05-01", tz = "UTC") + c(0, 3600, 7200))
  p <- ggplot2::ggplot(track, ggplot2::aes(x, y, z = time)) +
    geom_sf_3d(data = spatial_features()[4, ], inherit.aes = FALSE) +
    geom_path_3d() + coord_3d()
  expect_no_warning(s <- stats_scene(p))
  expect_true(all(s$built$data[[1L]]$z == -Inf))
  expect_s3_class(s$built$layout$panel_scales_z[[1L]], "ScaleContinuousDatetime")
})
