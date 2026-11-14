vertex_colour_cube <- function() {
  cube <- expand.grid(x = 0:1, y = 0:1, z = 0:1)
  quads <- rbind(c(1, 3, 4, 2), c(5, 6, 8, 7), c(1, 2, 6, 5),
                 c(3, 7, 8, 4), c(1, 5, 7, 3), c(2, 4, 8, 6))
  hex <- grDevices::rgb(cube$x, cube$y, cube$z)
  mesh_3d(cube, quads, vertex_data = data.frame(.vertex = 1:8, hex = hex))
}

vertex_colour_grid <- function() {
  d <- expand.grid(x = seq(-1, 1, length.out = 5), y = seq(-1, 1, length.out = 5))
  d$z <- d$x^2 - d$y^2
  d$hex <- grDevices::hsv((d$x + 1) / 2.5, 0.7, 0.9)
  d
}

test_that("mesh and surface keep literal vertex colours under smooth shading", {
  m <- vertex_colour_cube()
  mesh <- ggplot2::ggplot(m, ggplot2::aes(x, y, z = z, fill = I(hex), group = 1)) +
    geom_mesh_3d(shading = "smooth", shade = FALSE) + coord_3d()
  d <- vertex_colour_grid()
  surface <- ggplot2::ggplot(d, ggplot2::aes(x, y, z = z, fill = I(hex), group = 1)) +
    geom_surface_3d(shading = "smooth", shade = FALSE) + coord_3d()
  for (p in list(mesh, surface)) {
    b <- ggplot2::ggplot_build(p)$data[[1L]]
    expect_gt(length(unique(b$fill)), 4L)
    faces <- split(b$fill, interaction(b$.face, b[[intersect(c(".mesh", "group"), names(b))[1L]]], drop = TRUE))
    expect_true(any(vapply(faces, function(f) length(unique(f)) > 1L, logical(1))))
    file <- tempfile(fileext = ".png")
    ragg::agg_png(file, 300, 300)
    expect_no_warning(print(p))
    grDevices::dev.off()
  }
})

test_that("flat shading rejects a fill that varies within a face at draw time", {
  m <- vertex_colour_cube()
  mesh <- ggplot2::ggplot(m, ggplot2::aes(x, y, z = z, fill = I(hex), group = 1)) +
    geom_mesh_3d() + coord_3d()
  d <- vertex_colour_grid()
  surface <- ggplot2::ggplot(d, ggplot2::aes(x, y, z = z, fill = I(hex), group = 1)) +
    geom_surface_3d() + coord_3d()
  for (p in list(mesh, surface)) {
    file <- tempfile(fileext = ".png")
    ragg::agg_png(file, 300, 300)
    expect_error(print(p), "needs shading = \"smooth\"")
    grDevices::dev.off()
  }
})

test_that("numeric vertex fill is still averaged per face under flat shading", {
  cube <- expand.grid(x = 0:1, y = 0:1, z = 0:1)
  quads <- rbind(c(1, 3, 4, 2), c(5, 6, 8, 7), c(1, 2, 6, 5),
                 c(3, 7, 8, 4), c(1, 5, 7, 3), c(2, 4, 8, 6))
  m <- mesh_3d(cube, quads, vertex_data = data.frame(.vertex = 1:8, v = rowSums(cube)))
  b <- ggplot2::ggplot_build(ggplot2::ggplot(m, ggplot2::aes(x, y, z = z, fill = v)) +
    geom_mesh_3d() + coord_3d())$data[[1L]]
  expect_true(all(tapply(b$fill, b$.face, function(f) length(unique(f)) == 1L)))
  expect_length(unique(b$fill), 2L)
})
