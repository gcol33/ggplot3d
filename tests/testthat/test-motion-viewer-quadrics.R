# A top view of spheres in the unit box, unlit and flat unless `light` is
# given, on a white panel without furniture.
viewer_sphere_plot <- function(spheres, light = NULL, layers = list(), alpha = 1) {
  ggplot2::ggplot(spheres, ggplot2::aes(x, y, z = z)) + layers +
    geom_sphere_3d(ggplot2::aes(radius = r), fill = "#2050C0", alpha = alpha, shade = !is.null(light)) +
    ggplot2::scale_x_continuous(limits = c(0, 1), expand = c(0, 0)) +
    ggplot2::scale_y_continuous(limits = c(0, 1), expand = c(0, 0)) + scale_z_continuous(limits = c(0, 1), expand = c(0, 0)) +
    coord_3d(azimuth = -90, elevation = 90, light = light, panes = character(), box = "none", axes = character()) +
    ggplot2::theme_void() + ggplot2::theme(panel.background = ggplot2::element_rect(fill = "white", colour = NA))
}

# Canvas pixels per box unit along x, from the projected box corners.
viewer_box_scale <- function(b, id) {
  box <- b$eval(sprintf("document.getElementById('%s').ggplot3d.getProjectedBox(1)", id))
  x <- vapply(box, `[[`, numeric(1), 1L)
  ratio <- b$eval(sprintf("(() => { const c = document.querySelector('#%s canvas'); return c.width/c.getBoundingClientRect().width; })()", id))
  (max(x) - min(x)) * ratio
}

blue_pixels <- function(px) px$rgba[, 3L] > 150 & px$rgba[, 1L] < 120

test_that("the viewer receives quadric rows instead of inscribed meshes and picks them", {
  skip_if_not_installed("htmlwidgets")
  spheres <- data.frame(x = c(0.3, 0.7), y = c(0.3, 0.6), z = 0.5, r = c(0.15, 0.2))
  w <- view_3d(viewer_sphere_plot(spheres), panel = 1, width = 240, height = 240, elementId = "rows")
  scene <- w$x$panels[[1L]]$scene
  expect_identical(decode_column(scene$quadrics$kind), c(1L, 1L))
  expect_length(decode_column(scene$faces$offset), 0L)
  to_box <- matrix(decode_column(scene$quadrics$to_box), 16L)
  to_local <- matrix(decode_column(scene$quadrics$to_local), 16L)
  for (i in 1:2) expect_equal(matrix(to_box[, i], 4L) %*% matrix(to_local[, i], 4L), diag(4), tolerance = 1e-5)
  expect_equal(to_box[13:15, ], rbind(spheres$x, spheres$y, spheres$z), tolerance = 1e-6)
  b <- viewer_browser(list(w))
  on.exit(b$close(), add = TRUE)
  hits <- viewer_hits(b, "rows", step = 6)
  expect_setequal(unique(vapply(hits, `[[`, integer(1), "primitive_id")), decode_column(scene$quadrics$primitive_id))
  expect_setequal(unique(vapply(hits, `[[`, integer(1), "row")), 1:2)
  expect_equal(b$eval("document.querySelector('#rows canvas').getContext('webgl2').getError()"), 0)
})

test_that("a ray-cast sphere covers its exact disc, not the inscribed polygon", {
  skip_if_not_installed("htmlwidgets")
  r <- 0.3
  w <- view_3d(viewer_sphere_plot(data.frame(x = 0.5, y = 0.5, z = 0.5, r = r)), panel = 1, width = 300, height = 300, elementId = "disc")
  b <- viewer_browser(list(w), width = 320, height = 320)
  on.exit(b$close(), add = TRUE)
  radius <- r * viewer_box_scale(b, "disc")
  area <- sum(blue_pixels(viewer_canvas_pixels(b, "disc")$disc))
  segments <- getOption("ggplot3d.quadric_segments", 32L)
  polygon <- 0.5 * segments * sin(2 * pi / segments) * radius^2
  expect_gt(pi * radius^2 - polygon, 100)
  expect_lt(abs(area - pi * radius^2), 0.25 * (pi * radius^2 - polygon))
})

test_that("ray-cast fragments carry the exact depth where a plane cuts a sphere", {
  skip_if_not_installed("htmlwidgets")
  r <- 0.3; level <- 0.5 + 0.6 * r
  plane <- data.frame(x = c(0, 1, 1, 0), y = c(0, 0, 1, 1), z = level)
  cut <- list(geom_polygon_3d(data = plane, ggplot2::aes(x, y, z = z), inherit.aes = FALSE, fill = "#D04020", colour = NA, shade = FALSE))
  w <- view_3d(viewer_sphere_plot(data.frame(x = 0.5, y = 0.5, z = 0.5, r = r), layers = cut), panel = 1,
    width = 300, height = 300, elementId = "cut")
  b <- viewer_browser(list(w), width = 320, height = 320)
  on.exit(b$close(), add = TRUE)
  radius <- 0.8 * r * viewer_box_scale(b, "cut")
  area <- sum(blue_pixels(viewer_canvas_pixels(b, "cut")$cut))
  expect_lt(abs(area - pi * radius^2), 2 * pi * radius)
})

test_that("translucent quadrics accumulate their near and far sides", {
  skip_if_not_installed("htmlwidgets")
  alpha <- 0.4
  w <- view_3d(viewer_sphere_plot(data.frame(x = 0.5, y = 0.5, z = 0.5, r = 0.3), alpha = alpha), panel = 1,
    width = 200, height = 200, elementId = "glass")
  b <- viewer_browser(list(w), width = 220, height = 220)
  on.exit(b$close(), add = TRUE)
  px <- viewer_canvas_pixels(b, "glass")$glass
  centre <- px$rgba[(px$h %/% 2) * px$w + px$w %/% 2 + 1L, 1:3]
  covered <- 1 - (1 - alpha)^2
  linear <- covered * ggplot3d:::srgb_decode_3d(grDevices::col2rgb("#2050C0")[, 1L] / 255) + (1 - covered)
  expected <- 255 * ggplot3d:::srgb_encode_3d(linear)
  expect_lte(max(abs(centre - expected)), 3)
})

test_that("lit ray-cast quadrics take the raster backend's colours", {
  skip_if_not_installed("htmlwidgets")
  skip_if_not_installed("ragg")
  plot <- viewer_sphere_plot(data.frame(x = 0.5, y = 0.5, z = 0.5, r = 0.3), light = light_3d())
  w <- view_3d(plot, panel = 1, width = 240, height = 240, elementId = "lit")
  b <- viewer_browser(list(w), width = 260, height = 260)
  on.exit(b$close(), add = TRUE)
  px <- viewer_canvas_pixels(b, "lit")$lit
  file <- tempfile(fileext = ".png")
  ragg::agg_png(file, 240, 240, res = 96)
  print(plot + coord_3d(azimuth = -90, elevation = 90, light = light_3d(), panes = character(), box = "none",
    axes = character(), backend = backend_3d("raster")))
  grDevices::dev.off()
  raster <- png::readPNG(file) * 255
  # Colour at the image of the sphere's centre, and halfway to its rim on
  # four sides, against the same points of the raster image.
  disc <- function(rows, cols, colour) {
    k <- which(colour(rows, cols))
    list(row = mean(rows[k]), col = mean(cols[k]), radius = sqrt(length(k) / pi))
  }
  grid <- expand.grid(row = seq_len(px$h), col = seq_len(px$w))
  rgba <- px$rgba[(px$h - grid$row) * px$w + grid$col, ]
  viewer <- disc(grid$row, grid$col, function(r, c) rgba[, 3L] > rgba[, 1L] + 40)
  rgrid <- expand.grid(row = seq_len(dim(raster)[1L]), col = seq_len(dim(raster)[2L]))
  rpix <- cbind(raster[, , 1][cbind(rgrid$row, rgrid$col)], raster[, , 2][cbind(rgrid$row, rgrid$col)], raster[, , 3][cbind(rgrid$row, rgrid$col)])
  static <- disc(rgrid$row, rgrid$col, function(r, c) rpix[, 3L] > rpix[, 1L] + 40)
  offsets <- rbind(c(0, 0), c(0.5, 0), c(-0.5, 0), c(0, 0.5), c(0, -0.5))
  for (i in seq_len(nrow(offsets))) {
    at <- function(d) round(c(d$row, d$col) + offsets[i, ] * d$radius)
    v <- at(viewer); s <- at(static)
    expect_lte(max(abs(rgba[(v[2L] - 1L) * px$h + v[1L], 1:3] - rpix[(s[2L] - 1L) * dim(raster)[1L] + s[1L], ])), 4)
  }
})

test_that("quadrics with an unbounded image after camera pre become inscribed meshes", {
  quadrics <- quadric_rows(c(1L, 1L), c(diag(0.2, 3L), diag(0.1, 3L)), c(0.5, 0.5, 0.5, 0.5, 0.5, 0.2))
  packet <- quadric_packet(quadrics)
  # w = 1 - (x + y + z) / 2 stays positive on both spheres but not on the
  # bounding box of the first.
  pre <- diag(4); pre[4L, 1:3] <- -0.5
  cast <- ggplot3d:::view_quadrics_3d(packet$quadrics, pre)
  expect_identical(cast$rows, 2L)
  view <- ggplot3d:::view_packet_3d(packet, pre)
  expect_identical(decode_column(view$quadrics$kind), 1L)
  expect_gt(length(decode_column(view$faces$offset)), 100L)
  expect_true(all(decode_column(view$faces$source_row) == 1L))
  expect_equal(matrix(decode_column(view$quadrics$to_box), 4L), pre %*% rbind(cbind(diag(0.1, 3L), c(0.5, 0.5, 0.2)), c(0, 0, 0, 1)),
    tolerance = 1e-6)
})
