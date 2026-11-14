band_hill <- function(shading = "smooth", scale = ggplot2::scale_fill_viridis_b()) {
  d <- expand.grid(x = seq(-2, 2, length.out = 36), y = seq(-2, 2, length.out = 36))
  d$z <- with(d, exp(-(x^2 + y^2)) + 0.5 * exp(-((x - 1)^2 + (y + 1)^2) * 2))
  ggplot2::ggplot(d, ggplot2::aes(x, y, z = z, fill = z)) +
    geom_surface_3d(shading = shading, colour = NA, shade = FALSE) + scale + coord_3d()
}

band_face_values <- function(packet) {
  at <- rep(packet$faces$offset, packet$faces$count) + sequence(packet$faces$count) - 1L
  v <- packet$corners$vertex[at]
  list(x = packet$vertices$x[v], y = packet$vertices$y[v], z = packet$vertices$z[v],
       owner = rep(seq_along(packet$faces$count), packet$faces$count), at = at)
}

band_image <- function(grob, size = 400L) {
  path <- tempfile(fileext = ".png")
  on.exit(unlink(path))
  ragg::agg_png(path, size, size, background = "white")
  tryCatch(grid::grid.draw(grob), finally = grDevices::dev.off())
  png::readPNG(path)[, , 1:3]
}

test_that("surface bands split on scalar iso-lines under both shading modes", {
  for (shading in c("flat", "smooth")) {
    out <- stats_scene(band_hill(shading))
    packet <- out$packet
    scale <- out$built$plot@scales$get_scales("fill")
    f <- band_face_values(packet)
    range <- out$built$layout$panel_params[[1L]]$ranges$z
    value <- f$z * diff(range) + range[1L]
    breaks <- scale$get_breaks()
    crosses <- vapply(split(value, f$owner), function(v)
      any(breaks > min(v) + 1e-12 & breaks < max(v) - 1e-12), logical(1))
    expect_false(any(crosses))
    expect_gt(length(packet$faces$count), 2450L)
    expect_lt(length(packet$faces$count), 4900L)
    mid <- vapply(split(value, f$owner), mean, numeric(1))
    expect_identical(grDevices::col2rgb(packet$styles$fill[packet$faces$style_id]), grDevices::col2rgb(unname(scale$map(mid))))
    if (!is.null(packet$corners$red)) {
      rgb <- t(grDevices::col2rgb(packet$styles$fill[packet$faces$style_id[f$owner]])) / 255
      expect_equal(cbind(packet$corners$red[f$at], packet$corners$green[f$at], packet$corners$blue[f$at]), rgb, ignore_attr = TRUE)
    }
  }
})

test_that("the vector hill bands agree with the analytic field away from iso-lines", {
  skip_if_not_installed("ragg")
  skip_if_not_installed("png")
  out <- stats_scene(band_hill())
  packet <- out$packet
  viewport <- new_scene_viewport(101.6, 101.6, dpi = 100)
  camera <- new_camera_3d(elevation = 90, azimuth = -90)
  scene <- scene_compile(packet, camera, viewport)
  img <- band_image(render_scene_grid(scene, viewport, NULL))
  x <- ((seq_len(400) - 0.5) / 400 - min(scene$vertices$x_npc)) / diff(range(scene$vertices$x_npc)) * 4 - 2
  y <- ((400 - seq_len(400) + 0.5) / 400 - min(scene$vertices$y_npc)) / diff(range(scene$vertices$y_npc)) * 4 - 2
  xx <- rep(x, each = 400); yy <- rep(y, 400)
  field <- exp(-(xx^2 + yy^2)) + 0.5 * exp(-((xx - 1)^2 + (yy + 1)^2) * 2)
  scale <- out$built$plot@scales$get_scales("fill")
  expected <- t(grDevices::col2rgb(scale$map(field))) / 255
  interior <- abs(xx) < 1.96 & abs(yy) < 1.96
  for (b in scale$get_breaks()) interior <- interior & abs(field - b) > 0.01
  difference <- apply(abs(matrix(img, ncol = 3L) - expected), 1L, max)
  expect_gt(sum(interior), 30000L)
  expect_lt(mean(difference[interior] > 2 / 255), 0.005)
})

test_that("mesh triangles and polygons use transformed binned fill and colour values", {
  vertices <- data.frame(x = c(0, 1, 1, 0), y = c(0, 0, 1, 1), z = c(0, 1, 1, 0))
  mesh <- mesh_3d(vertices, matrix(1:4, 1L), vertex_data = data.frame(.vertex = 1:4, value = c(1, 100, 100, 1)))
  for (aes in c("fill", "colour")) for (shading in c("flat", "smooth")) {
    mapping <- ggplot2::aes(x, y, z = z)
    mapping[[aes]] <- rlang::quo(value)
    scale <- if (aes == "fill") ggplot2::scale_fill_steps(trans = "log10", breaks = c(10), limits = c(1, 100)) else
      ggplot2::scale_colour_fermenter(trans = "log10", breaks = c(10), limits = c(1, 100))
    out <- stats_scene(ggplot2::ggplot(mesh, mapping) + geom_mesh_3d(shading = shading) + scale + coord_3d())
    p <- out$packet
    f <- band_face_values(p)
    x_range <- out$built$layout$panel_params[[1L]]$ranges$x
    x <- f$x * diff(x_range) + x_range[1L]
    cut <- (10 - 1) / (100 - 1)
    expect_true(any(abs(x - cut) < 1e-12))
    expect_false(any(vapply(split(x, f$owner), function(v) min(v) < cut - 1e-12 && max(v) > cut + 1e-12, logical(1))))
    if (aes == "fill") {
      expected <- out$built$plot@scales$get_scales(aes)$map(log10(1 + vapply(split(x, f$owner), mean, numeric(1)) * 99))
      expect_identical(grDevices::col2rgb(p$styles$fill[p$faces$style_id]), grDevices::col2rgb(expected))
    } else {
      at <- rep(p$polylines$offset, p$polylines$count) + sequence(p$polylines$count) - 1L
      x <- p$vertices$x[p$corners$vertex[at]] * diff(x_range) + x_range[1L]
      owner <- rep(seq_along(p$polylines$count), p$polylines$count)
      expect_false(any(vapply(split(x, owner), function(v) min(v) < cut - 1e-12 && max(v) > cut + 1e-12, logical(1))))
      expected <- out$built$plot@scales$get_scales(aes)$map(log10(1 + vapply(split(x, owner), mean, numeric(1)) * 99))
      expect_identical(grDevices::col2rgb(p$styles$colour[p$polylines$style_id]), grDevices::col2rgb(expected))
    }
  }
})

test_that("a piecewise constant continuous palette is split at its actual jumps", {
  palette <- function(x) ifelse(x <= 0.37, "navy", ifelse(x <= 0.81, "gold", "firebrick"))
  d <- expand.grid(x = 0:1, y = 0:1); d$z <- d$x
  p <- ggplot2::ggplot(d, ggplot2::aes(x, y, z = z, fill = x)) + geom_surface_3d() +
    ggplot2::continuous_scale("fill", palette = palette, limits = c(0, 1)) + coord_3d()
  out <- stats_scene(p)
  f <- band_face_values(out$packet)
  range <- out$built$layout$panel_params[[1]]$ranges$x
  x <- f$x * diff(range) + range[1L]
  for (cut in c(0.37, 0.81)) {
    expect_true(any(abs(x - cut) < 1e-12))
    expect_false(any(vapply(split(x, f$owner), function(v) min(v) < cut - 1e-12 && max(v) > cut + 1e-12, logical(1))))
  }
})

test_that("band cuts preserve linear normals, texture coordinates and triangle area", {
  d <- data.frame(x = c(0, 1, 0), y = c(0, 0, 1), z = 0, .source = 1:3, .layer = 1L,
    fill = c("navy", "gold", "navy"), nx = c(0, 0.6, 0), ny = c(0, 0, 0.8), nz = c(1, 0.8, 0.6),
    u = c(0, 1, 0), v = c(0, 0, 1), .band_fill = c(0, 1, 0))
  params <- list(shading = "smooth", .band_scales = list(fill = list(breaks = c(0.25, 0.75),
    map = function(x) ifelse(x <= 0.25, "navy", ifelse(x <= 0.75, "gold", "tomato")))))
  p <- packet_from_primitives_3d(d, 1:3, "faces", params, 1L, 3L)
  f <- band_face_values(p)
  weights <- cbind(1 - f$x - f$y, f$x, f$y)
  expected <- weights %*% as.matrix(d[c("nx", "ny", "nz")])
  actual <- cbind(p$corners$normal_x[f$at], p$corners$normal_y[f$at], p$corners$normal_z[f$at])
  expect_equal(actual, expected, ignore_attr = TRUE, tolerance = 1e-14)
  area <- vapply(split(seq_along(f$x), f$owner), function(i) {
    j <- c(i[-1L], i[1L])
    abs(sum(f$x[i] * f$y[j] - f$x[j] * f$y[i])) / 2
  }, numeric(1))
  expect_equal(sum(area), 0.5, tolerance = 1e-14)
  split <- band_split_primitives_3d(d, 1:3, "faces", 3L, params)
  expect_equal(split$data$u, split$data$x)
  expect_equal(split$data$v, split$data$y)
})

test_that("vector, raster and viewer share the same band faces and smooth paint", {
  skip_if_not_installed("ragg")
  skip_if_not_installed("png")
  skip_if_not_installed("jsonlite")
  out <- stats_scene(band_hill())
  packet <- out$packet
  view <- view_packet_3d(packet)
  decode_int <- function(column) readBin(jsonlite::base64_dec(column$b64), "integer", n = 1e6, size = 4L, endian = "little")
  expect_identical(decode_int(view$faces$count), packet$faces$count)
  expect_identical(decode_int(view$corners$vertex), packet$corners$vertex)
  viewport <- new_scene_viewport(101.6, 101.6, dpi = 100)
  camera <- new_camera_3d(elevation = 90, azimuth = -90)
  scene <- scene_compile(packet, camera, viewport)
  vector <- band_image(render_scene_grid(scene, viewport, NULL))
  raster <- band_image(raster_image_grob_3d(raster_scene_3d(packet, camera, viewport, 4L, NULL)))
  difference <- apply(abs(vector - raster), 1:2, max)
  expect_lt(mean(difference > 5 / 255), 0.01)
  packet$styles$shade <- TRUE
  light <- lights_3d(light_directional(), mode = "tone", shadow = "#22305a", highlight = "#fff3d0")
  scene <- scene_compile(packet, camera, viewport)
  vector <- band_image(render_scene_grid(scene, viewport, light))
  raster <- band_image(raster_image_grob_3d(raster_scene_3d(packet, camera, viewport, 4L, light)))
  expect_lt(mean(apply(abs(vector - raster), 1:2, max) > 5 / 255), 0.01)
})

test_that("continuous scale packets and compiled scenes are byte-identical with the band path bypassed", {
  plots <- unlist(lapply(c("flat", "smooth"), function(shading)
    list(band_hill(shading, ggplot2::scale_fill_viridis_c()),
         band_hill(shading, ggplot2::scale_fill_gradient(low = "#eeeeee", high = "#ffffff")))), recursive = FALSE)
  packets <- lapply(plots, function(p) stats_scene(p)$packet)
  baseline <- testthat::with_mocked_bindings(lapply(plots, function(p) stats_scene(p)$packet), band_scale_3d = function(scale) NULL)
  viewport <- new_scene_viewport(60, 60)
  for (i in seq_along(packets)) {
    expect_identical(serialize(packets[[i]], NULL), serialize(baseline[[i]], NULL))
    expect_identical(serialize(scene_compile(packets[[i]], new_camera_3d(), viewport), NULL),
      serialize(scene_compile(baseline[[i]], new_camera_3d(), viewport), NULL))
  }
})

test_that("scale limits cut a face before transparent out-of-range bands are discarded", {
  d <- expand.grid(x = 0:1, y = 0:1); d$z <- d$x
  for (geom in list(geom_surface_3d(shading = "smooth"), geom_surface_3d())) {
    out <- stats_scene(ggplot2::ggplot(d, ggplot2::aes(x, y, z = z, fill = x)) + geom +
      ggplot2::scale_fill_steps(breaks = 0.5, limits = c(0.2, 0.8), oob = scales::censor, na.value = NA) + coord_3d())
    f <- band_face_values(out$packet)
    ranges <- out$built$layout$panel_params[[1L]]$ranges
    x <- f$x * diff(ranges$x) + ranges$x[1L]
    y <- f$y * diff(ranges$y) + ranges$y[1L]
    area <- vapply(split(seq_along(x), f$owner), function(i) {
      j <- c(i[-1L], i[1L]); abs(sum(x[i] * y[j] - x[j] * y[i])) / 2
    }, numeric(1))
    expect_equal(sum(area[out$packet$faces$opacity == 1]), 0.6, tolerance = 1e-12)
  }
})

test_that("banded faces keep exact visibility against the independent referee", {
  source(testthat::test_path("..", "fixtures", "oracle", "reference.R"), local = TRUE)
  d <- expand.grid(x = 0:1, y = 0:1); d$z <- 0.4 * d$x
  p <- ggplot2::ggplot(d, ggplot2::aes(x, y, z = z, fill = x)) +
    geom_surface_3d(shading = "smooth") + ggplot2::scale_fill_stepsn(colours = c("navy", "gold", "tomato"),
      breaks = c(0.2, 0.6), limits = c(0, 1)) +
    geom_point_3d(data = data.frame(x = 0.5, y = 0.5, z = 0.6), inherit.aes = FALSE,
      ggplot2::aes(x, y, z = z), colour = "black", size = 4) + coord_3d()
  packet <- stats_scene(p)$packet
  viewport <- new_scene_viewport(60, 60)
  for (camera in list(new_camera_3d(), new_camera_3d(projection = "perspective"))) {
    compiled <- scene_compile(packet, camera, viewport)
    expect_true(compiled$capabilities$exact_visibility)
    score <- ref_score(packet, camera, viewport, compiled, n = 80L)
    expect_gt(score$scored, 0)
    expect_equal(score$wrong, 0L)
  }
})

test_that("implicit binned limits train on vertex samples for flat and smooth shading", {
  d <- expand.grid(x = 0:1, y = 0:1); d$z <- d$x
  flat <- stats_scene(ggplot2::ggplot(d, ggplot2::aes(x, y, z = z, fill = x)) +
    geom_surface_3d() + ggplot2::scale_fill_steps(breaks = c(0.2, 0.8)) + coord_3d())
  smooth <- stats_scene(ggplot2::ggplot(d, ggplot2::aes(x, y, z = z, fill = x)) +
    geom_surface_3d(shading = "smooth") + ggplot2::scale_fill_steps(breaks = c(0.2, 0.8)) + coord_3d())
  expect_identical(flat$built$plot@scales$get_scales("fill")$get_limits(), c(0, 1))
  expect_identical(flat$packet$faces$count, smooth$packet$faces$count)
  expect_identical(flat$packet$styles$fill[flat$packet$faces$style_id], smooth$packet$styles$fill[smooth$packet$faces$style_id])
  palette <- scales::new_continuous_palette(function(x) ifelse(x <= 0.37, "navy", "gold"), type = "colour")
  custom <- stats_scene(ggplot2::ggplot(d, ggplot2::aes(x, y, z = z, fill = x)) + geom_surface_3d() +
    ggplot2::continuous_scale("fill", palette = palette) + coord_3d())
  expect_identical(custom$built$plot@scales$get_scales("fill")$get_limits(), c(0, 1))
  expect_gt(length(custom$packet$faces$count), 2L)
})

test_that("date-valued band variables retain their transformation class when remapped", {
  d <- expand.grid(x = 0:1, y = 0:1); d$z <- d$x
  d$value <- as.Date("2026-01-01") + 10 * d$x
  out <- stats_scene(ggplot2::ggplot(d, ggplot2::aes(x, y, z = z, fill = value)) + geom_surface_3d() +
    ggplot2::binned_scale("fill", palette = scales::pal_seq_gradient(), transform = "date",
      limits = as.Date(c("2026-01-01", "2026-01-11")), breaks = as.Date("2026-01-05")) + coord_3d())
  f <- band_face_values(out$packet)
  r <- out$built$layout$panel_params[[1L]]$ranges$x
  x <- f$x * diff(r) + r[1L]
  expect_true(any(abs(x - 0.4) < 1e-12))
  expect_false(any(vapply(split(x, f$owner), function(v) min(v) < 0.4 - 1e-12 && max(v) > 0.4 + 1e-12, logical(1))))
})
