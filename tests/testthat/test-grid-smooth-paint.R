smooth_paint_encode <- function(v) ifelse(v <= 0.0031308, 12.92 * v, 1.055 * v^(1 / 2.4) - 0.055)

smooth_paint_packet <- function(rgb, opacity = 1, colour = NA_character_, normals = NULL) {
  corners <- list(vertex = 1:3, source_row = 1:3)
  if (!is.null(normals)) corners[c("normal_x", "normal_y", "normal_z")] <- lapply(1:3, function(k) normals[, k])
  corners[c("red", "green", "blue")] <- lapply(1:3, function(k) rgb[, k])
  new_scene_packet(
    vertices = list(x = c(0.1, 0.9, 0.5), y = c(0.1, 0.1, 0.9), z = rep(0.5, 3L), w = rep(1, 3L)),
    corners = corners,
    faces = list(offset = 1L, layer_id = 1L, opacity = opacity),
    styles = data.frame(style_id = 1L, fill = "grey50", colour = colour, shade = FALSE))
}

# Vector and raster renderings of a packet on a 200 x 200 px device and the
# largest channel difference (in 1/255) over the pixels at least three pixels
# inside the face.
smooth_paint_compare <- function(packet, mode = "auto", dpi = 100, mm = 50.8,
                                 camera = new_camera_3d(elevation = 90, azimuth = -90), light = NULL, inset = 3L) {
  skip_if_not_installed("ragg")
  skip_if_not_installed("png")
  old <- options(ggplot3d.smooth_vector = mode)
  on.exit(options(old), add = TRUE)
  viewport <- new_scene_viewport(mm, mm, dpi = dpi)
  draw <- function(grob) {
    path <- tempfile(fileext = ".png")
    ragg::agg_png(path, mm / 25.4, mm / 25.4, units = "in", res = dpi, background = "white")
    grid::grid.draw(grob)
    grDevices::dev.off()
    on.exit(unlink(path))
    png::readPNG(path)[, , 1:3]
  }
  scene <- scene_compile(packet, camera, viewport)
  vector_grob <- render_scene_grid(scene, viewport, light)
  vector <- draw(vector_grob)
  raster <- draw(raster_image_grob_3d(raster_scene_3d(packet, camera, viewport, 4L, light)))
  inside <- rowSums(abs(raster - 1), dims = 2L) > 0.02
  for (step in seq_len(inset)) inside <- inside & cbind(FALSE, inside[, -ncol(inside)]) & cbind(inside[, -1L], FALSE) &
    rbind(FALSE, inside[-nrow(inside), ]) & rbind(inside[-1L, ], FALSE)
  difference <- apply(abs(vector - raster), 1:2, max) * 255
  list(error = max(difference[inside]), pixels = sum(inside), grob = vector_grob,
    difference = difference, inside = inside, scene = scene, vector = vector, raster = raster)
}

smooth_shared_edge_pixels <- function(scene, size) {
  f <- scene$fragments
  at <- rep(f$offset, f$count) + sequence(f$count) - 1L
  next_vertex <- unlist(lapply(split(seq_along(at), rep(seq_along(f$count), f$count)),
    function(i) c(i[-1L], i[1L])), use.names = FALSE)
  xy <- cbind(scene$vertices$x_npc[at], scene$vertices$y_npc[at]) * size
  key <- apply(round(xy, 5), 1L, paste, collapse = ":")
  edge <- ifelse(key < key[next_vertex], paste(key, key[next_vertex]), paste(key[next_vertex], key))
  pixels <- matrix(FALSE, size, size)
  for (i in which(duplicated(edge))) {
    a <- xy[i, ]; b <- xy[next_vertex[i], ]
    t <- seq(0.05, 0.95, length.out = max(2L, ceiling(sqrt(sum((a - b)^2)) * 3)))
    p <- outer(1 - t, a) + outer(t, b)
    col <- pmax(1L, pmin(size, floor(p[, 1L]) + 1L))
    row <- pmax(1L, pmin(size, size - floor(p[, 2L])))
    pixels[cbind(row, col)] <- TRUE
  }
  pixels
}

test_that("lit Gaussian smooth surfaces match raster along every shared triangle edge", {
  d <- expand.grid(x = seq(-2, 2, length.out = 30), y = seq(-2, 2, length.out = 30))
  d$z <- exp(-(d$x^2 + d$y^2))
  p <- ggplot2::ggplot(d, ggplot2::aes(x, y, z = z, fill = x)) +
    geom_surface_3d(shading = "smooth", colour = NA) + ggplot2::scale_fill_viridis_c() + coord_3d()
  packet <- stats_scene(p)$packet
  for (dpi in c(110, 300)) {
    out <- smooth_paint_compare(packet, dpi = dpi, mm = 101.6, camera = new_camera_3d(), light = light_3d(), inset = 4L)
    edges <- smooth_shared_edge_pixels(out$scene, nrow(out$difference)) & out$inside
    expect_gt(sum(edges), 1000L)
    expect_lte(max(out$difference[edges]), 5 + 1e-8)
    expect_lte(out$error, 5 + 1e-8)
    expect_lte(unname(stats::quantile(out$difference[edges], 0.99)), 2 + 1e-8)
  }
})

grob_classes <- function(g) {
  out <- class(g)[1L]
  if (inherits(g, "gTree")) out <- c(out, unlist(lapply(g$children, grob_classes)))
  out
}

test_that("a face whose colour varies along one direction is one gradient, without subdivision", {
  rgb <- smooth_paint_encode(c(1, 0.4, 0.05) %o% c(0.6, 0.2, 0.05))
  x <- c(5, 45, 25); y <- c(5, 5, 45)
  linear <- c(1, 0.4, 0.05) %o% c(0.6, 0.2, 0.05)
  expect_false(smooth_image_needed_3d(x, y, rep(1L, 3L), linear, gradients = TRUE))
  painted <- smooth_face_paint_3d(x, y, rep(1L, 3L), linear, gradients = TRUE)
  expect_identical(painted$gradient, TRUE)
  out <- smooth_paint_compare(smooth_paint_packet(rgb))
  expect_gt(out$pixels, 2000L)
  expect_lte(out$error, 2)
  expect_false("rastergrob" %in% grob_classes(out$grob))
})

test_that("a face with colours varying in two directions is painted as an image matching raster", {
  out <- smooth_paint_compare(smooth_paint_packet(diag(3)))
  expect_true("rastergrob" %in% grob_classes(out$grob))
  expect_gt(out$pixels, 2000L)
  expect_lte(out$error, 2)
  translucent <- smooth_paint_compare(smooth_paint_packet(diag(3), opacity = 0.5))
  expect_lte(translucent$error, 2)
  bordered <- smooth_paint_compare(smooth_paint_packet(diag(3), colour = "black"))
  expect_true(all(c("rastergrob", "polygon") %in% grob_classes(bordered$grob)))
})

test_that("opaque flat and gradient faces share continuous seam coverage", {
  xy <- expand.grid(x = c(0.1, 0.5, 0.9), y = c(0.1, 0.9))
  faces <- rbind(c(1L, 2L, 4L), c(2L, 5L, 4L), c(2L, 3L, 5L), c(3L, 6L, 5L))
  vertex <- as.vector(t(faces))
  channel <- smooth_paint_encode(c(0.25, 0.5, 0.5, 0.25, 0.5, 0.5))[vertex]
  packet <- new_scene_packet(vertices = list(x = xy$x, y = xy$y, z = rep(0.5, 6L)),
    corners = list(vertex = vertex, red = channel, green = channel, blue = channel),
    faces = list(offset = seq(1L, 10L, by = 3L), count = rep(3L, 4L), layer_id = rep(1L, 4L)),
    styles = data.frame(style_id = 1L, fill = "grey50", colour = NA_character_, shade = FALSE))
  out <- smooth_paint_compare(packet)
  expect_gt(out$pixels, 0L)
  expect_lte(out$error, 2)
})

test_that("adjacent one-direction gradients share continuous paint on vector devices", {
  index <- c(1L, 2L, 3L, 1L, 3L, 4L)
  rgb <- smooth_paint_encode(c(0.95, 0.02, 0.6, 0.25)[index] %o% c(0.6, 0.2, 0.05))
  packet <- new_scene_packet(
    vertices = list(x = c(0.1, 0.9, 0.9, 0.1), y = c(0.1, 0.1, 0.9, 0.9), z = rep(0.5, 4L), w = rep(1, 4L)),
    corners = list(vertex = index, source_row = index, red = rgb[, 1L], green = rgb[, 2L], blue = rgb[, 3L]),
    faces = list(offset = c(1L, 4L), layer_id = c(1L, 1L)),
    styles = data.frame(style_id = 1L, fill = "grey50", colour = NA_character_, shade = FALSE))
  out <- smooth_paint_compare(packet)
  expect_lte(out$error, 2 + 1e-8)
  expect_true("rastergrob" %in% grob_classes(out$grob))
  path <- tempfile()
  on.exit(unlink(paste0(path, c(".pdf", ".svg", "-cairo.pdf"))))
  devices <- list(pdf = function() grDevices::pdf(paste0(path, ".pdf"), 2, 2))
  if (capabilities("cairo")) devices$cairo <- function() grDevices::cairo_pdf(paste0(path, "-cairo.pdf"), 2, 2)
  if (requireNamespace("svglite", quietly = TRUE)) devices$svglite <- function() svglite::svglite(paste0(path, ".svg"), 2, 2)
  for (device in devices) {
    device()
    grob <- render_scene_grid(out$scene, out$scene$viewport)
    expect_true("rastergrob" %in% grob_classes(grob))
    expect_no_error(grid::grid.draw(grob))
    grDevices::dev.off()
  }
})

test_that("smooth image clipping carries only the union boundary", {
  ring <- ggplot3d:::grid_union_paths_cpp(c(0, 1, 1, 0, 0, 1), c(0, 0, 1, 0, 1, 1), rep(1:2, each = 3L))
  expect_identical(unique(ring$id), 1L)
  expect_length(ring$x, 4L)
  expect_equal(range(ring$x), c(0, 1))
  expect_equal(range(ring$y), c(0, 1))
  cells <- expand.grid(x = 0:2, y = 0:2)
  cells <- cells[cells$x != 1 | cells$y != 1, ]
  x <- as.vector(rbind(cells$x, cells$x + 1, cells$x + 1, cells$x))
  y <- as.vector(rbind(cells$y, cells$y, cells$y + 1, cells$y + 1))
  hole <- ggplot3d:::grid_union_paths_cpp(x, y, rep(seq_len(nrow(cells)), each = 4L))
  area <- vapply(split(seq_along(hole$id), hole$id), function(i) {
    j <- c(i[-1L], i[1L])
    sum(hole$x[i] * hole$y[j] - hole$x[j] * hole$y[i]) / 2
  }, numeric(1))
  expect_equal(sort(unname(area)), c(-1, 9))
})

test_that("smooth images use perspective-correct corner interpolation", {
  packet <- smooth_paint_packet(diag(3))
  packet$vertices$z <- c(0.1, 0.8, 0.4)
  out <- smooth_paint_compare(packet, dpi = 150, camera = new_camera_3d(projection = "perspective", distance = 3))
  expect_gt(out$pixels, 100L)
  expect_lte(out$error, 2 + 1e-8)
})

test_that("smooth images preserve interpolated ambient accessibility", {
  packet <- smooth_paint_packet(diag(3), normals = matrix(c(0, 0, 1), 3L, 3L, byrow = TRUE))
  packet$vertices$z[] <- 0
  packet$styles$shade <- TRUE
  mesh <- list(position = rbind(c(-2, -2, 0.25), c(2, -2, 0.25), c(2, 2, 0.25), c(-2, 2, 0.25)),
    normal = matrix(0, 4L, 3L), colour = cbind(matrix(0, 4L, 3L), 1),
    triangles = matrix(c(1L, 2L, 3L, 1L, 3L, 4L), 3L))
  light <- light_3d(ambient = 1, diffuse = 0)
  plain <- smooth_paint_compare(packet, dpi = 150, light = light)
  light$ao <- list(amount = 1, radius = 0.5, samples = 64L)
  light$occlusion <- ggplot3d:::ambient_occlusion_prepare_cpp(mesh, 64L, 0.5, 1, 1e-8)
  shaded <- smooth_paint_compare(packet, dpi = 150, light = light)
  expect_lte(shaded$error, 2 + 1e-8)
  expect_true("rastergrob" %in% grob_classes(shaded$grob))
  expect_gt(sum(plain$vector - shaded$vector > 0.05), 1000L)
})

test_that("interleaved smooth surfaces retain their compiled visibility", {
  d <- expand.grid(x = seq(-1, 1, length.out = 9), y = seq(-1, 1, length.out = 9))
  p <- ggplot2::ggplot(transform(d, z = 0.6 * x), ggplot2::aes(x, y, z = z, fill = x)) +
    geom_surface_3d(shading = "smooth", colour = NA) +
    geom_surface_3d(data = transform(d, z = -0.6 * x + 0.2 * y), shading = "smooth", colour = NA) +
    coord_3d(azimuth = -45, elevation = 60)
  out <- smooth_paint_compare(stats_scene(p)$packet, dpi = 150, mm = 76.2,
    camera = new_camera_3d(azimuth = -45, elevation = 60), light = light_3d())
  expect_lte(unname(stats::quantile(out$difference[out$inside], 0.99)), 3 + 1e-8)
  expect_lte(out$error, 5 + 1e-8)
  expect_identical(unlist(out$grob$fragment_runs, use.names = FALSE), out$scene$draw_order)
})

test_that("forced subdivision is capped and its pieces overlap inside the face", {
  rgb <- rbind(c(1, 0.8, 0.8), c(0.8, 1, 0.8), c(0.8, 0.8, 1))
  out <- smooth_paint_compare(smooth_paint_packet(rgb), mode = "subdivide")
  expect_false("rastergrob" %in% grob_classes(out$grob))
  expect_lte(out$error, 3)
  x <- c(0, 40, 10); y <- c(0, 0, 30)
  linear <- diag(3)
  painted <- smooth_face_paint_3d(x, y, rep(1L, 3L), linear, gradients = TRUE, pixel_mm = 0.01)
  expect_identical(max(painted$id), 256L)
  coarse <- smooth_face_paint_3d(x, y, rep(1L, 3L), linear, gradients = TRUE, pixel_mm = 15)
  expect_identical(max(coarse$id), 4L)
  pieces <- subdivide_colour_triangles_3d(list(cbind(0, 0), cbind(40, 0), cbind(10, 30)),
    list(linear[1L, , drop = FALSE], linear[2L, , drop = FALSE], linear[3L, , drop = FALSE]), 2 / 255,
    pixel_mm = 1, max_level = 1L)
  area <- function(P) sum(abs((P[[2L]][, 1L] - P[[1L]][, 1L]) * (P[[3L]][, 2L] - P[[1L]][, 2L]) -
    (P[[3L]][, 1L] - P[[1L]][, 1L]) * (P[[2L]][, 2L] - P[[1L]][, 2L]))) / 2
  expect_gt(area(pieces$P), 600)
  X <- do.call(rbind, pieces$P)
  weights <- solve(rbind(c(0, 40, 10), c(0, 0, 30), 1), rbind(t(X), 1))
  expect_gte(min(weights), -1e-9)
})

test_that("an interpolated colour hull draws to a small vector file", {
  skip_on_cran()
  rgb <- expand.grid(r = seq(0, 1, length.out = 5), g = seq(0, 1, length.out = 5), b = seq(0, 1, length.out = 5))
  rgb <- rgb[apply(rgb == 0 | rgb == 1, 1L, any), ]
  lab <- grDevices::convertColor(as.matrix(rgb), from = "sRGB", to = "Lab", scale.in = 1)
  gamut <- data.frame(a = lab[, 2L], b = lab[, 3L], L = lab[, 1L], hex = grDevices::rgb(rgb$r, rgb$g, rgb$b))
  p <- ggplot2::ggplot(gamut, ggplot2::aes(a, b, z = L)) +
    stat_hull_3d(ggplot2::aes(fill = I(hex), group = 1), shading = "smooth", shade = FALSE) +
    coord_3d(aspect = "data")
  path <- tempfile(fileext = ".svg")
  on.exit(unlink(path))
  time <- system.time({
    grDevices::svg(path, 4, 4)
    print(p)
    grDevices::dev.off()
  })[["elapsed"]]
  expect_lt(file.size(path), 1e6)
  expect_lt(time, 20)
})
