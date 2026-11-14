horizontal_quad <- function(z) list(vertices = list(x = c(0, 1, 1, 0), y = c(0, 0, 1, 1), z = rep(z, 4L)))
# A square viewport of `px` raster pixels a side at 144 dpi (a hair under, so
# that rounding the size up to whole pixels cannot add one).
quadric_view <- function(px = 240L) new_scene_viewport((px - 1e-6) * 25.4 / 144, (px - 1e-6) * 25.4 / 144, 144)

# Millimetres on screen per box unit (orthographic) and the screen centre.
screen_jacobian <- function(scene, viewport) {
  cam <- scene$camera
  diag(c(0.5 * cam$projection[1, 1] * viewport$width_mm, 0.5 * cam$projection[2, 2] * viewport$height_mm)) %*%
    rbind(cam$view[1L, 1:3], cam$view[2L, 1:3]) %*% diag(cam$model_scale)
}

vector_pixels <- function(scene, viewport, px) {
  capture <- ragg::agg_capture(width = px, height = px, units = "px", res = viewport$dpi, background = "white")
  grid::grid.newpage()
  grid::pushViewport(grid::viewport(width = grid::unit(viewport$width_mm, "mm"), height = grid::unit(viewport$height_mm, "mm")))
  grid::grid.draw(ggplot3d:::render_scene_grid(scene, viewport))
  image <- capture(native = FALSE)
  grDevices::dev.off()
  grDevices::col2rgb(image)
}
# Straight RGB of a raster result over white, pixels in column-major order.
raster_pixels <- function(result) {
  h <- result$height; w <- result$width
  packed <- as.double(result$image)[rep((seq_len(h) - 1L) * w, w) + rep(seq_len(w), each = h)]
  packed[packed < 0] <- packed[packed < 0] + 2^32
  channels <- vapply(0:3, function(k) (packed %/% 256^k) %% 256, numeric(length(packed)))
  alpha <- channels[, 4L] / 255
  t(round(channels[, 1:3] * alpha + 255 * (1 - alpha)))
}

test_that("the outline of an ellipsoid is the exact projected ellipse", {
  linear <- matrix(c(0.2, 0.05, 0, 0, 0.1, 0.03, 0.02, 0, 0.15), 3L)
  packet <- quadric_packet(quadric_rows(1L, linear, c(0.5, 0.5, 0.5)))
  camera <- new_camera_3d(azimuth = 20, elevation = 35, aspect = c(2, 1, 1.5))
  viewport <- new_scene_viewport(120, 90, 144)
  scene <- scene_compile(packet, camera, viewport)
  outline <- ggplot3d:::quadric_outline_cpp(scene$source, camera, viewport, 1L, 0.001)
  expect_true(outline$bounded)
  jl <- screen_jacobian(scene, viewport) %*% linear
  y <- cbind(outline$x_npc * viewport$width_mm - viewport$width_mm / 2, outline$y_npc * viewport$height_mm - viewport$height_mm / 2)
  residual <- colSums(t(y) * solve(jl %*% t(jl), t(y))) - 1
  expect_lt(max(abs(residual)), 1e-9)
  expect_gt(length(residual), 100L)
})

test_that("a sphere's outline under perspective is the cone of sight lines tangent to it", {
  r <- 0.2; centre <- c(0.6, 0.4, 0.5)
  packet <- quadric_packet(quadric_rows(1L, diag(r, 3L), centre))
  camera <- new_camera_3d(projection = "perspective", azimuth = 40, elevation = 25, distance = 2)
  viewport <- new_scene_viewport(100, 80, 144)
  scene <- scene_compile(packet, camera, viewport)
  outline <- ggplot3d:::quadric_outline_cpp(scene$source, camera, viewport, 1L, 0.001)
  cam <- scene$camera
  sight <- outer((2 * outline$x_npc - 1) / cam$projection[1, 1], cam$view[1L, 1:3]) +
    outer((2 * outline$y_npc - 1) / cam$projection[2, 2], cam$view[2L, 1:3]) +
    matrix(cam$view[3L, 1:3], length(outline$x_npc), 3L, byrow = TRUE)
  axis <- centre - cam$eye
  cosine <- drop(sight %*% axis) / sqrt(rowSums(sight^2)) / sqrt(sum(axis^2))
  expect_equal(cosine, rep(sqrt(1 - r^2 / sum(axis^2)), length(cosine)), tolerance = 1e-10)
})

test_that("a cylinder's outline is the convex hull of its end ellipses", {
  linear <- cbind(c(0.1, 0, 0), c(0, 0.1, 0), c(0.2, 0.1, 0.3))
  linear[, 1:2] <- qr.Q(qr(cbind(linear[, 3L], c(1, 0, 0), c(0, 1, 0))))[, 2:3] * 0.1
  origin <- c(0.35, 0.4, 0.3)
  for (kind in 2:3) {
    ratio <- if (kind == 2L) 1 else 0.4
    packet <- quadric_packet(quadric_rows(kind, linear, origin, radius_ratio = ratio))
    camera <- new_camera_3d(azimuth = -30, elevation = 20)
    viewport <- new_scene_viewport(100, 100, 144)
    scene <- scene_compile(packet, camera, viewport)
    outline <- ggplot3d:::quadric_outline_cpp(scene$source, camera, viewport, 1L, 0.001)
    j <- screen_jacobian(scene, viewport)
    y <- cbind(outline$x_npc * 100 - 50, outline$y_npc * 100 - 50)
    on_end <- vapply(0:1, function(z) {
      centre <- drop(j %*% (origin + z * linear[, 3L] - 0.5))
      shape <- j %*% (linear[, 1:2] * c(1, ratio)[z + 1L])
      d <- t(y) - centre
      abs(colSums(d * solve(shape %*% t(shape), d)) - 1)
    }, numeric(nrow(y)))
    expect_lt(max(apply(on_end, 1L, min)), 1e-9)
    a <- seq(0, 2 * pi, length.out = 50L)
    ends <- do.call(rbind, lapply(0:1, function(z) t(j %*% (origin - 0.5 + linear %*% rbind(c(1, ratio)[z + 1L] * cos(a),
      c(1, ratio)[z + 1L] * sin(a), z)))))
    # Every point of the end circles lies inside the outline, up to its chord error.
    edge <- y[c(seq.int(2L, nrow(y)), 1L), ] - y
    length <- sqrt(rowSums(edge^2))
    inside <- vapply(seq_len(nrow(ends)), function(i)
      min((edge[, 1L] * (ends[i, 2L] - y[, 2L]) - edge[, 2L] * (ends[i, 1L] - y[, 1L])) / length), numeric(1))
    expect_gt(min(inside), -1e-3)
  }
})

test_that("a plane cuts a sphere on the exact circle in vector and raster output", {
  skip_if_not_installed("ragg")
  r <- 0.25; plane <- 0.5 + 0.6 * r
  packet <- quadric_packet(quadric_rows(1L, diag(r, 3L), c(0.5, 0.5, 0.5)), horizontal_quad(plane))
  camera <- new_camera_3d(azimuth = -90, elevation = 90)
  px <- 240L; viewport <- quadric_view(px)
  scene <- scene_compile(packet, camera, viewport)
  vector <- vector_pixels(scene, viewport, px)
  raster <- ggplot3d:::raster_scene_3d(packet, camera, viewport, 1L, buffers = TRUE)
  rgb <- raster_pixels(raster)
  j <- screen_jacobian(scene, viewport)
  pixel_mm <- 25.4 / viewport$dpi
  centre <- (c(px, px) + 1) / 2
  grid <- expand.grid(col = seq_len(px), row = seq_len(px))
  radius_mm <- sqrt((grid$col - centre[1L])^2 + (grid$row - centre[2L])^2) * pixel_mm
  cut <- 0.8 * r * sqrt(sum(j[1L, ]^2)); rim <- r * sqrt(sum(j[1L, ]^2))
  red <- function(rgb) rgb[1L, ] > 200 & rgb[2L, ] < 80
  inside <- radius_mm < cut - 1.5 * pixel_mm
  ring <- radius_mm > cut + 1.5 * pixel_mm & radius_mm < rim - 1.5 * pixel_mm
  index <- grid$row + (grid$col - 1L) * px
  for (image in list(vector, rgb)) {
    expect_true(all(red(image[, index[inside]])))
    expect_false(any(red(image[, index[ring]])))
  }
  expect_true(all(raster$buffers$kind[cbind(grid$row, grid$col)][inside] == 5L))
  expect_lt(mean(colSums(abs(vector - rgb)) > 60), 0.02)
})

test_that("raster and vector agree on intersecting quadrics, meshes and a projective pre", {
  skip_if_not_installed("ragg")
  quadrics <- quadric_rows(c(1L, 2L), c(diag(0.2, 3L), cbind(c(0.08, 0, 0), c(0, 0.08, 0), c(0, 0.3, 0.5))),
    c(0.5, 0.5, 0.45, 0.5, 0.2, 0.15))
  packet <- quadric_packet(quadrics, list(vertices = list(x = c(0.1, 0.9, 0.9, 0.1), y = c(0.3, 0.3, 0.8, 0.8),
    z = c(0.3, 0.3, 0.7, 0.7))))
  pre <- diag(4); pre[4L, 3L] <- 0.3
  px <- 200L; viewport <- quadric_view(px)
  for (camera in list(new_camera_3d(azimuth = -40, elevation = 25),
                      new_camera_3d(projection = "perspective", azimuth = 30, elevation = 35),
                      new_camera_3d(azimuth = -40, elevation = 25, pre = pre))) {
    scene <- scene_compile(packet, camera, viewport)
    vector <- vector_pixels(scene, viewport, px)
    raster <- raster_pixels(ggplot3d:::raster_scene_3d(packet, camera, viewport, 1L))
    expect_lt(mean(colSums(abs(vector - raster)) > 60), 0.03)
  }
})

test_that("raster samples take the exact depth and normal of the quadric", {
  r <- 0.25
  packet <- quadric_packet(quadric_rows(1L, diag(r, 3L), c(0.5, 0.5, 0.5)))
  camera <- new_camera_3d(azimuth = -90, elevation = 90)
  px <- 81L; viewport <- quadric_view(px)
  result <- ggplot3d:::raster_scene_3d(packet, camera, viewport, 1L, buffers = TRUE)
  scene <- scene_compile(packet, camera, viewport)
  j <- screen_jacobian(scene, viewport)
  mm_per_box <- sqrt(sum(j[1L, ]^2))
  normal <- cbind(as.vector(result$buffers$normal$x), as.vector(result$buffers$normal$y), as.vector(result$buffers$normal$z))
  depth <- as.vector(result$buffers$depth)
  hit <- which(as.vector(result$buffers$kind) == 5L)
  expect_gt(length(hit), 300L)
  expect_equal(rowSums(normal[hit, ]^2), rep(1, length(hit)), tolerance = 1e-12)
  # Seen from above, a sample at horizontal distance d from the axis lies
  # sqrt(r^2 - d^2) above the centre, with normal z component sqrt(1 - d^2 / r^2).
  centre_depth <- depth[(px + 1L) / 2 + px * ((px - 1L) / 2)]
  expect_equal(normal[(px + 1L) / 2 + px * ((px - 1L) / 2), ], c(0, 0, 1), tolerance = 1e-12)
  height <- normal[hit, 3L] * r
  expect_equal(depth[hit] - centre_depth, r - height, tolerance = 1e-9)
})

test_that("quadric compilation and rasterisation do not depend on the thread count", {
  quadrics <- quadric_rows(c(1L, 3L, 1L), c(diag(0.2, 3L), cbind(c(0.1, 0, 0), c(0, 0.1, 0), c(0, 0, 0.6)), diag(0.1, 3L)),
    c(0.5, 0.5, 0.5, 0.3, 0.6, 0.1, 0.7, 0.3, 0.6), radius_ratio = c(1, 0.3, 1))
  camera <- new_camera_3d(azimuth = 25, elevation = 30)
  viewport <- quadric_view(120L)
  old <- options(ggplot3d.threads = 1L, ggplot3d.cache_bytes = 0); on.exit(options(old))
  for (opacity in c(1, 0.5)) {
    packet <- quadric_packet(c(quadrics, list(opacity = rep(opacity, 3L))), horizontal_quad(0.55))
    options(ggplot3d.threads = 1L)
    serial <- scene_compile(packet, camera, viewport)
    image <- ggplot3d:::raster_scene_3d(packet, camera, viewport, 2L)$image
    options(ggplot3d.threads = 3L)
    threaded <- scene_compile(packet, camera, viewport)
    expect_identical(threaded[c("vertices", "fragments", "draw_order", "masks")], serial[c("vertices", "fragments", "draw_order", "masks")])
    expect_identical(ggplot3d:::raster_scene_3d(packet, camera, viewport, 2L)$image, image)
  }
})

test_that("the far side is ordered where translucency, an open end or a cut shows it", {
  camera <- new_camera_3d(azimuth = -30, elevation = 25)
  viewport <- new_scene_viewport(100, 100, 144)
  sides <- function(packet, camera = new_camera_3d(azimuth = -30, elevation = 25)) {
    scene <- scene_compile(packet, camera, viewport)
    f <- scene$fragments; v <- scene$vertices
    parts <- ggplot3d:::quadric_fragment_parts_cpp(scene$source, camera, viewport, f$source_index,
      cbind(f$normal_x, f$normal_y, f$normal_z), cbind(v$x[f$offset], v$y[f$offset], v$z[f$offset]))
    list(back = parts$back, part = parts$part)
  }
  sphere <- quadric_rows(1L, diag(0.2, 3L), c(0.5, 0.5, 0.5))
  expect_false(any(sides(quadric_packet(sphere))$back))
  expect_true(any(sides(quadric_packet(c(sphere, list(opacity = 0.5))))$back))
  cut <- new_camera_3d(azimuth = -30, elevation = 25, near = 2.7)
  expect_true(any(sides(quadric_packet(sphere), cut)$back))
  tube <- quadric_rows(2L, cbind(c(0.1, 0, 0), c(0, 0.1, 0), c(0, 0, 0.5)), c(0.5, 0.5, 0.25))
  closed <- sides(quadric_packet(tube))
  expect_false(any(closed$back))
  expect_true(all(c(0L, 2L) %in% closed$part))
  open <- quadric_packet(c(tube[names(tube) != "cap_1"], list(cap_1 = 0L)))
  open_sides <- sides(open)
  expect_true(any(open_sides$back))
  expect_false(2L %in% open_sides$part)
})

# Three translucent spheres that overlap on screen without touching in space.
translucent_spheres <- function() {
  quadric_packet(c(quadric_rows(rep(1L, 3L), rep(diag(0.12, 3L), 3L), c(0.3, 0.5, 0.3, 0.5, 0.5, 0.5, 0.7, 0.5, 0.7)),
    list(opacity = rep(0.5, 3L))), fill = "#2050C0")
}

test_that("each visible side of a translucent quadric is painted in one run", {
  packet <- translucent_spheres()
  for (camera in list(new_camera_3d(azimuth = -30, elevation = 25), new_camera_3d(projection = "perspective", azimuth = 70, elevation = 10))) {
    viewport <- quadric_view(200L)
    scene <- scene_compile(packet, camera, viewport)
    grDevices::pdf(NULL)
    runs <- render_scene_grid(scene, viewport)$fragment_runs
    grDevices::dev.off()
    f <- scene$fragments
    info <- ggplot3d:::quadric_fragment_info_3d(scene, which(f$kind == 5L))
    side <- vapply(runs, function(run) paste(f$source_index[run[1L]], info$part[run[1L]], info$back[run[1L]]), character(1))
    expect_setequal(side, paste(rep(1:3, each = 2L), 0L, c(TRUE, FALSE)))
    expect_false(anyDuplicated(side) > 0L)
  }
})

test_that("translucent quadrics show no seams inside their vector images", {
  skip_if_not_installed("ragg")
  px <- 300L; viewport <- quadric_view(px)
  scene <- scene_compile(translucent_spheres(), new_camera_3d(azimuth = -30, elevation = 25), viewport)
  colour <- colSums(vector_pixels(scene, viewport, px) * c(65536, 256, 1))
  # Pixel centres in pixels from the bottom left, in the column-major order
  # of the captured image.
  grid <- expand.grid(row = seq_len(px), col = seq_len(px))
  x <- grid$col - 0.5; y <- px - grid$row + 0.5
  # Flat films have one colour wherever the same set of outlines encloses a
  # pixel; pixels within 1.5 px of an outline take antialiased colours.
  rings <- ggplot3d:::quadric_fragment_info_3d(scene, which(scene$fragments$kind == 5L))$ring
  inside <- matrix(FALSE, length(x), length(rings)); near <- rep(FALSE, length(x))
  for (k in seq_along(rings)) {
    rx <- rings[[k]]$x * px; ry <- rings[[k]]$y * px; n <- length(rx); nxt <- c(2:n, 1L)
    for (i in seq_len(n)) {
      ax <- rx[i]; ay <- ry[i]; bx <- rx[nxt[i]]; by <- ry[nxt[i]]
      crosses <- (ay > y) != (by > y) & x < ax + (y - ay) * (bx - ax) / (by - ay)
      inside[crosses, k] <- !inside[crosses, k]
      t <- pmin(1, pmax(0, ((x - ax) * (bx - ax) + (y - ay) * (by - ay)) / ((bx - ax)^2 + (by - ay)^2)))
      near <- near | (x - ax - t * (bx - ax))^2 + (y - ay - t * (by - ay))^2 < 1.5^2
    }
  }
  key <- apply(inside, 1L, paste, collapse = "")[!near]
  shades <- tapply(colour[!near], key, function(v) length(unique(v)))
  expect_gte(length(shades), 5L)
  expect_true(all(shades == 1L))
})

test_that("an unbounded image draws the stand-in facets", {
  packet <- quadric_packet(quadric_rows(1L, diag(0.45, 3L), c(0.5, 0.5, 0.5)))
  camera <- new_camera_3d(projection = "perspective", distance = 0.3, clip_data = FALSE)
  viewport <- new_scene_viewport(100, 100, 144)
  scene <- scene_compile(packet, camera, viewport)
  expect_gt(length(scene$fragments$kind), 0L)
  rows <- which(scene$fragments$kind == 5L)
  info <- ggplot3d:::quadric_fragment_info_3d(scene, rows)
  expect_null(info$ring[[1L]])
  drawn <- ggplot3d:::quadric_run_grob_3d(scene, rows, info, scene$styles[scene$fragments$style_id, , drop = FALSE], NULL)
  expect_null(drawn$clip)
  expect_s3_class(drawn$grob, "polygon")
})

test_that("viewer and export packets carry inscribed meshes of the quadrics", {
  linear <- matrix(c(0.2, 0.05, 0, 0, 0.1, 0.03, 0.02, 0, 0.15), 3L)
  packet <- quadric_packet(quadric_rows(c(1L, 3L), c(linear, linear), c(0.5, 0.5, 0.5, 0.2, 0.2, 0.2), radius_ratio = c(1, 0.5)))
  finite <- ggplot3d:::scene_finite_packet_3d(packet)
  expect_length(finite$quadrics$kind, 0L)
  expect_gt(length(finite$faces$offset), 100L)
  xyz <- scene_vertices_xyz_3d(finite$vertices)
  sphere <- finite$faces$source_row == 1L
  corners <- unlist(lapply(which(sphere), function(f) finite$faces$offset[f] + seq_len(finite$faces$count[f]) - 1L))
  local <- t(solve(linear, t(xyz[finite$corners$vertex[corners], ]) - 0.5))
  expect_equal(sqrt(rowSums(local^2)), rep(1, length(corners)), tolerance = 1e-12)
  normal <- cbind(finite$corners$normal_x, finite$corners$normal_y, finite$corners$normal_z)[corners, ]
  expect_equal(rowSums(normal^2), rep(1, length(corners)), tolerance = 1e-12)
  expect_equal(normal, local %*% solve(linear) / sqrt(rowSums((local %*% solve(linear))^2)), tolerance = 1e-12)
})

test_that("clip planes keep, drop or mesh quadrics", {
  quadrics <- quadric_rows(c(1L, 1L, 1L), rep(diag(0.1, 3L), 3L), c(0.2, 0.5, 0.5, 0.5, 0.5, 0.5, 0.8, 0.5, 0.5))
  packet <- quadric_packet(quadrics)
  plane <- list(list(normal = c(-1, 0, 0), offset = 0.55, cap = FALSE))
  cut <- ggplot3d:::clip_packet_planes_3d(packet, plane)
  expect_identical(cut$quadrics$transform_13, 0.2)
  expect_gt(length(cut$faces$offset), 0L)
  corners <- unlist(Map(function(o, n) o + seq_len(n) - 1L, cut$faces$offset, cut$faces$count))
  expect_true(all(scene_vertices_xyz_3d(cut$vertices)[cut$corners$vertex[corners], 1L] <= 0.55 + 1e-12))
  expect_false(anyDuplicated(c(cut$faces$primitive_id, cut$quadrics$primitive_id)) > 0L)
})

test_that("spheres, tubes and vector heads emit quadrics", {
  t <- seq(0, 2 * pi, length.out = 12L)
  helix <- data.frame(x = cos(t), y = sin(t), z = t / 4)
  s <- stats_scene(ggplot2::ggplot(helix, ggplot2::aes(x, y, z = z)) + geom_tube_3d(radius = 0.1) + coord_3d())
  q <- s$packet$quadrics
  expect_identical(sum(q$kind == 2L), 11L)
  expect_identical(sum(q$kind == 1L), 12L)
  flat <- stats_scene(ggplot2::ggplot(helix, ggplot2::aes(x, y, z = z)) + geom_tube_3d(radius = 0.1, cap = "none") + coord_3d())
  q <- flat$packet$quadrics
  expect_identical(sum(q$kind == 1L), 10L)
  expect_identical(sum(q$cap_0 == 0L) + sum(q$cap_1 == 0L), 2L)
  growing <- stats_scene(ggplot2::ggplot(transform(helix, r = seq(0.05, 0.2, length.out = 12L)),
    ggplot2::aes(x, y, z = z, radius = r)) + geom_tube_3d() + coord_3d())
  expect_true(all(growing$packet$quadrics$kind %in% c(1L, 3L)))
  translucent <- stats_scene(ggplot2::ggplot(helix, ggplot2::aes(x, y, z = z)) + geom_tube_3d(radius = 0.1, alpha = 0.5) + coord_3d())
  expect_length(translucent$packet$quadrics$kind, 0L)
  expect_gt(length(translucent$packet$faces$offset), 100L)
  millimetre <- integration_packets(ggplot2::ggplot(helix, ggplot2::aes(x, y, z = z)) +
    geom_tube_3d(radius = 1, radius.unit = "mm") + coord_3d())
  expect_false(is.null(attr(millimetre$grobs[[1L]]$packet, "quadric_mm")))
  d <- data.frame(x = 0, y = 0, z = 0, u = 1, v = 1, w = 0)
  v <- stats_scene(ggplot2::ggplot(d, ggplot2::aes(x, y, z = z, u = u, v = v, w = w)) +
    geom_quiver_3d(shaft = "tube") + coord_3d())
  expect_setequal(v$packet$quadrics$kind, 2:3)
  expect_identical(v$packet$quadrics$radius_ratio[v$packet$quadrics$kind == 3L], 0)
})
