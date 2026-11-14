stable_soup <- function(n, seed = 1) {
  set.seed(seed)
  polygons <- lapply(seq_len(n), function(i) sweep(matrix(stats::rnorm(9, sd = .12), 3), 2, stats::runif(3, .2, .8), "+"))
  xyz <- do.call(rbind, polygons)
  ggplot3d:::new_scene_packet(
    vertices = list(x = xyz[, 1], y = xyz[, 2], z = xyz[, 3]),
    corners = list(vertex = seq_len(nrow(xyz))),
    faces = list(offset = seq.int(1L, by = 3L, length.out = n), count = rep(3L, n),
                 layer_id = seq_len(n), opacity = rep(.6, n)),
    styles = data.frame(style_id = 1L))
}

stable_compile <- function(packet, azimuth, stable, elevation = 25, projection = "orthographic") {
  ggplot3d:::scene_compile(packet,
    ggplot3d:::new_camera_3d(azimuth = azimuth, elevation = elevation, projection = projection),
    ggplot3d:::new_scene_viewport(100, 100), stable_partition = stable)
}

# Primitive painted last over each of n x n sample points, drawing the face
# fragments in draw order with an even-odd fill.
stable_paint <- function(scene, n = 61L) {
  g <- (seq_len(n) - 0.5) / n + 1e-4 * pi
  q <- as.matrix(expand.grid(g, g))
  top <- integer(nrow(q))
  f <- scene$fragments; v <- scene$vertices
  for (i in scene$draw_order) {
    if (f$kind[i] != 1L) next
    idx <- seq.int(f$offset[i], length.out = f$count[i])
    x <- v$x_npc[idx]; y <- v$y_npc[idx]; j <- c(length(x), seq_len(length(x) - 1L))
    inside <- logical(nrow(q))
    for (k in seq_along(x)) {
      cross <- (y[k] > q[, 2]) != (y[j[k]] > q[, 2])
      inside <- xor(inside, cross & q[, 1] < (x[j[k]] - x[k]) * (q[, 2] - y[k]) / (y[j[k]] - y[k]) + x[k])
    }
    top[inside] <- f$parent_primitive_id[i]
  }
  top
}

# Fragments as primitive and box-coordinate vertex sets.
stable_fragments <- function(scene) {
  f <- scene$fragments; v <- scene$vertices
  vapply(seq_along(f$offset), function(i) {
    idx <- seq.int(f$offset[i], length.out = f$count[i])
    paste(f$parent_primitive_id[i], paste(sort(sprintf("%.6f,%.6f,%.6f", v$x[idx], v$y[idx], v$z[idx])), collapse = ";"))
  }, "")
}

stable_overlap <- function(a, b) {
  a <- stable_fragments(a); b <- stable_fragments(b)
  length(intersect(a, b)) / length(union(a, b))
}

test_that("a stable partition paints what a cold compile paints", {
  withr::local_options(ggplot3d.cache_bytes = 0)
  packet <- stable_soup(60L)
  for (azimuth in c(-60, 20, 110)) {
    cold <- stable_compile(packet, azimuth, FALSE)
    stable <- stable_compile(packet, azimuth, TRUE)
    expect_gt(length(stable$draw_order), length(packet$faces$count))
    expect_setequal(stable$fragments$parent_primitive_id, cold$fragments$parent_primitive_id)
    expect_identical(stable_paint(stable), stable_paint(cold))
  }
})

test_that("stable cuts stay in place while the camera turns", {
  withr::local_options(ggplot3d.cache_bytes = 0)
  packet <- stable_soup(200L)
  frames <- function(stable) lapply(seq(0, 60, by = 15), function(azimuth) stable_compile(packet, azimuth, stable))
  overlap <- function(scenes) mapply(stable_overlap, scenes[-length(scenes)], scenes[-1L])
  cold <- overlap(frames(FALSE))
  stable <- overlap(frames(TRUE))
  expect_true(all(stable > 0.95))
  expect_lt(max(cold), 0.3)
})

test_that("intersecting faces are cut once on their own planes", {
  withr::local_options(ggplot3d.cache_bytes = 0)
  quad <- function(z) cbind(c(0, 1, 1, 0), c(0, 0, 1, 1), z)
  xyz <- rbind(quad(c(.2, .8, .8, .2)), quad(c(.8, .2, .2, .8)))
  packet <- ggplot3d:::new_scene_packet(vertices = list(x = xyz[, 1], y = xyz[, 2], z = xyz[, 3]),
    corners = list(vertex = 1:8), faces = list(offset = c(1L, 5L), count = c(4L, 4L), opacity = c(.4, .4)),
    styles = data.frame(style_id = 1L))
  scenes <- lapply(c(0, 90, 200), function(azimuth) stable_compile(packet, azimuth, TRUE))
  for (scene in scenes) expect_length(scene$draw_order, 3L)
  expect_identical(sort(stable_fragments(scenes[[1L]])), sort(stable_fragments(scenes[[3L]])))
})

test_that("the stable partition does not depend on the thread count", {
  withr::local_options(ggplot3d.cache_bytes = 0, ggplot3d.threads = 1L)
  packet <- stable_soup(80L)
  serial <- stable_compile(packet, 35, TRUE)
  withr::local_options(ggplot3d.threads = 3L)
  expect_identical(stable_compile(packet, 35, TRUE), serial)
})

test_that("compiled scenes are cached apart for stable and cold partitions", {
  withr::local_options(ggplot3d.cache_bytes = 256 * 2^20)
  packet <- stable_soup(40L, seed = 2)
  cold <- stable_compile(packet, 30, FALSE)
  stable <- stable_compile(packet, 30, TRUE)
  testthat::local_mocked_bindings(scene_compile_cpp = function(...) stop("engine called"), .package = "ggplot3d")
  expect_identical(stable_compile(packet, 30, TRUE), stable)
  expect_identical(stable_compile(packet, 30, FALSE), cold)
})

test_that("the engine rejects an invalid stable partition flag", {
  packet <- stable_soup(4L)
  view <- ggplot3d:::new_scene_viewport(100, 100)
  attr(view, "engine_stable_partition") <- NA
  expect_error(ggplot3d:::scene_compile_cpp(packet[!names(packet) %in% c("styles", "materials")],
    ggplot3d:::new_camera_3d(), view), "TRUE or FALSE")
})

test_that("quadric stand-ins keep one refinement through an orbit", {
  withr::local_options(ggplot3d.cache_bytes = 0)
  cylinder <- c(list(kind = 2L),
    ggplot3d:::quadric_transforms_3d(array(diag(c(.1, .1, .5)), c(3L, 3L, 1L)), matrix(c(.75, .5, .25), 1L)))
  packet <- ggplot3d:::new_scene_packet(quadrics = cylinder,
    styles = data.frame(style_id = 1L, fill = "red", shade = FALSE))
  angles <- function(stable) {
    lapply(seq(0, 270, by = 45), function(azimuth) {
      scene <- stable_compile(packet, azimuth, stable, elevation = 10, projection = "perspective")
      f <- scene$fragments; v <- scene$vertices
      idx <- unlist(lapply(which(f$kind == 5L), function(i) seq.int(f$offset[i], length.out = f$count[i])))
      round(atan2(v$y[idx] - .5, v$x[idx] - .75) %% (2 * pi), 6)
    })
  }
  # Stand-ins of two refinements would leave vertices of both angle grids,
  # which never share an angle; one refinement leaves exactly its own grid.
  around <- unique(unlist(angles(TRUE)))
  expect_true(length(around) %in% c(8L, 12L, 16L, 24L, 32L, 48L, 64L, 96L, 128L))
  step <- 2 * pi / length(around)
  offset <- (around / step - 0.5) %% 1
  expect_lt(max(pmin(offset, 1 - offset)), 1e-4)
})

test_that("animate_3d() passes stable_partition to the vector renderer", {
  skip_if_not_installed("ragg")
  p <- ggplot2::ggplot(data.frame(x = c(0, 1, 0), y = c(0, 0, 1), z = c(0, 1, 1)), ggplot2::aes(x, y, z = z)) +
    geom_polygon_3d(fill = "steelblue", alpha = 0.6) + coord_3d()
  calls <- list()
  compile <- ggplot3d:::scene_compile
  testthat::local_mocked_bindings(scene_compile = function(packet, camera, viewport, stable_partition = FALSE) {
    calls[[length(calls) + 1L]] <<- stable_partition
    compile(packet, camera, viewport, stable_partition)
  }, .package = "ggplot3d")
  movie <- animate_3d(p, frames = 2, renderer = "vector", width = 320, height = 320,
                      progress = FALSE, stable_partition = TRUE)
  unlink(movie$frames)
  expect_true(any(unlist(calls)))
  expect_error(animate_3d(p, frames = 2, stable_partition = NA), "TRUE or FALSE")
})
