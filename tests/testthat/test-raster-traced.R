source(testthat::test_path("..", "fixtures", "scenes", "builders.R"), local = TRUE)

traced_ranges <- list(x = c(0, 1), y = c(0, 1), z = c(0, 1))
traced_viewport <- ggplot3d:::new_scene_viewport(25.4, 25.4, 31)
traced_camera <- ggplot3d:::new_camera_3d(azimuth = -90, elevation = 90)

traced_sphere <- function(radius = .2) {
  list(kind = "sdf", tape = ggplot3d:::sdf_compile_3d(sdf_sphere(c(.5, .5, .5), radius)),
    ranges = traced_ranges, fill = "blue", tolerance = 1e-8, max_steps = 512L, layer_id = 7L)
}

traced_volume <- function(sigma) {
  axes <- list(x = c(0, 1), y = c(0, 1), z = c(0, 1))
  grid <- expand.grid(axes); grid$value <- 1
  field <- ggplot3d:::stats_scalar_grid_3d(grid)
  c(list(kind = "volume", mode = 0L, samples = 16L, level = 0, ranges = traced_ranges, layer_id = 3L), field,
    ggplot3d:::volume_transfer_3d(field, sigma, "white"))
}

traced_pixel <- function(result, row, col) {
  packed <- as.double(result$image[(row - 1L) * result$width + col])
  if (packed < 0) packed <- packed + 2^32
  (packed %/% 256^(0:3)) %% 256 / 255
}

test_that("a vector stroke through a traced sphere is hidden behind it only", {
  empty <- ggplot3d:::new_scene_packet()
  result <- ggplot3d:::raster_scene_3d(empty, traced_camera, traced_viewport, 1L, buffers = TRUE,
    traced = list(traced_sphere(.3)))
  depth <- result$buffers$opaque_depth
  expect_true(any(is.finite(depth)))
  expect_identical(result$buffers$coverage_depth, depth)
  line <- function(z) {
    p <- ref_packet(lines = list(rbind(c(.1, .5, z[1L]), c(.9, .5, z[2L]))), line_width = .5)
    p$styles$colour <- "red"
    ggplot3d:::raster_occlusion_3d(p, traced_camera, traced_viewport, result)$status$polylines
  }
  expect_identical(line(c(.5, .5)), 2L)
  expect_identical(line(c(.95, .95)), 0L)
  expect_identical(line(c(.05, .05)), 2L)
  far <- ref_packet(lines = list(rbind(c(.45, .5, .05), c(.55, .5, .05))), line_width = .5)
  far$styles$colour <- "red"
  expect_identical(ggplot3d:::raster_occlusion_3d(far, traced_camera, traced_viewport, result)$status$polylines, 1L)
})

test_that("vector and hybrid panels place traced layers as images and declare them", {
  skip_if_not_installed("ragg")
  path <- data.frame(x = c(-1, 1), y = c(0, 0), z = c(-1, 1))
  plot <- function(type) ggplot2::ggplot() +
    geom_isofunction_3d(fun = sdf_sphere(radius = .6), render = "trace", fill = "steelblue") +
    geom_path_3d(data = path, ggplot2::aes(x, y, z = z), colour = "red") +
    ggplot2::xlim(-1, 1) + ggplot2::ylim(-1, 1) + scale_z_continuous(limits = c(-1, 1)) +
    coord_3d(backend = backend_3d(type, dpi = 60))
  find <- function(g) {
    if (identical(g$name, "scene-hybrid-3d")) return(list(g))
    unlist(lapply(c(as.list(g$children), g$grobs), find), recursive = FALSE)
  }
  for (type in c("vector", "hybrid")) {
    file <- tempfile(fileext = ".png")
    ragg::agg_png(file, 300, 300, res = 72)
    grob <- grid::grid.force(ggplot2::ggplotGrob(plot(type)))
    grDevices::dev.off(); unlink(file)
    panel <- find(grob)[[1L]]
    expect_true(panel$capabilities$raster)
    expect_identical(panel$capabilities$image_layers, 1L)
    expect_identical(utils::tail(panel$occlusion$status$polylines, 1L), 2L)
    expect_identical(panel$raster_result$traced$layer_id, 1L)
  }
})

test_that("a translucent raster quad inside a volume composites at its depth", {
  sigma <- 2; alpha <- .5
  expected <- function(z) {
    a1 <- 1 - exp(-sigma * (1 - z)); a2 <- 1 - exp(-sigma * z)
    q <- ggplot3d:::srgb_decode_3d(c(1, 0, 0))
    colour <- a1; a <- a1
    colour <- colour + (1 - a) * alpha * q; a <- a + (1 - a) * alpha
    colour <- colour + (1 - a) * a2; a <- a + (1 - a) * a2
    c(ggplot3d:::srgb_encode_3d(colour / a), a)
  }
  for (z in c(.2, .5, .8)) {
    packet <- ref_packet(faces = list(ref_quad(z)), face_alpha = alpha)
    packet$styles$fill <- "red"
    result <- ggplot3d:::raster_scene_3d(packet, traced_camera, traced_viewport, 1L, buffers = TRUE,
      traced = list(traced_volume(sigma)))
    expect_equal(traced_pixel(result, 16L, 16L), expected(z), tolerance = 1.5 / 255)
    expect_lt(result$buffers$depth[16, 16], result$buffers$coverage_depth[16, 16])
  }
  front <- traced_pixel(ggplot3d:::raster_scene_3d({
    p <- ref_packet(faces = list(ref_quad(.8)), face_alpha = alpha); p$styles$fill <- "red"; p
  }, traced_camera, traced_viewport, 1L, traced = list(traced_volume(sigma))), 16L, 16L)
  back <- traced_pixel(ggplot3d:::raster_scene_3d({
    p <- ref_packet(faces = list(ref_quad(.2)), face_alpha = alpha); p$styles$fill <- "red"; p
  }, traced_camera, traced_viewport, 1L, traced = list(traced_volume(sigma))), 16L, 16L)
  expect_gt(front[1L] - front[3L], back[1L] - back[3L])
})

test_that("coverage depth of a homogeneous volume is its half-alpha depth", {
  sigma <- 3
  result <- ggplot3d:::raster_scene_3d(ggplot3d:::new_scene_packet(), traced_camera, traced_viewport, 1L,
    buffers = TRUE, traced = list(traced_volume(sigma)))
  total <- 1 - exp(-sigma)
  half <- -log(1 - total / 2) / sigma
  plane <- function(z) {
    packet <- ref_packet(faces = list(ref_quad(z)))
    packet$styles$fill <- "red"
    ggplot3d:::raster_scene_3d(packet, traced_camera, traced_viewport, 1L, buffers = TRUE)$buffers$opaque_depth[16, 16]
  }
  top <- plane(1); bottom <- plane(0)
  expect_equal(result$buffers$depth[16, 16], top, tolerance = 1e-9)
  expect_equal((result$buffers$coverage_depth[16, 16] - top) / (bottom - top), half, tolerance = 1e-6)
})

test_that("traced primitive ids extend the raster ids and agree with the item buffers", {
  packet <- ref_packet(faces = list(ref_quad(.1, x = c(0, 1, 1, 0), y = c(0, 0, 1, 1))))
  packet$styles$fill <- "red"
  plain <- ggplot3d:::raster_scene_3d(packet, traced_camera, traced_viewport, 1L, buffers = TRUE)
  traced <- ggplot3d:::raster_scene_3d(packet, traced_camera, traced_viewport, 1L, buffers = TRUE,
    traced = list(traced_sphere()))
  expect_identical(traced$traced$primitive_id, 2L)
  expect_identical(traced$traced$layer_id, 7L)
  sphere <- traced$buffers$kind == 6L
  expect_true(any(sphere) && any(!sphere))
  expect_true(all(traced$id[sphere] == 2L))
  expect_true(all(traced$buffers$row[sphere] == 1L))
  expect_identical(traced$id[!sphere], plain$id[!sphere])
  expect_identical(traced$buffers$kind[!sphere], plain$buffers$kind[!sphere])
  expect_equal(traced$buffers$normal$z[16, 16], 1, tolerance = 1e-6)
  expect_identical(plain$buffers$coverage_depth, plain$buffers$opaque_depth)
})

test_that("traced and face depths share the camera coordinate under every projection", {
  for (projection in c("orthographic", "perspective", "oblique")) {
    camera <- ggplot3d:::new_camera_3d(projection = projection, azimuth = -90, elevation = 90)
    draw <- function(z) {
      packet <- ref_packet(faces = list(ref_quad(z)))
      packet$styles$fill <- "red"
      ggplot3d:::raster_scene_3d(packet, camera, traced_viewport, 1L, buffers = TRUE, traced = list(traced_sphere()))
    }
    front <- draw(.9); behind <- draw(.1)
    expect_lt(front$buffers$depth[16, 16], behind$buffers$depth[16, 16])
    expect_identical(c(front$buffers$kind[16, 16], behind$buffers$kind[16, 16]), c(1L, 6L))
    expect_equal(traced_pixel(behind, 16L, 16L), c(0, 0, 1, 1))
    expect_equal(traced_pixel(front, 16L, 16L), c(1, 0, 0, 1))
  }
})
