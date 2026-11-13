polygon_area_of <- function(vertices, faces) {
  sum(unlist(lapply(Filter(function(f) nrow(f) > 0L, faces), function(f) apply(f, 1L, function(i) {
    xy <- vertices[i, 1:2, drop = FALSE]
    nxt <- xy[c(seq.int(2L, nrow(xy)), 1L), , drop = FALSE]
    sum(xy[, 1L] * nxt[, 2L] - nxt[, 1L] * xy[, 2L]) / 2
  }))))
}

test_that("winding trapezoids cover the nonzero region of nested and overlapping contours", {
  square <- function(x0, y0, s, clockwise = FALSE) {
    xy <- cbind(c(x0, x0 + s, x0 + s, x0), c(y0, y0, y0 + s, y0 + s))
    if (clockwise) xy[4:1, ] else xy
  }
  holed <- winding_trapezoids_3d(list(square(0, 0, 4), square(1, 1, 2, clockwise = TRUE)))
  expect_equal(polygon_area_of(holed$vertices, holed$faces), 16 - 4)
  overlapping <- winding_trapezoids_3d(list(square(0, 0, 2), square(1, 1, 2)))
  expect_equal(polygon_area_of(overlapping$vertices, overlapping$faces), 7)
  diamond <- winding_trapezoids_3d(list(cbind(c(0, 1, 0, -1), c(-1, 0, 1, 0))))
  expect_equal(polygon_area_of(diamond$vertices, diamond$faces), 2)
  expect_true(all(vapply(diamond$faces, ncol, integer(1)) %in% 3:4))
  for (mesh in list(holed, overlapping, diamond)) {
    for (f in Filter(function(f) nrow(f) > 0L, mesh$faces)) {
      areas <- apply(f, 1L, function(i) polygon_area_of(mesh$vertices, list(matrix(i, 1L))))
      expect_true(all(areas > 0))
    }
  }
})

test_that("text outlines take grid's text box and glyph meshes cover the outlines", {
  skip_if_not_installed("systemfonts")
  pdf(NULL); on.exit(dev.off(), add = TRUE)
  outline <- text_outline_3d("Height 42", 12)
  width <- grid::convertWidth(grid::stringWidth("Height 42"), "mm", TRUE)
  expect_equal(outline$size[1L], width, tolerance = 0.05)
  expect_equal(mean(range(outline$x)), 0, tolerance = 0.05 * width)
  mesh <- text_mesh_3d("O", 12)
  expect_true(all(mesh$vertices[, 3L] == 0))
  area <- polygon_area_of(mesh$vertices, mesh$faces)
  ring <- split(seq_along(text_outline_3d("O", 12)$x), text_outline_3d("O", 12)$id)
  o <- text_outline_3d("O", 12)
  rings <- vapply(ring, function(i) ring_area_3d(cbind(o$x, o$y), i), numeric(1))
  expect_equal(area, abs(sum(rings)), tolerance = 1e-6)
  expect_error(text_outline_3d(quote(alpha), 12), "character labels")
})

test_that("geom_text_3d(orientation = 'plane') lays labels in the plane as scene faces", {
  skip_if_not_installed("systemfonts")
  d <- data.frame(x = c(1, 3), y = c(1, 3), z = c(1, 3), label = c("Ab", "c"))
  scene <- function(plane, ...) {
    stats_scene(ggplot2::ggplot(d, ggplot2::aes(x, y, z = z, label = label)) +
      geom_text_3d(orientation = "plane", plane = plane, colour = "red", size = 5) + coord_3d(...))$packet
  }
  for (plane in c("xy", "xz", "yz")) {
    packet <- scene(plane)
    expect_gt(length(packet$faces$offset), 5L)
    expect_length(packet$text$offset, 0L)
    expect_true(all(packet$styles$fill[packet$faces$style_id] == "#FF0000"))
    fixed <- c(xy = "z", xz = "y", yz = "x")[[plane]]
    expect_true(all(packet$vertices[[fixed]] %in% range(packet$vertices[[fixed]])))
    expect_length(unique(round(packet$vertices[[fixed]], 9)), 2L)
  }
  above <- scene("xy", elevation = 30); below <- scene("xy", elevation = -30)
  first <- function(p) p$vertices$x[p$vertices$z == min(p$vertices$z)]
  centre <- function(p) mean(range(first(p)))
  expect_equal(sort(first(below) - centre(below)), sort(-(first(above) - centre(above))), tolerance = 1e-9)
  expect_error(stats_scene(ggplot2::ggplot(d, ggplot2::aes(x, y, z = z, label = label)) +
    geom_text_3d(orientation = "plane", plane = "zz") + coord_3d()), "plane must be one of")
  expect_error(geom_text_3d(orientation = "page"))
  expect_error(geom_text_3d(plane = "xy"), "Unknown 3D layer arguments")
})

test_that("plane text renders in the vector and raster backends", {
  skip_if_not_installed("systemfonts")
  d <- data.frame(x = 1:3, y = 1:3, z = 1:3, label = c("a", "b", "c"))
  p <- ggplot2::ggplot(d, ggplot2::aes(x, y, z = z, label = label)) +
    geom_text_3d(orientation = "plane", angle = 30, size = 6)
  for (backend in list(backend_3d(), backend_3d("raster", dpi = 72))) {
    path <- tempfile(fileext = ".png")
    ragg::agg_png(path, width = 3, height = 3, units = "in", res = 72)
    expect_no_error(print(p + coord_3d(backend = backend)))
    grDevices::dev.off(); unlink(path)
  }
})
