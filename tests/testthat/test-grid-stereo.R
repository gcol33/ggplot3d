stereo_plot <- function(stereo, ...) {
  d <- data.frame(x = c(0, 1, 0.5), y = c(0, 0, 1), z = c(0, 1, 0.5))
  ggplot2::ggplot(d, ggplot2::aes(x, y, z = z)) + geom_point_3d(size = 2) +
    coord_3d(stereo = stereo, ...)
}

stereo_drawn <- function(plot, device = function(path) grDevices::pdf(path, width = 8, height = 4)) {
  path <- tempfile()
  device(path)
  on.exit({ grDevices::dev.off(); unlink(path) }, add = TRUE)
  forced <- grid::grid.force(ggplot2::ggplotGrob(plot))
  grid::grid.draw(forced)
  motion_panels_3d(forced)
}

find_grobs <- function(grob, test) {
  found <- if (test(grob)) list(grob) else list()
  children <- if (inherits(grob, "gTree")) as.list(grob$children) else list()
  if (inherits(grob, "GridGroup")) children <- c(children, list(grob$src, grob$dst))
  c(found, unlist(lapply(children, find_grobs, test), recursive = FALSE))
}

basis_of <- function(camera) {
  b <- frame_basis_3d(camera, new_scene_viewport(100, 100))
  list(right = b$basis[, 1L], up = b$basis[, 2L], forward = b$forward)
}

test_that("stereo_3d() validates its arguments", {
  s <- stereo_3d("anaglyph", separation = 4, colours = c("red", "cyan"))
  expect_s3_class(s, "ggplot3d_stereo")
  expect_equal(unname(s$filters[, "left"]), c(1, 0, 0))
  expect_equal(unname(s$filters[, "right"]), c(0, 1, 1))
  expect_output(print(s), "anaglyph, separation 4 degrees")
  expect_error(stereo_3d(separation = 0), "separation")
  expect_error(stereo_3d(separation = c(1, 2)), "separation")
  expect_error(stereo_3d(colours = "red"), "two filter colours")
  expect_error(stereo_3d(colours = c("red", "notacolour")), "valid colours")
  expect_error(coord_3d(stereo = "pair"), "stereo_3d")
})

test_that("eye cameras turn about the image's vertical axis", {
  for (args in list(list(-60, 30, 0), list(20, 75, 15), list(-90, 90, 0), list(10, -40, -30))) {
    camera <- new_camera_3d(azimuth = args[[1]], elevation = args[[2]], roll = args[[3]])
    base <- basis_of(camera)
    for (offset in c(-2.5, 4)) {
      eye <- basis_of(stereo_camera_3d(camera, offset))
      theta <- offset * pi / 180
      expect_equal(eye$up, base$up, tolerance = 1e-10)
      expect_equal(eye$forward, cos(theta) * base$forward - sin(theta) * base$right, tolerance = 1e-10)
      expect_equal(eye$right, cos(theta) * base$right + sin(theta) * base$forward, tolerance = 1e-10)
    }
  }
  level <- new_camera_3d(azimuth = -60, elevation = 0, roll = 0)
  expect_equal(stereo_camera_3d(level, 3)$azimuth, -57, tolerance = 1e-10)
  expect_equal(stereo_camera_3d(level, 3)$elevation, 0, tolerance = 1e-10)
})

test_that("near geometry has crossed disparity and orthographic disparity is horizontal", {
  camera <- new_camera_3d(azimuth = -60, elevation = 30)
  offsets <- stereo_offsets_3d(stereo_3d(separation = 6))
  basis <- frame_basis_3d(camera, new_scene_viewport(100, 100))
  near <- rbind(c(0.5, 0.5, 0.5) - 0.3 * basis$forward, c(0.5, 0.5, 0.5) + 0.3 * basis$forward)
  project <- function(offset) {
    eye <- stereo_camera_3d(camera, offset)
    frame_positions_3d(near, eye, frame_basis_3d(eye, new_scene_viewport(100, 100)))
  }
  left <- project(offsets[["left"]]); right <- project(offsets[["right"]])
  expect_gt(left[1L, 1L], right[1L, 1L])
  expect_lt(left[2L, 1L], right[2L, 1L])
  expect_equal(left[, 2L], right[, 2L], tolerance = 1e-10)
})

test_that("pair and cross modes place the eye images side by side in a 2:1 panel", {
  coord <- coord_3d(stereo = stereo_3d("pair"))
  expect_equal(coord$aspect(list()), 0.5)
  expect_equal(coord_3d(stereo = stereo_3d("anaglyph"))$aspect(list()), 1)
  expect_equal(coord_3d()$aspect(list()), 1)
  for (mode in c("pair", "cross")) {
    panel <- stereo_drawn(stereo_plot(stereo_3d(mode, separation = 6)))[[1L]]
    eyes <- as.list(panel$children)
    expect_length(eyes, 2L)
    expect_equal(unname(vapply(eyes, `[[`, character(1), "eye")),
                 if (mode == "pair") c("left", "right") else c("right", "left"))
    expect_equal(unname(vapply(eyes, function(e) grid::convertX(e$vp$x, "npc", TRUE), numeric(1))), c(0, 0.5))
    azimuths <- vapply(eyes, function(e) e$panel_params$camera$azimuth, numeric(1))
    expected <- vapply(c(-3, 3), function(o) stereo_camera_3d(new_camera_3d(), o)$azimuth, numeric(1))
    expect_equal(unname(sort(azimuths)), expected)
    expect_true(all(vapply(eyes, function(e) !is.null(e$scene), logical(1))))
  }
})

test_that("both eyes share one magnification", {
  panel <- stereo_drawn(stereo_plot(stereo_3d("pair", separation = 20)))[[1L]]
  eyes <- as.list(panel$children)
  sizes <- lapply(eyes, function(e) unlist(e$inner_viewport[c("width_mm", "height_mm")]))
  expect_equal(sizes[[1L]], sizes[[2L]])
  expect_length(eyes[[1L]]$eye_group, 2L)
})

test_that("stereo pairs work per facet and per view", {
  d <- data.frame(x = c(0, 1, 0.5, 0.2), y = c(0, 0, 1, 1), z = c(0, 1, 0.5, 0.2), g = c("a", "a", "b", "b"))
  p <- ggplot2::ggplot(d, ggplot2::aes(x, y, z = z)) + geom_point_3d() +
    ggplot2::facet_wrap(~g) + coord_3d(stereo = stereo_3d("cross"))
  panels <- stereo_drawn(p)
  expect_length(panels, 2L)
  expect_true(all(vapply(panels, function(q) length(q$children) == 2L, logical(1))))
  expect_length(as.list(panels[[1L]]$children)[[1L]]$eye_group, 4L)
  v <- stereo_drawn(stereo_plot(stereo_3d("pair")) + facet_views(c("front", "isometric")))
  expect_length(v, 2L)
  iso <- camera_3d("isometric")
  expected <- c(stereo_camera_3d(new_camera_3d(azimuth = -90, elevation = 0), -1.5)$azimuth,
                stereo_camera_3d(new_camera_3d(azimuth = iso$azimuth, elevation = iso$elevation), -1.5)$azimuth)
  expect_equal(unname(vapply(v, function(q) as.list(q$children)[[1L]]$panel_params$camera$azimuth, numeric(1))),
               expected, tolerance = 1e-8)
  expect_equal(expected[1L], -91.5)
})

test_that("ink colours keep their grey level through each filter", {
  filters <- stereo_3d("anaglyph")$filters
  expect_equal(ink_colours_3d(c("white", "black", NA, "transparent"), filters[, "left"]),
               c("#FFFFFFFF", "#00FFFFFF", NA, "transparent"))
  expect_equal(ink_colours_3d("black", filters[, "right"]), "#FF0000FF")
  grey <- ink_colours_3d("#80808080", filters[, "left"])
  expect_equal(unname(grDevices::col2rgb(grey, alpha = TRUE)[, 1L]), c(128, 255, 255, 128))
  both <- grDevices::col2rgb(c(ink_colours_3d("grey40", filters[, "left"]), ink_colours_3d("grey40", filters[, "right"]))) / 255
  expect_equal(unname(both[, 1L] * both[, 2L]), unname(grDevices::col2rgb("grey40")[, 1L] / 255), tolerance = 1e-8)
  red <- grDevices::col2rgb(ink_colours_3d("red", filters[, "left"]))[1L, 1L] / 255
  expect_equal(unname(red), grey_level_3d(matrix(c(1, 0, 0))), tolerance = 1 / 255)
  native <- structure(matrix(c(-1L, -16777216L), 1L), class = "nativeRaster")
  inked <- ink_native_raster_3d(native, filters[, "left"])
  expect_s3_class(inked, "nativeRaster")
  expect_equal(as.integer(inked), c(-1L, bitwOr(-16777216L, 0xFFFF00L)))
})

test_that("ink_grob_3d() recolours strokes, fills, text and children", {
  filter <- c(1, 0, 0)
  tree <- grid::gTree(children = grid::gList(
    grid::rectGrob(gp = grid::gpar(fill = "black", col = "white"), name = "r"),
    grid::textGrob("a", gp = grid::gpar(col = "black"), name = "t"),
    grid::rasterGrob(matrix(c("black", "white"), 1L), name = "i")))
  inked <- ink_grob_3d(tree, filter)
  expect_equal(inked$children$r$gp$fill, "#00FFFFFF")
  expect_equal(inked$children$r$gp$col, "#FFFFFFFF")
  expect_equal(inked$children$t$gp$col, "#00FFFFFF")
  expect_equal(as.vector(inked$children$i$raster), c("#00FFFFFF", "#FFFFFFFF"))
})

test_that("anaglyphs blend two monochrome passes where the device can multiply", {
  skip_if(getRversion() < "4.2.0")
  skip_if_not_installed("ragg")
  panel <- stereo_drawn(stereo_plot(stereo_3d("anaglyph", separation = 6)),
    function(path) ragg::agg_png(path, 600, 600, res = 96))[[1L]]
  group <- as.list(panel$children)[[1L]]
  expect_s3_class(group, "GridGroup")
  expect_equal(group$op, "multiply")
  expect_s3_class(group$src, "GridGroup")
  expect_equal(group$src$src$eye, "right")
  expect_equal(group$dst$eye, "left")
  expect_equal(group$dst$ink, stereo_3d()$filters[, "left"])
})

test_that("anaglyphs composite per channel without device blending and with the raster backend", {
  skip_if_not_installed("ragg")
  for (case in list(list(device = function(path) grDevices::pdf(path, 6, 6), backend = backend_3d("raster", dpi = 40)),
                    list(device = function(path) grDevices::postscript(path, width = 6, height = 6), backend = backend_3d("vector", dpi = 40)))) {
    p <- stereo_plot(stereo_3d("anaglyph", separation = 6), backend = case$backend)
    panel <- stereo_drawn(p, case$device)[[1L]]
    image <- as.list(panel$children)[[1L]]
    expect_s3_class(image, "rastergrob")
    expect_equal(dim(image$raster), c(6, 6) * 40, tolerance = 0.05)
    rgb <- grDevices::col2rgb(as.vector(image$raster), alpha = TRUE)
    expect_true(any(rgb["red", ] != rgb["green", ]))
    expect_true(all(abs(rgb["green", ] - rgb["blue", ]) <= 1))
  }
})

test_that("raster composite of identical eyes reproduces the grey level", {
  skip_if_not_installed("ragg")
  grDevices::pdf(NULL, width = 2, height = 2)
  on.exit(grDevices::dev.off(), add = TRUE)
  flat <- grid::rectGrob(gp = grid::gpar(fill = "grey40", col = NA))
  image <- anaglyph_raster_3d(list(left = flat, right = flat), stereo_3d("anaglyph")$filters, 20)$raster
  rgb <- grDevices::col2rgb(as.vector(image), alpha = TRUE)
  expect_true(all(abs(rgb[1:3, ] - 102) <= 1))
  expect_true(all(rgb[4L, ] == 255))
})

test_that("anaglyph mode warns that colour scales show luminance", {
  p <- ggplot2::ggplot(mtcars, ggplot2::aes(wt, mpg, z = hp, colour = cyl)) + geom_point_3d()
  expect_warning(ggplot2::ggplot_build(p + coord_3d(stereo = stereo_3d("anaglyph"))), "luminance")
  expect_no_warning(ggplot2::ggplot_build(p + coord_3d(stereo = stereo_3d("pair"))))
  expect_no_warning(ggplot2::ggplot_build(stereo_plot(stereo_3d("anaglyph"))))
})

test_that("view_3d() and mono_plot_3d() use the central camera", {
  p <- stereo_plot(stereo_3d("pair"))
  mono <- mono_plot_3d(p)
  expect_null(mono$coordinates$config$stereo)
  expect_s3_class(p$coordinates$config$stereo, "ggplot3d_stereo")
  skip_if_not_installed("htmlwidgets")
  skip_if_not_installed("ragg")
  widget <- view_3d(p)
  expect_equal(widget$x$camera$azimuth, -60)
})

test_that("animate_3d() draws stereo frames", {
  skip_if_not_installed("ragg")
  result <- animate_3d(stereo_plot(stereo_3d("pair")), azimuth = c(-60, -30), frames = 2L,
    width = 640L, height = 320L, renderer = "vector")
  on.exit(unlink(result$frames), add = TRUE)
  expect_equal(result$frame_count, 2L)
  expect_true(all(file.exists(result$frames)))
})
