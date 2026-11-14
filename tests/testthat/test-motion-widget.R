test_that("viewer serializes one mapped model scene with furniture and camera", {
  skip_if_not_installed("htmlwidgets")
  d <- data.frame(x = c(0, 1, 0), y = c(0, 0, 1), z = c(0, .4, 1))
  p <- ggplot2::ggplot(d, ggplot2::aes(x, y, z = z)) +
    geom_polygon_3d(fill = "#2878A0") +
    geom_point_3d(ggplot2::aes(colour = z), size = 3) +
    geom_path_3d(colour = "#B5342B") +
    geom_text_3d(ggplot2::aes(label = c("a", "b", "c"))) +
    coord_3d(projection = "perspective", azimuth = -35, elevation = 25,
      roll = 12, aspect = c(1, 2, 1))
  w <- view_3d(p, width = 360, height = 360, panel = 1, elementId = "motion-test")
  expect_s3_class(w, "htmlwidget")
  expect_s3_class(w, "ggplot3d_view")
  expect_identical(w$elementId, "motion-test")
  expect_identical(w$x$schema_version, 3L)
  expect_null(w$x$chrome)
  expect_length(w$x$panels, 1L)
  panel <- w$x$panels[[1L]]
  expect_equal(panel$camera$azimuth, -35)
  expect_equal(panel$camera$elevation, 25)
  expect_equal(panel$camera$roll, 12)
  expect_identical(panel$camera$projection, "perspective")
  expect_equal(panel$camera$aspect, c(1, 2, 1))
  expect_equal(panel$rect_mm[3:4], c(w$x$frame$width_mm, w$x$frame$height_mm))
  schemas <- ggplot3d:::scene_schemas()
  for (name in c("scene", "furniture")) {
    scene <- panel[[name]]
    expect_identical(scene$schema_version, 1L)
    expect_length(scene$materials, scene$style_count)
    expect_null(scene$styles)
    for (table in names(schemas)) {
      expect_identical(names(scene[[table]]), names(schemas[[table]]))
      types <- vapply(scene[[table]], `[[`, character(1), "type")
      expect_identical(unname(types), unname(c(double = "f32", integer = "i32", logical = "u8")[schemas[[table]]]))
    }
  }
  expect_gt(length(decode_column(panel$scene$faces$offset)), 0)
  expect_gt(length(decode_column(panel$scene$polylines$offset)), 0)
  expect_gt(length(decode_column(panel$scene$markers$offset)), 0)
  expect_gt(length(decode_column(panel$scene$text$offset)), 0)
  expect_true(all(vapply(panel$scene$materials, function(s) length(s$colour) == 4, logical(1))))
  expect_true(any(vapply(panel$scene$materials, function(s)
    identical(s$fill, ggplot3d:::view_rgba_3d("#2878A0")), logical(1))))
  expect_length(panel$axes, 3L)
  expect_identical(w$x$capabilities$opaque_visibility, "depth-buffer")
  expect_identical(w$x$capabilities$transparency, "primitive-centroid")
  expect_identical(w$x$capabilities$picking, "id-buffer")
  expect_true(all(is.finite(unlist(panel$viewport))))
  path <- tempfile(fileext = ".html")
  on.exit(unlink(c(path, sub(".html$", "_files", path)), recursive = TRUE), add = TRUE)
  htmlwidgets::saveWidget(w, path, selfcontained = FALSE)
  expect_true(file.exists(path))
  expect_match(paste(readLines(path), collapse = "\n"), "ggplot3d_view.js", fixed = TRUE)
})

test_that("binary columns round-trip the merged scene packet", {
  skip_if_not_installed("htmlwidgets")
  set.seed(3)
  d <- data.frame(x = runif(7), y = runif(7), z = c(-1e6, 0.1, 1 / 3, 2, 3, 4, 1e6))
  p <- ggplot2::ggplot(d, ggplot2::aes(x, y, z = z)) + geom_path_3d() + geom_point_3d() + coord_3d()
  w <- view_3d(p, width = 300, height = 300, panel = 1)
  packet <- ggplot3d:::merge_scene_packets(ggplot3d:::motion_panels_3d(ggplot2::ggplotGrob(p))[[1L]]$packets, 1L)
  scene <- w$x$panels[[1L]]$scene
  for (table in c("vertices", "corners", "polylines", "markers")) {
    for (column in names(packet[[table]])) {
      value <- packet[[table]][[column]]
      decoded <- decode_column(scene[[table]][[column]])
      if (is.double(value)) expect_equal(decoded, value, tolerance = 1e-6, info = paste(table, column))
      else expect_identical(decoded, if (is.logical(value)) value else as.integer(value), info = paste(table, column))
    }
  }
  expect_identical(ggplot3d:::base64_3d(charToRaw("Man")), "TWFu")
  expect_identical(ggplot3d:::base64_3d(charToRaw("Ma")), "TWE=")
  expect_identical(ggplot3d:::base64_3d(charToRaw("M")), "TQ==")
  expect_identical(ggplot3d:::base64_3d(raw()), "")
})

test_that("viewer selects a facet and validates its arguments", {
  skip_if_not_installed("htmlwidgets")
  p <- ggplot2::ggplot(mtcars, ggplot2::aes(wt, mpg, z = hp)) +
    geom_point_3d() + coord_3d() + ggplot2::facet_wrap(~am)
  w <- view_3d(p, panel = 2, width = 480, height = 320)
  expect_length(w$x$panels, 1L)
  expect_identical(w$x$panels[[1L]]$id, 2L)
  expect_error(view_3d(p, panel = 3), "outside")
  expect_error(view_3d(p, panel = 0), "positive integer")
  expect_error(view_3d(p, link = NA), "link")
  expect_error(view_3d(p, tooltip = "yes"), "tooltip")
  expect_error(view_3d(p, selection = "some"), "should be one of")
  expect_error(view_3d(p, data = mtcars), "SharedData")
})

test_that("selected panels reflow their aspect and furniture at the widget size", {
  skip_if_not_installed("htmlwidgets")
  skip_if_not_installed("svglite")
  p <- ggplot2::ggplot(mtcars, ggplot2::aes(wt, mpg, z = hp)) +
    geom_point_3d(size = 4) + coord_3d() + ggplot2::theme_minimal(base_size = 17) +
    ggplot2::theme(plot.margin = ggplot2::margin(0, 0, 0, 0))
  for (aspect in c(1, 0.5)) {
    plot <- p + ggplot2::theme(aspect.ratio = aspect)
    for (size in list(c(624, 384), c(384, 624))) {
      complete <- view_3d(plot, width = size[1], height = size[2])$x$panels[[1L]]
      stacked <- plot + ggplot2::facet_wrap(~am, ncol = 1)
      selected <- view_3d(stacked, panel = 2, width = size[1], height = size[2])
      panel <- selected$x$panels[[1L]]
      expect_null(selected$x$chrome)
      expect_identical(panel$id, 2L)
      expect_equal(panel$viewport, complete$viewport)
      expect_equal(panel$framing, complete$framing)
      expect_equal(panel$axes, complete$axes)
      expect_equal(panel$reference, complete$reference)
      expect_equal(panel$rect_mm[1:2], c(0, 0))
      expect_equal(panel$rect_mm[3:4], complete$rect_mm[3:4])
      expect_equal(panel$rect_mm[4] / panel$rect_mm[3], aspect)
      expect_equal(max(panel$rect_mm[3:4] / (size / 96 * 25.4)), 1)
      expect_setequal(decode_column(panel$scene$markers$source_row), which(mtcars$am == 1))
    }
  }
  views <- p + facet_views(c("front", "isometric"), ncol = 1)
  selected <- view_3d(views, panel = 2, width = 624, height = 384)$x$panels[[1L]]
  complete <- view_3d(p + do.call(coord_3d, camera_3d("isometric")),
    width = 624, height = 384)$x$panels[[1L]]
  expect_equal(selected$viewport, complete$viewport)
  expect_equal(selected$framing, complete$framing)
  expect_equal(selected$reference, complete$reference)
})

test_that("the whole plot keeps every panel at its place and embeds the rest as SVG", {
  skip_if_not_installed("htmlwidgets")
  skip_if_not_installed("svglite")
  p <- ggplot2::ggplot(mtcars, ggplot2::aes(wt, mpg, z = hp, colour = factor(cyl))) +
    geom_point_3d() + coord_3d() + ggplot2::facet_wrap(~am) + ggplot2::labs(title = "Cars")
  w <- view_3d(p, width = 720, height = 400)
  expect_equal(c(w$x$frame$width_mm, w$x$frame$height_mm), c(720, 400) / 96 * 25.4)
  expect_length(w$x$panels, 2L)
  rects <- do.call(rbind, lapply(w$x$panels, `[[`, "rect_mm"))
  expect_true(all(rects[, 1L] >= 0 & rects[, 1L] + rects[, 3L] <= w$x$frame$width_mm + 1e-6))
  expect_true(all(rects[, 2L] >= 0 & rects[, 2L] + rects[, 4L] <= w$x$frame$height_mm + 1e-6))
  expect_lt(rects[1L, 1L] + rects[1L, 3L], rects[2L, 1L])
  expect_equal(rects[1L, 2L], rects[2L, 2L])
  expect_match(w$x$chrome, "^<svg")
  expect_match(w$x$chrome, "Cars", fixed = TRUE)
  expect_match(w$x$chrome, "factor(cyl)", fixed = TRUE)
  expect_true(w$x$link)
  expect_false(view_3d(p, link = FALSE)$x$link)
  expect_identical(vapply(w$x$panels, `[[`, integer(1), "id"), 1:2)
})

test_that("viewers in one link group carry the group name", {
  skip_if_not_installed("htmlwidgets")
  p <- ggplot2::ggplot(mtcars, ggplot2::aes(wt, mpg, z = hp)) +
    geom_point_3d() + coord_3d() + facet_views(c("front", "isometric"))
  front <- view_3d(p, panel = 1, width = 320, height = 320, link = "cars")
  iso <- view_3d(p, panel = 2, width = 320, height = 320, link = "cars")
  expect_identical(front$x$link_group, "cars")
  expect_identical(iso$x$link_group, "cars")
  expect_true(front$x$link)
  expect_equal(c(front$x$panels[[1L]]$camera$azimuth, iso$x$panels[[1L]]$camera$azimuth), c(-90, -45))
  expect_null(view_3d(p, panel = 1, width = 320, height = 320)$x$link_group)
  expect_error(view_3d(p, link = c("a", "b")), "one group name")
  script <- paste(readLines(system.file("htmlwidgets", "ggplot3d_view.js", package = "ggplot3d")), collapse = "
")
  expect_match(script, "linkGroups", fixed = TRUE)
})
