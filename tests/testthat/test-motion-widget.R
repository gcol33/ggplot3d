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
  w <- view_3d(p, width = 360, height = 360, elementId = "motion-test")
  expect_s3_class(w, "htmlwidget")
  expect_s3_class(w, "ggplot3d_view")
  expect_identical(w$elementId, "motion-test")
  expect_identical(w$x$schema_version, 2L)
  expect_equal(w$x$camera$azimuth, -35)
  expect_equal(w$x$camera$elevation, 25)
  expect_equal(w$x$camera$roll, 12)
  expect_identical(w$x$camera$projection, "perspective")
  expect_equal(w$x$camera$aspect, c(1, 2, 1))
  for (name in c("scene", "furniture")) {
    scene <- w$x[[name]]
    expect_identical(scene$schema_version, 1L)
    expect_length(scene$materials, scene$style_count)
    expect_null(scene$styles)
    schemas <- ggplot3d:::scene_schemas()
    for (table in names(schemas)) {
      expect_identical(names(scene[[table]]), names(schemas[[table]]))
      expect_true(all(vapply(scene[[table]], function(v) all(is.finite(v)), logical(1))))
    }
  }
  expect_gt(length(w$x$scene$faces$offset), 0)
  expect_gt(length(w$x$scene$polylines$offset), 0)
  expect_gt(length(w$x$scene$markers$offset), 0)
  expect_gt(length(w$x$scene$text$offset), 0)
  expect_true(all(vapply(w$x$scene$materials, function(s) length(s$colour) == 4, logical(1))))
  expect_true(any(vapply(w$x$scene$materials, function(s)
    identical(s$fill, ggplot3d:::view_rgba_3d("#2878A0")), logical(1))))
  expect_length(w$x$axes, 3L)
  expect_identical(w$x$capabilities$opaque_visibility, "depth-buffer")
  expect_identical(w$x$capabilities$transparency, "primitive-centroid")
  expect_true(all(is.finite(unlist(w$x$viewport))))
  path <- tempfile(fileext = ".html")
  on.exit(unlink(c(path, sub(".html$", "_files", path)), recursive = TRUE), add = TRUE)
  htmlwidgets::saveWidget(w, path, selfcontained = FALSE)
  expect_true(file.exists(path))
  expect_match(paste(readLines(path), collapse = "\n"), "ggplot3d_view.js", fixed = TRUE)
})

test_that("viewer selects a facet and validates its index", {
  skip_if_not_installed("htmlwidgets")
  p <- ggplot2::ggplot(mtcars, ggplot2::aes(wt, mpg, z = hp)) +
    geom_point_3d() + coord_3d() + ggplot2::facet_wrap(~am)
  w <- view_3d(p, panel = 2, width = 480, height = 320)
  expect_identical(w$x$scene$panel_id, 2L)
  expect_error(view_3d(p, panel = 3), "outside")
  expect_error(view_3d(p, panel = 0), "positive integer")
})
