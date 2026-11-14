test_that("quiver endpoints honour pivots normalization and raw magnitude", {
  d <- data.frame(x = 1, y = 2, z = 3, u = 3, v = 4, w = 0)
  p <- ggplot2::ggplot(d, ggplot2::aes(x, y, z = z, u = u, v = v, w = w,
    fill = ggplot2::after_stat(norm)))
  for (pivot in c("tail", "middle", "tip")) {
    values <- ggplot2::ggplot_build(p + geom_quiver_3d(scale = 2, pivot = pivot, normalize = TRUE) + coord_3d())$data[[1L]]
    expect_equal(values$norm, 5)
    expect_equal(c(values$xend - values$x, values$yend - values$y, values$zend - values$z), c(1.2, 1.6, 0))
    fraction <- switch(pivot, tail = 0, middle = .5, tip = 1)
    expect_equal(c(values$x, values$y, values$z), c(1, 2, 3) - fraction * c(1.2, 1.6, 0))
  }
})

test_that("cone heads and tube shafts are solid geometry with transformed directions", {
  d <- data.frame(x = c(1, 2), y = 0, z = 0, u = c(2, 0), v = 0, w = 0)
  p <- ggplot2::ggplot(d, ggplot2::aes(x, y, z = z, u = u, v = v, w = w))
  for (shaft in c("line", "tube")) {
    out <- stats_scene(p + geom_quiver_3d(shaft = shaft, detail = 0) + coord_3d(aspect = c(2, 1, 1)))
    q <- out$packet$quadrics
    expect_identical(sort(q$kind), if (shaft == "tube") 2:3 else 3L)
    expect_true(all(is.finite(unlist(q[ggplot3d:::quadric_transform_columns]))))
    expect_true(all(q$source_row == 1L))
  }
  plain <- stats_scene(p + geom_quiver_3d(head = "none") + coord_3d())
  expect_length(plain$packet$faces$offset, 0L)
  expect_length(plain$packet$polylines$offset, 1L)
  flat <- stats_scene(p + geom_quiver_3d(head = "flat", shaft = "tube", detail = 0) + coord_3d())
  expect_true(any(flat$packet$polylines$arrow_end))
  cone <- stats_scene(p + geom_cone_3d(detail = 0, colour = NA) + ggplot2::scale_x_log10() + coord_3d())
  q <- cone$packet$quadrics
  expect_identical(q$kind, 3L)
  expect_identical(q$radius_ratio, 0)
  expect_length(cone$packet$polylines$offset, 0L)
  values <- cone$built$data[[1L]]
  expect_equal(values$xend, log10(c(3, 2)))
  range <- cone$built$layout$panel_params[[1L]]$ranges$x
  # The apex is the image of local (0, 0, 1): origin plus the axis column.
  expect_equal(q$transform_13 + q$transform_9, (log10(3) - range[1L]) / diff(range))
  expect_error(geom_quiver_3d(head_length = 2), "at most one")
})
