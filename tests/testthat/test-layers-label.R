label_panel_3d <- function(plot) {
  grDevices::pdf(tempfile(fileext = ".pdf"))
  on.exit(grDevices::dev.off(), add = TRUE)
  motion_panels_3d(grid::grid.force(ggplot2::ggplotGrob(plot)))[[1L]]
}

repel_children_3d <- function(panel) {
  placed <- Filter(function(g) identical(g$name, "scene-repel-3d"), panel$children)
  placed[[1L]]
}

box_overlaps_3d <- function(centre, half) {
  n <- nrow(centre); count <- 0L
  for (i in seq_len(n)) for (k in seq_len(n)) if (k > i) {
    if (half[i, 1] + half[k, 1] - abs(centre[i, 1] - centre[k, 1]) > 1e-3 &&
        half[i, 2] + half[k, 2] - abs(centre[i, 2] - centre[k, 2]) > 1e-3) count <- count + 1L
  }
  count
}

test_that("geom_label_3d draws padded boxes like geom_label inside coord_3d", {
  d <- data.frame(x = 1:2, y = 1:2, z = 1:2, label = c("a", "bb"))
  base <- ggplot2::ggplot(d, ggplot2::aes(x, y, z = z, label = label))
  ours <- integration_packets(base + geom_label_3d(label.padding = grid::unit(1, "mm")) + coord_3d())$packet
  theirs <- integration_packets(base + ggplot2::geom_label(label.padding = grid::unit(1, "mm")) + coord_3d())$packet
  expect_equal(ours$text$width_mm, theirs$text$width_mm)
  expect_equal(ours$styles$label_padding, c(1, 1))
  panel <- label_panel_3d(base + geom_label_3d(fill = "white") + coord_3d())
  expect_length(grid::getGrob(panel, "roundrect", grep = TRUE, global = TRUE), 2L)
})

test_that("the solver separates labels that start on one point", {
  n <- 6L
  anchors <- matrix(50, 1L, 2L)
  start <- matrix(50, n, 2L)
  half <- cbind(rep(6, n), rep(2, n))
  fit <- repel_labels_cpp(anchors, 1, rep(1L, n), start, half, c(0, 100, 0, 100),
    1, 1, 10000L, Inf, 100L, 0L, 1, TRUE)
  centre <- cbind(fit$x, fit$y)
  expect_false(fit$overlapping)
  expect_lt(fit$iterations, 10000L)
  expect_identical(box_overlaps_3d(centre, half), 0L)
  outside <- pmax(abs(centre[, 1] - 50) - half[, 1], 0)^2 + pmax(abs(centre[, 2] - 50) - half[, 2], 0)^2
  expect_true(all(outside >= 1 - 1e-6))
  again <- repel_labels_cpp(anchors, 1, rep(1L, n), start, half, c(0, 100, 0, 100),
    1, 1, 10000L, Inf, 100L, 0L, 1, TRUE)
  expect_identical(again$x, fit$x)
})

test_that("the solver keeps direction, bounds and the overlap limit", {
  n <- 4L
  anchors <- matrix(c(10, 10), 1L)
  half <- cbind(rep(3, n), rep(1, n))
  vertical <- repel_labels_cpp(anchors, 0.5, rep(1L, n), matrix(10, n, 2L), half, c(0, 40, 0, 40),
    1, 1, 10000L, Inf, 100L, 2L, 3, TRUE)
  expect_equal(vertical$x, rep(10, n), tolerance = 1e-2)
  expect_true(all(vertical$x - 3 >= 0 & vertical$y - 1 >= 0 & vertical$y + 1 <= 40))
  limited <- repel_labels_cpp(anchors, 0.5, rep(1L, n), matrix(10, n, 2L), half, c(0, 40, 0, 40),
    1, 1, 10000L, Inf, 1L, 0L, 3, TRUE)
  expect_identical(limited$hidden, c(FALSE, FALSE, TRUE, TRUE))
})

test_that("repelled labels are placed on the page with leaders in both backends", {
  d <- data.frame(x = c(1, 1.02, 1.04, 2, 3), y = c(1, 1.02, 1, 2, 1), z = c(1, 1.02, 1.01, 2, 3),
                  name = c("first label", "second label", "third label", "", "far"))
  base <- ggplot2::ggplot(d, ggplot2::aes(x, y, z = z, label = name)) + geom_point_3d()
  for (backend in c("vector", "raster")) {
    placed <- repel_children_3d(label_panel_3d(base + geom_text_repel_3d(seed = 4, min.segment.length = 0) +
      coord_3d(backend = backend_3d(backend))))
    labels <- grid::getGrob(placed, "scene-repel-labels")
    expect_length(labels$label, 4L)
    expect_identical(placed$hidden, 0L)
    leaders <- grid::getGrob(placed, "scene-repel-leaders")
    expect_gte(length(leaders$x0), 2L)
  }
  boxed <- repel_children_3d(label_panel_3d(base + geom_label_repel_3d(seed = 4, fill = "white") + coord_3d()))
  expect_length(grid::getGrob(boxed, "roundrect", grep = TRUE, global = TRUE), 4L)
  none <- repel_children_3d(label_panel_3d(base + geom_text_repel_3d(seed = 4, min.segment.length = Inf) + coord_3d()))
  expect_null(grid::getGrob(none, "scene-repel-leaders"))
})

test_that("leaders run from the box edge to the edge of the point's disc", {
  leader <- repel_leaders_3d(matrix(c(0, 0), 1L), matrix(c(10, 0), 1L), matrix(c(2, 1), 1L), 1)
  expect_equal(c(leader$x0, leader$y0, leader$x1, leader$y1, leader$length), c(8, 0, 1, 0, 7))
})

test_that("a warm start keeps labels where the previous frame left them", {
  d <- data.frame(x = c(1, 1.01, 1.02, 2), y = c(1, 1.01, 1, 2), z = c(1, 1, 1, 2),
                  name = c("one", "two", "three", "four"))
  p <- ggplot2::ggplot(d, ggplot2::aes(x, y, z = z, label = name)) + geom_text_repel_3d(seed = 1) + coord_3d()
  grDevices::pdf(tempfile(fileext = ".pdf"))
  on.exit(grDevices::dev.off(), add = TRUE)
  grob <- integration_packets(p)$grobs[[1L]]
  expect_s3_class(grob, "scene_repel_grob")
  state <- new.env(parent = emptyenv())
  context <- list(project = function(xyz) cbind(xyz[, 1] * 100, xyz[, 2] * 100, xyz[, 3]),
    width_mm = 100, height_mm = 100, state = state)
  first <- place_overlay_3d(grob, context)
  grob$settings$seed <- 99
  second <- place_overlay_3d(grob, context)
  expect_equal(second$iterations, 0L)
  position <- function(g) grid::convertX(grid::getGrob(g, "scene-repel-labels")$x, "mm", TRUE)
  expect_equal(position(second), position(first))
  cold <- place_overlay_3d(grob, modifyList(context, list(state = NULL)))
  expect_false(isTRUE(all.equal(position(cold), position(first))))
})

test_that("repel arguments are checked", {
  expect_error(geom_text_repel_3d(force = -1), "nonnegative number")
  expect_error(geom_text_repel_3d(box.padding = "a"), "nonnegative length")
  expect_error(geom_text_repel_3d(direction = "z"), "should be one of")
  expect_error(geom_text_repel_3d(seed = c(1, 2)), "seed must be")
  expect_error(geom_label_repel_3d(xlim = 1), "panel fractions")
  expect_error(geom_text_repel_3d(nudge_x = Inf), "nudge_x")
})

test_that("animate_3d carries label offsets from frame to frame", {
  skip_if_not_installed("ragg")
  d <- data.frame(x = c(1, 1.01, 1.02, 2), y = c(1, 1.01, 1, 2), z = c(1, 1, 1, 2),
                  name = c("one", "two", "three", "four"))
  p <- ggplot2::ggplot(d, ggplot2::aes(x, y, z = z, label = name)) + geom_point_3d() +
    geom_text_repel_3d() + coord_3d()
  calls <- list()
  solve <- repel_labels_cpp
  local_mocked_bindings(repel_labels_cpp = function(...) {
    fit <- solve(...)
    calls[[length(calls) + 1L]] <<- fit
    fit
  })
  movie <- animate_3d(p, azimuth = c(-60, -59.5), frames = 2, width = 240, height = 240)
  on.exit(unlink(movie$frames), add = TRUE)
  expect_length(calls, 2L)
  expect_lt(calls[[2L]]$iterations, calls[[1L]]$iterations)
})
