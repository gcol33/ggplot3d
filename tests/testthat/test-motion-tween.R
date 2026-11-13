tween_test_states <- function(plot) {
  plot <- tween_plot_3d(plot)
  values <- motion_time_values_3d(plot, "t")
  limits <- motion_frame_limits_3d(plot, "t", values)
  lapply(values, function(value) ggplot2::ggplot_build(motion_frame_plot_3d(plot, "t", value, limits)))
}

test_that("tween schedules preserve endpoints and validate easing", {
  spec <- tween_3d(n = 2)
  schedule <- tween_schedule_3d(c("a", "b", "c"), spec)
  expect_equal(schedule$from, c(1L, 1L, 1L, 2L, 2L, 2L, 3L))
  expect_equal(schedule$fraction, c(0, 1/3, 2/3, 0, 1/3, 2/3, 0))
  expect_equal(nrow(tween_schedule_3d(1, spec)), 1L)
  expect_error(tween_3d(n = -1), "nonnegative")
  expect_error(tween_3d(ease = function(x) x + 1), "preserve")
  expect_error(tween_schedule_3d(1:2, tween_3d(ease = function(x) x + sin(pi*x))), "between zero and one")
})

test_that("raw group values match reordered points and disappearing groups", {
  d <- data.frame(x = c(0, 10, 20, 2), y = 0, z = 1,
    id = c("a", "b", "b", "a"), t = c(0, 0, 1, 1))
  p <- ggplot2::ggplot(d, ggplot2::aes(x, y, z = z, group = id)) + geom_point_3d() + coord_3d()
  states <- tween_test_states(p)
  entry <- find_emitter_3d(states[[1L]]$plot@layers[[1L]]$geom)
  a <- states[[1L]]$data[[1L]]; b <- states[[2L]]$data[[1L]]
  middle <- tween_layer_3d(a, b, .5, entry, tween_3d())
  expect_equal(middle$x[match(c("a", "b"), middle$.tween_group)], c(1, 15))
  source <- tween_layer_3d(a, b, .5, entry, tween_3d(match = ".source"))
  expect_equal(source$x, c(10, 6))
  gone <- tween_layer_3d(a, b[b$.tween_group == "b", , drop = FALSE], .25, entry, tween_3d())
  expect_equal(gone$alpha[gone$.tween_group == "a"], .75)
  expect_equal(gone$x[gone$.tween_group == "b"], 12.5)
  expect_identical(tween_layer_3d(a, b, 0, entry, tween_3d()), a)
  expect_identical(tween_layer_3d(a, b, 1, entry, tween_3d()), b)
})

test_that("numeric attributes, colours and rotations interpolate with their contracts", {
  a <- data.frame(x = c(0, 2), y = 0, z = 0, fill = c("black", "red"), label = c("first", "second"))
  b <- a; b$x <- c(2, 4); b$fill <- c("white", "blue"); b$label <- c("third", "fourth")
  out <- tween_rows_3d(a, b, .5)
  expect_equal(out$x, c(1, 3))
  expect_equal(grDevices::col2rgb(out$fill[1L])[, 1L], c(188L, 188L, 188L), ignore_attr = TRUE)
  expect_equal(out$label, c("third", "fourth"))
  a$rotation <- list(diag(3), diag(3))
  turn <- matrix(c(0, 1, 0, -1, 0, 0, 0, 0, 1), 3L)
  b$rotation <- list(turn, turn)
  rotation <- tween_rows_3d(a, b, .5)$rotation[[1L]]
  expect_equal(crossprod(rotation), diag(3), tolerance = 1e-12)
  expect_equal(det(rotation), 1)
  expect_equal(rotation[1:2, 1L], rep(sqrt(.5), 2), tolerance = 1e-12)
})

test_that("paths resample by arc length and polygons align winding and starts", {
  a <- data.frame(x = c(0, 2), y = 0, z = 0, group = 1, .tween_group = "a", .source = 1:2)
  b <- data.frame(x = 0:2, y = 2, z = 0, group = 1, .tween_group = "a", .source = 1:3)
  out <- tween_paths_3d(a, b, .5, FALSE)
  expect_equal(out$x, 0:2)
  expect_equal(out$y, rep(1, 3))
  square <- data.frame(x = c(0, 1, 1, 0), y = c(0, 0, 1, 1), z = 0)
  out <- tween_paths_3d(square, square[c(3, 2, 1, 4), ], .5, TRUE)
  expect_equal(out, square, ignore_attr = TRUE)
})

test_that("fixed surfaces pair face keys when input rows are reordered", {
  a <- expand.grid(x = 0:2, y = 0:2); a$z <- a$x + a$y; a$t <- 0
  b <- a[nrow(a):1L, ]; b$z <- 2 * b$z; b$t <- 1
  d <- rbind(a, b)
  p <- ggplot2::ggplot(d, ggplot2::aes(x, y, z = z, fill = z)) + geom_surface_3d() + coord_3d()
  states <- tween_test_states(p)
  entry <- find_emitter_3d(states[[1L]]$plot@layers[[1L]]$geom)
  middle <- tween_layer_3d(states[[1L]]$data[[1L]], states[[2L]]$data[[1L]], .5, entry, tween_3d())
  expect_equal(middle$z, 1.5 * (middle$x + middle$y))
  expect_equal(nrow(middle), nrow(states[[1L]]$data[[1L]]))
  table <- states[[1L]]; table$data[[1L]] <- middle
  expect_no_error(ggplot2::ggplot_gtable(table))
})

test_that("polygon rings retain holes during interpolation", {
  outer <- data.frame(x = c(0, 2, 2, 0), y = c(0, 0, 2, 2), ring = 1L)
  hole <- data.frame(x = c(.5, .5, 1.5, 1.5), y = c(.5, 1.5, 1.5, .5), ring = 2L)
  a <- rbind(outer, hole); a$z <- 0; a$t <- 0
  b <- a; b$x <- b$x + 1; b$t <- 1
  p <- ggplot2::ggplot(rbind(a, b), ggplot2::aes(x, y, z = z, subgroup = ring, group = 1)) +
    geom_polygon_3d(fill = "steelblue") + coord_3d()
  states <- tween_test_states(p)
  entry <- find_emitter_3d(states[[1L]]$plot@layers[[1L]]$geom)
  middle <- tween_layer_3d(states[[1L]]$data[[1L]], states[[2L]]$data[[1L]], .5, entry, tween_3d())
  expect_length(unique(middle$group), 1L)
  expect_setequal(middle$subgroup, 1:2)
  built <- states[[1L]]; built$data[[1L]] <- middle
  packet <- motion_panels_3d(ggplot2::ggplot_gtable(built))[[1L]]$packets[[1L]]
  params <- built$layout$panel_params[[1L]]
  x <- packet$vertices$x * diff(params$ranges$x) + params$ranges$x[1L]
  y <- packet$vertices$y * diff(params$ranges$y) + params$ranges$y[1L]
  area <- sum(vapply(seq_along(packet$faces$offset), function(i) {
    ids <- packet$corners$vertex[packet$faces$offset[i] + 0:2]
    abs((x[ids[2L]] - x[ids[1L]]) * (y[ids[3L]] - y[ids[1L]]) -
      (x[ids[3L]] - x[ids[1L]]) * (y[ids[2L]] - y[ids[1L]])) / 2
  }, numeric(1)))
  expect_equal(area, 3, tolerance = 1e-9)
})

test_that("mesh connectivity changes cross-fade complete shapes", {
  a <- data.frame(x = c(0, 1, 0), y = c(0, 0, 1), z = 0, .mesh = 1L, .face = 1L,
    .corner = 1:3, .vertex = 1:3, group = 1L, .tween_group = "a", alpha = 1)
  b <- a; b$.vertex[3L] <- 4L; b$z <- 1
  out <- tween_layer_3d(a, b, .25, list(tween = "group"), tween_3d())
  expect_equal(nrow(out), 6L)
  expect_equal(out$z, c(0, 0, 0, 1, 1, 1))
  expect_equal(out$alpha, c(rep(.75, 3), rep(.25, 3)))
  expect_length(unique(out$.mesh), 2L)
  expect_length(unique(out$group), 2L)
  out <- tween_layer_3d(a[FALSE, ], b, .5, list(tween = "row"),
    tween_3d(enter = function(d) { d$z <- 0; d }))
  expect_equal(out$z, rep(.5, 3))
})

test_that("statistics finish before intermediate data are generated", {
  counter <- new.env(parent = emptyenv()); counter$n <- 0L
  StatSquare <- ggplot2::ggproto("StatSquare", ggplot2::Stat,
    required_aes = c("x", "y", "z"),
    compute_group = function(data, scales) {
      counter$n <- counter$n + 1L
      data.frame(x = mean(data$x), y = mean(data$y), z = mean(data$z)^2)
    })
  d <- data.frame(x = c(0, 0), y = 0, z = c(1, 3), t = 0:1)
  p <- ggplot2::ggplot(d, ggplot2::aes(x, y, z = z)) + geom_point_3d(stat = StatSquare) + coord_3d()
  states <- tween_test_states(p); calls <- counter$n
  schedule <- tween_schedule_3d(0:1, tween_3d(n = 1))
  table <- motion_tween_build_3d(states, schedule, 2L, tween_3d(n = 1))
  expect_equal(counter$n, calls)
  packet <- motion_panels_3d(table)[[1L]]$packets[[1L]]
  range <- states[[1L]]$layout$panel_params[[1L]]$ranges$z
  expect_equal(packet$vertices$z * diff(range) + range[1L], 5)
})

test_that("animate_3d renders in-between frames with fixed limits and transition metadata", {
  d <- data.frame(x = c(0, 1, 1, 2), y = c(0, 1, 1, 0), z = c(0, 1, 2, 0),
    id = c("a", "b", "a", "b"), t = c(0, 0, 1, 1))
  p <- ggplot2::ggplot(d, ggplot2::aes(x, y, z = z, group = id)) + geom_point_3d() +
    coord_3d(axes = character())
  for (renderer in c("vector", "raster")) {
    out <- animate_3d(p, time = "t", tween = tween_3d(n = 1), width = 240, height = 240,
      renderer = renderer, progress = FALSE)
    on.exit(unlink(out$frames), add = TRUE)
    expect_equal(out$frame_count, 3L)
    expect_equal(out$camera$value, c(0, .5, 1))
    expect_equal(out$transition$fraction, c(0, .5, 0))
    expect_length(unique(vapply(out$panel_extents, function(x) paste(unlist(x), collapse = ","), character(1))), 1L)
    expect_true(all(file.info(out$frames)$size > 0))
  }
  expect_error(animate_3d(p, tween = tween_3d()), "time set")
})

test_that("gganimate state and time transitions give actionable guidance", {
  skip_if_not_installed("gganimate")
  d <- data.frame(x = 1:4, y = 1:4, z = 1:4, t = c(0, 0, 1, 1))
  p <- ggplot2::ggplot(d, ggplot2::aes(x, y, z = z)) + geom_point_3d() + coord_3d()
  for (transition in list(gganimate::transition_states(t), gganimate::transition_time(t))) {
    expect_error(gganimate::animate(p + transition, nframes = 2, fps = 2, width = 200, height = 200,
      renderer = gganimate::file_renderer(tempdir(), prefix = "ggplot3d-unsupported-")), "animate_3d")
  }
})
