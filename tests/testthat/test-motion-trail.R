trail_test_job <- function(plot, trail = 2, tween = NULL) {
  plot <- tween_plot_3d(plot)
  values <- motion_time_values_3d(plot, "t")
  limits <- motion_frame_limits_3d(plot, "t", values)
  states <- lapply(values, function(value) ggplot2::ggplot_build(motion_frame_plot_3d(plot, "t", value, limits)))
  trail <- motion_trail_spec_3d(trail, "t")
  list(plot = plot, keyframes = states, schedule = if (!is.null(tween)) tween_schedule_3d(values, tween),
    tween = tween, trail = trail, trail_layers = motion_trail_layers_3d(plot, "t", trail),
    trail_history = new.env(parent = emptyenv()))
}

trail_test_plot <- function(data = data.frame(x = 0:3, y = c(0, 1, 0, 1), z = 0:3, t = 1:4)) {
  ggplot2::ggplot(data, ggplot2::aes(x, y, z = z, group = 1)) +
    geom_point_3d(colour = "steelblue", size = 3) + coord_3d(axes = character())
}

test_that("trail specifications fail early on invalid windows and options", {
  p <- trail_test_plot()
  for (bad in list(0, -1, 1.5, NA_real_, Inf, c(1, 2), .Machine$integer.max)) {
    expect_error(animate_3d(p, time = "t", trail = bad), "trail length")
  }
  expect_error(animate_3d(p, trail = 2), "requires time")
  expect_error(animate_3d(p, time = "t", trail = list(length = 2, typo = 1)), "trail must")
  expect_error(motion_trail_spec_3d(list(length = 2, radius = 0), "t"), "positive finite")
  expect_error(motion_trail_spec_3d(list(length = 2, width = NA_real_), "t"), "positive finite")
  expect_error(motion_trail_spec_3d(list(length = 2, radius = 1, width = 1), "t"), "either")
  expect_error(motion_trail_spec_3d(list(length = 2, taper = NA), "t"), "trail taper")
  expect_error(motion_trail_spec_3d(list(length = 2, normal = c(0, 0, 0)), "t"), "nonzero")
  expect_null(motion_trail_spec_3d(NULL, NULL))
})

test_that("windows contain preceding frames and fade the existing alpha", {
  p <- trail_test_plot() + geom_point_3d(alpha = .6)
  job <- trail_test_job(p)
  expect_length(motion_trail_curves_3d(motion_trail_history_3d(job, 1)[[1]], 1), 0)
  history <- motion_trail_history_3d(job, 4)
  expect_equal(history[[1]]$.trail_frame, 2:4)
  expect_equal(history[[1]]$alpha, c(0, .5, 1))
  expect_equal(history[[2]]$alpha, .6 * c(0, .5, 1))
  expect_setequal(ls(job$trail_history), as.character(2:4))
  params <- job$keyframes[[1]]$layout$panel_params[[1]]
  packets <- motion_trail_packets_3d(history[1], params, job$trail)
  expect_length(packets, 1)
  expect_gt(length(packets[[1]]$faces$offset), 0)
  expect_equal(range(packets[[1]]$faces$opacity), c(0, 1))
  expect_true(all(is.finite(unlist(packets[[1]]$vertices))))
  expect_identical(unique(packets[[1]]$faces$layer_id), 1L)
})

test_that("trails use the same identities as tweening across reordered groups", {
  d <- data.frame(x = c(0, 10, 12, 2), y = 0, z = 0,
    id = c("a", "b", "b", "a"), t = c(0, 0, 1, 1))
  p <- ggplot2::ggplot(d, ggplot2::aes(x, y, z = z, group = id, colour = id)) +
    geom_point_3d() + coord_3d()
  job <- trail_test_job(p, trail = 4, tween = tween_3d(n = 3))
  history <- motion_trail_history_3d(job, 5)[[1]]
  curves <- motion_trail_curves_3d(history, 1)
  expect_length(curves, 2)
  expect_equal(unname(sort(vapply(curves, function(d) diff(range(d$x)), numeric(1)))), c(2, 2))
  expect_equal(curves[[1]]$.trail_frame, 1:5)
  expect_equal(sort(history$x[history$.trail_frame == 3]), c(1, 11))
  source <- trail_test_job(p, trail = 4, tween = tween_3d(n = 3, match = ".source"))
  curves <- motion_trail_curves_3d(motion_trail_history_3d(source, 5)[[1]], 1)
  expect_equal(unname(sort(vapply(curves, function(d) diff(range(d$x)), numeric(1)))), c(8, 12))
  fresh <- trail_test_job(p, trail = 4, tween = tween_3d(n = 3))
  expect_equal(motion_trail_history_3d(job, 4), motion_trail_history_3d(fresh, 4))
})

test_that("missing and nonfinite observations split trails and stationary marks add no surface", {
  d <- data.frame(x = c(0, 1, NA, 3, 4, 5), y = 0, z = 0,
    PANEL = 1, .trail_key = "a", .trail_frame = c(1:5, 7))
  curves <- motion_trail_curves_3d(d, 1)
  expect_length(curves, 2)
  expect_equal(lapply(curves, `[[`, "x"), list(c(0, 1), c(3, 4)), ignore_attr = TRUE)
  expect_length(motion_trail_curves_3d(d, 2), 0)
  d$x <- 1
  expect_length(motion_trail_curves_3d(d, 1), 0)
  d <- data.frame(x = c(0, 1, 1), y = 0, z = 0, PANEL = 1,
    .trail_key = "a", .trail_frame = 1:3)
  expect_equal(motion_trail_curves_3d(d, 1)[[1]]$.trail_frame, c(1L, 3L))
})

test_that("static layers and group-based surfaces are excluded and layer selection is validated", {
  p <- trail_test_plot() + geom_point_3d(data = data.frame(x = 0, y = 0, z = 0))
  job <- trail_test_job(p)
  expect_identical(job$trail_layers, 1L)
  expect_error(motion_trail_layers_3d(job$plot, "t", list(layers = 2)), "time-varying row-based")
  p <- p + geom_point_3d()
  job <- trail_test_job(p, list(length = 2, layers = 3))
  expect_identical(job$trail_layers, 3L)
  surface <- ggplot2::ggplot(expand.grid(x = 0:1, y = 0:1, t = 1:2), ggplot2::aes(x, y, z = x)) +
    geom_surface_3d() + coord_3d()
  expect_length(trail_test_job(surface)$trail_layers, 0)
})

test_that("object absence leaves separate history runs and glyph anchors use the row registry", {
  d <- data.frame(x = c(0, 10, 1, 11, 12, 3, 13, 4, 14), y = 0, z = 0,
    id = c("a", "b", "a", "b", "b", "a", "b", "a", "b"), t = c(1, 1, 2, 2, 3, 4, 4, 5, 5))
  p <- ggplot2::ggplot(d, ggplot2::aes(x, y, z = z, group = id, fill = id)) +
    geom_glyph_3d(size = .1, size.unit = "data", detail = 0) + coord_3d()
  job <- trail_test_job(p, trail = 4)
  expect_identical(job$trail_layers, 1L)
  history <- motion_trail_history_3d(job, 5)
  curves <- motion_trail_curves_3d(history[[1]], 1)
  expect_equal(sort(vapply(curves, nrow, integer(1)), method = "radix"), c(2L, 2L, 5L), ignore_attr = TRUE)
  expect_equal(sort(vapply(curves, function(d) min(d$x), numeric(1))), c(0, 3, 10), ignore_attr = TRUE)
})

test_that("tube and ribbon geometry share the sweep with mapped sizes and tapering", {
  job <- trail_test_job(trail_test_plot(), list(length = 3, radius = .1))
  history <- motion_trail_history_3d(job, 4)
  params <- job$keyframes[[1]]$layout$panel_params[[1]]
  curve <- motion_trail_curves_3d(history[[1]], 1)[[1]]
  curve$fill <- curve$colour; curve$colour <- NA_character_; curve$group <- 1L
  expected <- sweep_surface_3d(curve, rep(.1, nrow(curve)), rep(0, nrow(curve)),
    sweep_profile_3d(NULL, 8L, NULL), FALSE, "flat", NULL)
  packet <- emit_scene_3d(expected, list(.emitter = "GeomStatsSurface3D", shade = TRUE), params, 1L)
  expect_equal(motion_trail_packets_3d(history, params, job$trail)[[1]], packet)
  tapered <- job$trail; tapered$taper <- TRUE
  expect_false(identical(motion_trail_packets_3d(history, params, tapered)[[1]]$vertices, packet$vertices))
  history[[1]]$radius <- .04
  mapped <- motion_trail_packets_3d(history, params, job$trail)
  job$trail$radius <- .04; history[[1]]$radius <- NULL
  expect_equal(mapped, motion_trail_packets_3d(history, params, job$trail))
  ribbon <- motion_trail_spec_3d(list(length = 3, width = .2, normal = c(0, 0, 1)), "t")
  strips <- motion_trail_packets_3d(history, params, ribbon)
  expect_equal(length(strips[[1]]$faces$offset), 2 * (nrow(curve) - 1))
})

test_that("facets and transformed positions retain their own trail coordinates", {
  d <- expand.grid(t = 1:3, site = c("a", "b"))
  d$x <- 10^d$t; d$y <- d$t; d$z <- d$t + ifelse(d$site == "b", 100, 0)
  p <- trail_test_plot(d) + ggplot2::scale_x_log10() + ggplot2::facet_wrap(~site) +
    coord_3d(z_scales = "free")
  job <- trail_test_job(p)
  history <- motion_trail_history_3d(job, 3)
  for (panel in 1:2) {
    curve <- motion_trail_curves_3d(history[[1]], panel)[[1]]
    expect_equal(curve$x, 1:3)
    params <- job$keyframes[[1]]$layout$panel_params[[panel]]
    packet <- motion_trail_packets_3d(history, params, job$trail)[[1]]
    expect_equal(packet$panel_id, panel)
    expect_true(all(is.finite(unlist(packet$vertices))))
  }
})

test_that("animate_3d renders native trails with and without tweening", {
  p <- trail_test_plot()
  counts <- integer()
  emit <- motion_trail_packets_3d
  local_mocked_bindings(motion_trail_packets_3d = function(...) {
    packets <- emit(...)
    counts <<- c(counts, sum(vapply(packets, function(p) length(p$faces$offset), integer(1))))
    packets
  })
  for (renderer in c("raster", "vector")) {
    for (spec in list(NULL, tween_3d(n = 1))) {
      counts <- integer()
      out <- animate_3d(p, time = "t", tween = spec, trail = 2, renderer = renderer,
        width = 200, height = 200, progress = FALSE)
      on.exit(unlink(out$frames), add = TRUE)
      expect_equal(out$frame_count, if (is.null(spec)) 4L else 7L)
      expect_true(all(file.info(out$frames)$size > 0))
      expect_identical(counts[1], 0L)
      expect_true(all(counts[-1] > 0))
      expect_equal(length(unique(vapply(out$panel_extents, function(x) paste(unlist(x), collapse = ","), character(1)))), 1L)
    }
  }
})
