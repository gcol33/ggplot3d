grid_test_device <- function(code, width = 7, height = 7) {
  path <- tempfile(fileext = ".png")
  ragg::agg_png(path, width = width, height = height, units = "in", res = 150)
  on.exit({ grDevices::dev.off(); unlink(path) }, add = TRUE)
  force(code)
}

grid_test_scene <- function() {
  packet <- new_scene_packet(
    vertices = list(x = c(0, 1, 0, 0.4, 0.6), y = c(0, 0, 1, 0.4, 0.6), z = c(0, 0, 0, 0.4, 0.6)),
    corners = list(vertex = 1:5), faces = list(offset = 1L, style_id = 1L),
    text = list(offset = 4:5, style_id = 2:3),
    styles = data.frame(style_id = 1:3, fill = c("red", NA, NA), colour = c(NA, "blue", "black"), label = c("", "a", "b")))
  scene <- scene_compile(packet, new_camera_3d(), new_scene_viewport(100, 100, 150))
  permutation <- order(scene$fragments$kind)
  scene$fragments <- lapply(scene$fragments, `[`, permutation)
  scene$fragments$fragment_id <- seq_along(permutation)
  scene$draw_order <- seq_along(permutation)
  scene
}

test_that("backend preserves consecutive draw runs and keyed styles", {
  grid_test_device({
    scene <- grid_test_scene()
    scene$draw_order <- c(2L, 1L, 3L)
    scene$styles <- scene$styles[c(3, 1, 2), ]
    result <- render_scene_grid(scene, scene$viewport)
    expect_identical(unlist(result$fragment_runs, use.names = FALSE), scene$draw_order)
    expect_length(result$children, 3L)
    expect_s3_class(result$children[[1]], "text")
    expect_s3_class(result$children[[2]], "polygon")
    expect_identical(result$children[[2]]$gp$fill, "red")
    scene$draw_order <- c(1L, 2L, 3L)
    result <- render_scene_grid(scene, scene$viewport)
    expect_length(result$children, 2L)
    expect_equal(result$children[[2]]$label, c("a", "b"))
  })
})

test_that("effective opacity is applied once and translucent coverage is unchanged", {
  grid_test_device({
    scene <- grid_test_scene()
    scene$fragments$opacity[1L] <- 0.4
    scene$styles$alpha <- 0.2
    result <- render_scene_grid(scene, scene$viewport)
    face <- result$children[[1L]]
    expect_equal(face$gp$alpha, 0.4)
    expect_true(is.na(face$gp$col))
    expect_null(face$vp)
  })
})

test_that("sparse style tables receive transparent colour defaults", {
  grid_test_device({
    scene <- grid_test_scene()
    scene$styles <- scene$styles["style_id"]
    result <- render_scene_grid(scene, scene$viewport)
    expect_true(all(is.na(result$children[[1L]]$gp$fill)))
    expect_true(all(is.na(result$children[[1L]]$gp$col)))
  })
})

test_that("masks use even-odd clipping and unsupported devices error", {
  grid_test_device({
    scene <- grid_test_scene()
    scene$fragments$mask_offset[2] <- 1L
    scene$fragments$mask_count[2] <- 4L
    scene$masks <- list(x_npc = c(0, 1, 1, 0), y_npc = c(0, 0, 1, 1), ring = rep(1L, 4))
    result <- render_scene_grid(scene, scene$viewport)
    expect_s3_class(result$children[[2]]$vp$clip, "GridClipPath")
    expect_true(result$capabilities$clipping_paths)
    testthat::local_mocked_bindings(grid_capabilities_3d = function() list(clipping_paths = FALSE))
    expect_error(render_scene_grid(scene, scene$viewport), "device does not support clipping paths")
  })
})

test_that("dash arclength advances across hidden or clipped sections", {
  viewport <- new_scene_viewport(100, 100)
  pattern <- c(2, 2)
  a <- dash_segments_3d(c(0, 0.1), c(0, 0), 0, pattern, viewport)
  b <- dash_segments_3d(c(0.05, 0.1), c(0, 0), 5, pattern, viewport)
  expect_equal(a$x, c(0, 0.02, 0.04, 0.06, 0.08, 0.1))
  expect_equal(b$x, c(0.05, 0.06, 0.08, 0.1))
  expect_identical(dash_pattern_3d("blank", 1), numeric())
})

test_that("opaque mesh seams are covered by a union underlay, never by outlines", {
  packet <- new_scene_packet(
    vertices = list(x = c(0, 1, 0, 1), y = c(0, 0, 1, 1), z = c(0, 0, 0, 0)),
    corners = list(vertex = c(1L, 2L, 3L, 3L, 4L, 2L)), faces = list(offset = c(1L, 4L), style_id = c(1L, 1L)),
    styles = data.frame(style_id = 1L, fill = "red", colour = NA))
  grid_test_device({
    scene <- scene_compile(packet, new_camera_3d(), new_scene_viewport(100, 100, 150))
    result <- render_scene_grid(scene, scene$viewport)
    expect_length(result$children, 1L)
    run <- result$children[[1L]]
    expect_s3_class(run, "gTree")
    underlay <- run$children[[1L]]; faces <- run$children[[2L]]
    expect_identical(underlay$gp$fill, "red")
    expect_s3_class(underlay, "pathgrob")
    expect_identical(underlay$rule, "winding")
    expect_true(is.na(underlay$gp$col))
    expect_s3_class(faces, "polygon")
    expect_true(all(is.na(faces$gp$col)))
    expect_equal(sort(as.numeric(underlay$x)), sort(as.numeric(faces$x)))
    signed <- vapply(split(seq_along(underlay$id), underlay$id), function(i) {
      x <- as.numeric(underlay$x)[i]; y <- as.numeric(underlay$y)[i]
      sum(x * c(y[-1L], y[1L]) - c(x[-1L], x[1L]) * y)
    }, numeric(1))
    expect_true(all(signed > 0))
  })
})

test_that("lighting changes fill without changing opacity or mapped materials", {
  scene <- grid_test_scene(); saved <- scene$styles
  fill <- shade_fill_3d("#808080", scene$fragments, 1L, scene$camera, light_3d(ambient = 0.2, diffuse = 0))
  expect_false(identical(fill, "#808080"))
  expect_identical(scene$styles, saved)
})
grid_test_panels <- function(grob) {
  if (inherits(grob, "scene_panel_grob") && !is.null(grob$scene)) return(list(grob))
  children <- if (length(grob$children)) as.list(grob$children) else grob$grobs
  unlist(lapply(children, grid_test_panels), recursive = FALSE)
}

test_that("intersecting surfaces paint in few single-layer runs, each underlaid", {
  surface <- expand.grid(x = seq(-1, 1, length.out = 12), y = seq(-1, 1, length.out = 12))
  first <- transform(surface, z = 0.6 * x)
  second <- transform(surface, z = -0.6 * x + 0.2 * y)
  plot <- ggplot2::ggplot(first, ggplot2::aes(x, y, z = z)) +
    geom_surface_3d(fill = "#2E6F9E") + geom_surface_3d(data = second, fill = "#9E5E0F") +
    coord_3d(azimuth = -45, elevation = 60)
  grid_test_device({
    panel <- grid_test_panels(grid::grid.force(ggplot2::ggplotGrob(plot)))[[1L]]
    f <- panel$scene$fragments; rendered <- panel$children[[2L]]
    surface_runs <- Filter(function(run) all(f$kind[run] == 1L & f$layer_id[run] > 0L), rendered$fragment_runs)
    expect_true(all(vapply(surface_runs, function(run) length(unique(f$layer_id[run])) == 1L, logical(1))))
    expect_lt(length(surface_runs), sum(lengths(surface_runs)) / 4)
    children <- rendered$children[match(surface_runs, rendered$fragment_runs)]
    expect_true(all(vapply(children[lengths(surface_runs) > 1L], function(g)
      inherits(g, "gTree") && inherits(g$children[[1L]], "pathgrob") && inherits(g$children[[2L]], "polygon"), logical(1))))
  })
})

grid_test_plot <- function(...) {
  ggplot2::ggplot(data.frame(x = c(0, 1), y = c(0, 1), z = c(0, 1)), ggplot2::aes(x, y, z = z)) +
    ggplot2::geom_point() + coord_3d(...) + ggplot2::labs(x = "Horizontal x", y = "Horizontal y", z = "Height z")
}

test_that("real ggplot path compiles exactly once and resolves physical dimensions", {
  register_theme_3d()
  grid_test_device({
    calls <- 0L; original <- scene_compile
    testthat::local_mocked_bindings(scene_compile = function(...) { calls <<- calls + 1L; original(...) })
    p <- grid_test_plot()
    g <- grid::grid.force(ggplot2::ggplotGrob(p))
    panels <- grid_test_panels(g)
    expect_length(panels, 1L)
    expect_equal(calls, 1L)
    expect_gt(panels[[1]]$inner_viewport$width_mm, 0)
    expect_s3_class(panels[[1]]$scene, "ggplot3d_compiled_scene")
  })
})

test_that("eight independent azimuths retain panel dimensions and magnification", {
  grid_test_device({
    metrics <- lapply(seq(0, 315, 45), function(angle) {
      panel <- grid_test_panels(grid::grid.force(ggplot2::ggplotGrob(grid_test_plot(azimuth = angle, frame = "sphere"))))[[1]]
      c(unlist(panel$inner_viewport), view_height = panel$scene$camera$view_height)
    })
    for (i in 2:8) expect_equal(metrics[[i]], metrics[[1]], tolerance = 1e-12)
  })
})

test_that("labels and titles remain within panel bounds including facets", {
  grid_test_device({
    p <- grid_test_plot(roll = 25, box = "all") + ggplot2::facet_wrap(~ z, ncol = 2) + ggplot2::theme_minimal()
    panels <- grid_test_panels(grid::grid.force(ggplot2::ggplotGrob(p)))
    expect_length(panels, 2L)
    for (panel in panels) {
      for (bounds in panel$furniture$bounds) {
        expect_true(all(is.finite(bounds)))
        expect_gte(min(bounds[1:2]), 0)
        expect_lte(bounds[3], panel$outer_viewport$width_mm)
        expect_lte(bounds[4], panel$outer_viewport$height_mm)
      }
    }
  })
})

test_that("pole views suppress collapsed axes and small panels fail clearly", {
  grid_test_device({
    p <- grid_test_plot(elevation = 90, panes = character(), box = "none")
    panel <- grid_test_panels(grid::grid.force(ggplot2::ggplotGrob(p)))[[1L]]
    labels <- unlist(lapply(as.list(panel$furniture$children), function(g) if (inherits(g, "text")) as.character(g$label)))
    expect_false("Height z" %in% labels)
    params <- ggplot2::ggplot_build(p)$layout$panel_params[[1]]
    expect_error(panel_furniture_3d(params, ggplot2::theme_get(), new_scene_viewport(2, 2)), "panel too small")
  })
})

test_that("default pole furniture reaches the engine without camera perturbation", {
  grid_test_device({
    expect_no_error(grid::grid.force(ggplot2::ggplotGrob(grid_test_plot(elevation = 90))))
  })
})

test_that("cropped cameras retain axis furniture", {
  grid_test_device({
    expect_no_error(grid::grid.force(ggplot2::ggplotGrob(grid_test_plot(zoom = 2))))
    expect_no_error(grid::grid.force(ggplot2::ggplotGrob(grid_test_plot(zoom = 2, axes = character(), panes = character(), box = "none"))))
  })
})

test_that("theme inheritance, guide precedence and explicit suppression work", {
  register_theme_3d()
  theme <- ggplot2::theme_get() + ggplot2::theme(axis.text = ggplot2::element_text(colour = "red"))
  expect_identical(element_3d("axis.text.z", theme)$colour, "red")
  p <- grid_test_plot() + ggplot2::guides(z = guide_axis_3d(title = "Guide height"), y = "none")
  params <- ggplot2::ggplot_build(p)$layout$panel_params[[1]]
  expect_identical(params$axis_guides$z$title, "Guide height")
  expect_false("y" %in% names(params$axis_guides))
  grid_test_device({
    p <- grid_test_plot(panes = character(), box = "none") + ggplot2::theme_void()
    panel <- grid_test_panels(grid::grid.force(ggplot2::ggplotGrob(p)))[[1L]]
    expect_length(panel$furniture$children, 0L)
  })
})

test_that("rolled long labels use measured reserves and guides retain their options", {
  grid_test_device({
    p <- grid_test_plot(roll = 35) + ggplot2::scale_x_continuous(breaks = c(0, 1), labels = c("a very long lower label", "a very long upper label")) +
      ggplot2::guides(x = guide_axis_3d(n.dodge = 2, check.overlap = TRUE, angle = 20))
    panel <- grid_test_panels(grid::grid.force(ggplot2::ggplotGrob(p)))[[1L]]
    expect_equal(panel$panel_params$axis_guides$x$params$n.dodge, 2L)
    for (bounds in panel$furniture$bounds) {
      expect_gte(min(bounds[1:2]), 0)
      expect_lte(bounds[3], panel$outer_viewport$width_mm)
      expect_lte(bounds[4], panel$outer_viewport$height_mm)
    }
  })
})

test_that("split arrows are restricted to original endpoints", {
  packet <- new_scene_packet(vertices = list(x = c(0, 1), y = c(0.5, 0.5), z = c(0.5, 0.5)),
    corners = list(vertex = 1:2), polylines = list(offset = 1L, arrow_end = TRUE),
    styles = data.frame(style_id = 1L, colour = "black", linewidth = 0.3))
  packet$styles$arrow <- I(list(grid::arrow(type = "closed")))
  grid_test_device({
    scene <- scene_compile(packet, new_camera_3d(), new_scene_viewport(100, 100, 150))
    rendered <- render_scene_grid(scene, scene$viewport)
    expect_s3_class(rendered$children[[1L]], "gTree")
    expect_s3_class(rendered$children[[1L]]$children[[2L]], "polygon")
    scene$fragments$arrow_end[] <- FALSE
    rendered <- render_scene_grid(scene, scene$viewport)
    expect_s3_class(rendered$children[[1L]], "polyline")
  })
})

test_that("surface and points share one real depth scene and backend permutation", {
  grid_test_device({
    surface <- expand.grid(x = c(0, 1), y = c(0, 1)); surface$z <- 0.5
    points <- data.frame(x = c(0.5, 0.5), y = c(0.5, 0.5), z = c(0, 1))
    p <- ggplot2::ggplot(surface, ggplot2::aes(x, y, z = z)) + geom_surface_3d(fill = "steelblue", shade = FALSE) +
      ggplot2::geom_point(data = points, colour = "red", size = 2) +
      coord_3d(elevation = 75, panes = character(), axes = character(), box = "none") + ggplot2::theme_void()
    panel <- grid_test_panels(grid::grid.force(ggplot2::ggplotGrob(p)))[[1L]]
    scene <- panel$scene; f <- scene$fragments
    expect_equal(length(scene$source$faces$primitive_id), 2L)
    expect_equal(length(scene$source$markers$primitive_id), 2L)
    position <- match(seq_along(f$kind), scene$draw_order)
    covering_faces <- function(marker) {
      anchor <- f$offset[marker]
      p <- c(scene$vertices$x_npc[anchor], scene$vertices$y_npc[anchor])
      which(f$kind == 1L & vapply(seq_along(f$kind), function(i) {
        if (f$kind[i] != 1L) return(FALSE)
        ids <- seq.int(f$offset[i], length.out = f$count[i])
        polygon <- cbind(scene$vertices$x_npc[ids], scene$vertices$y_npc[ids])
        next_vertex <- polygon[c(seq.int(2L, nrow(polygon)), 1L), , drop = FALSE]
        cross <- (next_vertex[, 1] - polygon[, 1]) * (p[2] - polygon[, 2]) - (next_vertex[, 2] - polygon[, 2]) * (p[1] - polygon[, 1])
        all(cross >= -1e-12) || all(cross <= 1e-12)
      }, logical(1)))
    }
    for (marker in which(f$kind == 3L)) {
      covered <- covering_faces(marker)
      expect_gt(length(covered), 0)
      if (f$source_row[marker] == 2L) expect_gt(position[marker], max(position[covered]))
      else expect_lt(position[marker], min(position[covered]))
    }
    rendered <- panel$children[[2L]]
    rows <- scene$draw_order
    anchor <- !is.na(scene$styles$.anchor[match(f$style_id, scene$styles$style_id)]) & scene$styles$.anchor[match(f$style_id, scene$styles$style_id)]
    expect_identical(unlist(rendered$fragment_runs, use.names = FALSE), rows[!anchor[rows]])
  })
})

test_that("secondary guides retain independent titles and local themes", {
  p <- grid_test_plot() + ggplot2::scale_x_continuous(sec.axis = ggplot2::dup_axis(
    name = "Scale title", guide = guide_axis_3d(title = "Secondary guide", theme = ggplot2::theme(axis.text.x = ggplot2::element_text(colour = "green")))))
  params <- ggplot2::ggplot_build(p)$layout$panel_params[[1L]]
  expect_identical(params$axis_guides$x.sec$title, "Secondary guide")
  expect_identical(element_3d("axis.text.x", params$axis_guides$x.sec$theme)$colour, "green")
})

test_that("adjacent masks of one translucent glyph share one paint operation", {
  grid_test_device({
    scene <- grid_test_scene()
    scene$fragments <- lapply(scene$fragments, `[`, c(1L, 2L, 2L))
    scene$fragments$fragment_id <- 1:3
    scene$fragments$mask_offset <- c(1L, 1L, 5L)
    scene$fragments$mask_count <- c(0L, 4L, 4L)
    scene$fragments$opacity <- c(1, 0.5, 0.5)
    scene$draw_order <- 1:3
    scene$masks <- list(x_npc = c(0, 0.5, 0.5, 0, 0.5, 1, 1, 0.5),
      y_npc = c(0, 0, 1, 1, 0, 0, 1, 1), ring = rep(1L, 8L))
    result <- render_scene_grid(scene, scene$viewport)
    expect_length(result$children, 2L)
    expect_identical(result$fragment_runs[[2L]], 2:3)
    expect_length(result$children[[2L]]$label, 1L)
    expect_equal(result$children[[2L]]$gp$alpha, 0.5)
  })
})
test_that("frame selection validates and preserves sphere compatibility", {
  expect_error(coord_3d(frame = "unknown"), "arg")
  expect_identical(coord_3d()$config$frame, "view")
  camera <- new_camera_3d()
  viewport <- new_scene_viewport(100, 100, 150)
  expect_identical(frame_camera_3d(camera, viewport, "sphere"), camera)
  expect_gt(frame_camera_3d(camera, viewport, "view")$zoom, camera$zoom)
})

test_that("view frames fit native projected box corners in both projections", {
  corners <- cube_edges_3d()$corners
  packet <- new_scene_packet(vertices = lapply(as.data.frame(corners), as.double),
    corners = list(vertex = 1:8), text = list(offset = 1:8, partition = rep(2L, 8L)),
    styles = data.frame(style_id = 1L))
  for (projection in c("orthographic", "perspective")) {
    for (angle in c(0, 45, 90, 137)) {
      for (dimensions in list(c(100, 100), c(70, 130), c(150, 70))) {
        viewport <- new_scene_viewport(dimensions[1L], dimensions[2L], 150)
        camera <- new_camera_3d(projection, azimuth = angle, roll = 25, aspect = c(2, 1, 0.6))
        fitted <- frame_camera_3d(camera, viewport, "view")
        scene <- scene_compile(packet, fitted, viewport)
        expect_length(scene$fragments$offset, 8L)
        positions <- frame_positions_3d(corners, fitted, frame_basis_3d(fitted, viewport))
        source <- scene$source$text$offset[scene$fragments$source_index]
        expect_equal(unname(positions[source, 1L]), scene$vertices$x_npc, tolerance = 1e-12)
        expect_equal(unname(positions[source, 2L]), scene$vertices$y_npc, tolerance = 1e-12)
        expect_equal(unname(positions[source, 3L]), scene$vertices$depth, tolerance = 1e-12)
        xy <- c(scene$vertices$x_npc, scene$vertices$y_npc)
        expect_true(all(xy >= 0 & xy <= 1))
        expect_gt(max(abs(xy - 0.5)), 0.49)
      }
    }
  }
})

test_that("azimuth frames keep constant magnification and contain a whole turn", {
  viewport <- new_scene_viewport(100, 130, 150)
  for (projection in c("orthographic", "perspective")) {
    cameras <- lapply(seq(0, 345, 15), function(angle) {
      new_camera_3d(projection, azimuth = angle, elevation = 32, roll = 25, aspect = c(2, 1, 0.6))
    })
    zoom <- vapply(cameras, function(camera) frame_camera_3d(camera, viewport, "azimuth")$zoom, numeric(1))
    view <- vapply(cameras, function(camera) frame_camera_3d(camera, viewport, "view")$zoom, numeric(1))
    expect_equal(zoom, rep(zoom[1L], length(zoom)), tolerance = 1e-12)
    expect_true(all(zoom <= view + 1e-12))
  }
})

test_that("view framing reports a box intersecting the eye plane", {
  camera <- new_camera_3d("perspective", distance = 0.1)
  expect_error(frame_camera_3d(camera, new_scene_viewport(100, 100), "view"), "eye plane")
  expect_error(frame_camera_3d(camera, new_scene_viewport(100, 100), "azimuth"), "eye plane")
})

test_that("thinned label rectangles are disjoint and unthinned labels are all kept", {
  locations <- cbind(seq(10, 50, length.out = 9), rep(20, 9))
  size <- cbind(rep(12, 9), rep(3, 9))
  result <- thin_axis_labels_3d(locations, size, c(1, 0),
    matrix(numeric(), 0, 4L), new_scene_viewport(60, 40))
  kept <- result$boxes[result$keep, , drop = FALSE]
  expect_lt(sum(result$keep), 9L)
  expect_gt(sum(result$keep), 1L)
  expect_true(all(head(kept[, 3L], -1L) < tail(kept[, 1L], -1L)))
  # Slanted labels whose projections along the axis overlap but whose
  # rectangles do not are all kept.
  slanted <- cbind(seq(10, 50, length.out = 5), seq(10, 34, length.out = 5))
  expect_true(all(thin_axis_labels_3d(slanted, cbind(rep(20, 5), rep(3, 5)), c(0.86, 0.51),
    matrix(numeric(), 0, 4L), new_scene_viewport(80, 60))$keep))
  expect_true(all(thin_axis_labels_3d(locations, size, c(1, 0), matrix(numeric(), 0, 4L),
    new_scene_viewport(60, 40), thin = FALSE)$keep))
})

test_that("small facets thin tick labels and share a frame with free labels", {
  grid_test_device({
    data <- rbind(data.frame(x = c(0, 1), y = c(0, 1), z = c(0, 1), panel = "A"),
      data.frame(x = c(0, 1), y = c(0, 1e5), z = c(0, 1), panel = "B"))
    p <- ggplot2::ggplot(data, ggplot2::aes(x, y, z = z)) + ggplot2::geom_point() +
      coord_3d() + ggplot2::facet_wrap(~panel, scales = "free_y") + ggplot2::theme_minimal() +
      ggplot2::scale_x_continuous(breaks = seq(0, 1, 0.05))
    panels <- grid_test_panels(grid::grid.force(ggplot2::ggplotGrob(p)))
    expect_length(panels, 2L)
    expect_equal(panels[[1L]]$inner_viewport, panels[[2L]]$inner_viewport)
    expect_equal(panels[[1L]]$scene$camera$view_height, panels[[2L]]$scene$camera$view_height)
    for (panel in panels) {
      expect_lt(sum(panel$furniture$label_layout$x$keep), 21L)
      for (layout in panel$furniture$label_layout) {
        boxes <- layout$occupied
        if (nrow(boxes) > 1L) for (i in seq_len(nrow(boxes) - 1L)) {
          later <- boxes[seq.int(i + 1L, nrow(boxes)), , drop = FALSE]
          expect_false(any(later[, 1L] < boxes[i, 3L] & later[, 3L] > boxes[i, 1L] &
            later[, 2L] < boxes[i, 4L] & later[, 4L] > boxes[i, 2L]))
        }
      }
    }
  }, width = 5, height = 4)
})

test_that("frame groups belong to an individual built gtable", {
  grid_test_device({
    p <- grid_test_plot()
    first <- ggplot2::ggplotGrob(p)
    second <- ggplot2::ggplotGrob(p + ggplot2::facet_wrap(~z))
    a <- grid_test_panels(grid::grid.force(first))
    b <- grid_test_panels(grid::grid.force(second))
    expect_length(as.list(a[[1L]]$frame_group), 1L)
    expect_length(as.list(b[[1L]]$frame_group), 2L)
    expect_false(identical(a[[1L]]$frame_group, b[[1L]]$frame_group))
  })
})
test_that("furniture facing handles poles and finite perspective eyes", {
  viewport <- new_scene_viewport(100, 100)
  for (elevation in c(-90, 90)) {
    camera <- new_camera_3d(elevation = elevation)
    partitions <- furniture_partitions_3d(camera, viewport)
    expect_equal(unname(partitions[1:4]), rep(0L, 4))
    expect_equal(unname(partitions[c("zmin", "zmax")]), if (elevation > 0) c(0L, 2L) else c(2L, 0L))
  }
  camera <- new_camera_3d("perspective", azimuth = 0, elevation = 0, distance = 3)
  expect_equal(unname(furniture_partitions_3d(camera, viewport)), c(0L, 2L, 0L, 0L, 0L, 0L))
  camera <- new_camera_3d("perspective", azimuth = 0, elevation = 0, distance = 3,
    target = c(0.5, 2, 0.5), aspect = c(2, 1, 0.5))
  expect_equal(unname(furniture_partitions_3d(camera, viewport)), c(0L, 2L, 0L, 2L, 0L, 0L))
})

test_that("pane grids and box edges share fixed furniture partitions", {
  grid_test_device({
    for (clip_data in c(TRUE, FALSE)) for (elevation in c(-90, 30, 90)) {
      plot <- grid_test_plot(elevation = elevation, clip_data = clip_data,
        panes = c("xmin", "xmax", "ymin", "ymax", "zmin", "zmax"), box = "all") +
        ggplot2::scale_x_continuous(breaks = c(0.25, 0.75), minor_breaks = NULL) +
        ggplot2::scale_y_continuous(breaks = c(0.25, 0.75), minor_breaks = NULL) +
        scale_z_continuous(breaks = c(0.25, 0.75), minor_breaks = NULL)
      params <- ggplot2::ggplot_build(plot)$layout$panel_params[[1L]]
      furniture <- panel_furniture_3d(params, ggplot2::complete_theme(), new_scene_viewport(150, 150))
      packet <- furniture$packet
      expected <- furniture_partitions_3d(furniture$camera, furniture$viewport)
      expect_equal(packet$faces$partition, if (clip_data) unname(expected) else rep(1L, 6))
      vertices <- as.matrix(as.data.frame(packet$vertices))
      for (i in seq_along(packet$polylines$offset)) {
        ids <- seq.int(packet$polylines$offset[i], length.out = packet$polylines$count[i])
        xyz <- vertices[packet$corners$vertex[ids], , drop = FALSE]
        fixed <- which(xyz[1L, ] == xyz[2L, ] & xyz[1L, ] %in% c(0, 1))
        incident <- paste0(c("x", "y", "z")[fixed], ifelse(xyz[1L, fixed] == 0, "min", "max"))
        id <- packet$polylines$primitive_id[i]
        if (length(fixed) == 1L) {
          face <- max(which(packet$faces$primitive_id < id))
          partition <- packet$faces$partition[face]
        } else {
          partition <- if (!clip_data) 1L else if (any(expected[incident] == 2L)) 2L else 0L
        }
        expect_equal(packet$polylines$partition[i], partition,
          info = paste("elevation", elevation, "clip_data", clip_data, "line", i, "planes", paste(incident, collapse = ",")))
      }
      expect_true(all(diff(packet$polylines$source_row) > 0L))
      expect_true(all(packet$text$partition == 2L))
    }
  })
})

test_that("theme_bw panel border retains its stroke with transparent fill", {
  grid_test_device({
    plot <- grid_test_plot() + ggplot2::theme_bw()
    panel <- grid_test_panels(grid::grid.force(ggplot2::ggplotGrob(plot)))[[1L]]
    border <- panel$children[[length(panel$children)]]
    expect_s3_class(border, "rect")
    expect_true(is.na(border$gp$fill))
    expect_false(is.na(border$gp$col))
    expect_true(any(panel$scene$fragments$layer_id > 0L))
  })
})

test_that("shaded solids paint seam groups of non-overlapping faces over a two-tier underlay", {
  angle <- seq(0, 2 * pi, length.out = 41)[-41]
  disc <- data.frame(x = cos(angle), y = sin(angle), z = 0.3)
  p <- ggplot2::ggplot(disc, ggplot2::aes(x, y, z = z)) + geom_extrude_3d(fill = "#2E6F9E") +
    coord_3d(aspect = c(1, 1, 0.3), elevation = 35)
  grid_test_device({
    panel <- grid_test_panels(grid::grid.force(ggplot2::ggplotGrob(p)))[[1L]]
    f <- panel$scene$fragments; rendered <- panel$children[[2L]]
    faces <- panel$scene$draw_order[f$kind[panel$scene$draw_order] == 1L & f$layer_id[panel$scene$draw_order] > 0L]
    expect_lt(length(unique(f$seam_group[faces])), length(faces) / 4)
    for (run in rendered$fragment_runs) {
      if (all(f$kind[run] == 1L & f$layer_id[run] > 0L)) expect_length(unique(f$seam_group[run]), 1L)
    }
    shaded <- Filter(function(g) inherits(g, "gTree") && length(g$children) == 3L, rendered$children)
    expect_gt(length(shaded), 0L)
    tiers <- shaded[[1L]]$children
    expect_s3_class(tiers[[1L]], "pathgrob"); expect_length(tiers[[1L]]$gp$fill, 1L)
    expect_s3_class(tiers[[2L]], "pathgrob"); expect_gt(length(tiers[[2L]]$gp$fill), 1L)
    expect_s3_class(tiers[[3L]], "polygon")
  })
})

test_that("fragments of one contour line paint as one stroke", {
  bump <- expand.grid(x = seq(-2, 2, length.out = 25), y = seq(-2, 2, length.out = 25))
  bump$z <- with(bump, exp(-(x^2 + y^2)) + 0.3 * exp(-((x - 1)^2 + (y + 1)^2) * 3))
  p <- ggplot2::ggplot(bump, ggplot2::aes(x, y, z = z)) + geom_contour_3d(bins = 5) + coord_3d()
  lines <- ggplot2::layer_data(p)
  expect_true(all(table(lines$group) > 10L))
  grid_test_device({
    panel <- grid_test_panels(grid::grid.force(ggplot2::ggplotGrob(p)))[[1L]]
    f <- panel$scene$fragments; rendered <- panel$children[[2L]]
    contour <- Filter(function(run) all(f$kind[run] == 2L & f$layer_id[run] > 0L), rendered$fragment_runs)
    expect_lte(length(contour), 2L * length(unique(lines$group)))
  })
})
