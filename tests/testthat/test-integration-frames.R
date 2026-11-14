draw_plot_3d <- function(p) {
  grDevices::pdf(NULL, width = 6, height = 5)
  on.exit(grDevices::dev.off())
  forced <- grid::grid.force(ggplot2::ggplotGrob(p))
  motion_panels_3d(forced)
}

face_packet_3d <- function(xyz, faces) {
  new_scene_packet(vertices = list(x = xyz[, 1L], y = xyz[, 2L], z = xyz[, 3L]),
    corners = list(vertex = as.integer(unlist(faces)), source_row = rep.int(1L, length(unlist(faces)))),
    faces = list(offset = cumsum(c(1L, head(lengths(faces), -1L))), count = lengths(faces)),
    styles = data.frame(style_id = 1L, fill = "grey50"))
}

test_that("the box frame is the identity and leaves emitted packets untouched", {
  frame <- FrameBox
  xyz <- matrix(stats::runif(12), 4L)
  expect_identical(frame$map(xyz), xyz)
  packet <- face_packet_3d(rbind(c(0, 0, 0), c(1, 0, 0), c(0, 1, 0)), list(1:3))
  expect_identical(frame_packet_3d(frame, packet), packet)
  expect_identical(names(furniture_partitions_3d(new_camera_3d(), new_scene_viewport(100, 100))),
    c("xmin", "xmax", "ymin", "ymax", "zmin", "zmax"))
})

test_that("generic subdivision reproduces box furniture visibility under an identity transform", {
  d <- data.frame(x = 1:3, y = c(1, 3, 2), z = c(2, 1, 3))
  for (projection in c("orthographic", "perspective")) {
    plain <- ggplot2::ggplot(d, ggplot2::aes(x, y, z = z)) + geom_point_3d() + coord_3d(projection = projection, azimuth = 35)
    curved <- ggplot2::ggplot(d, ggplot2::aes(x, y, z = z)) + geom_point_3d() +
      coord_3d(projection = projection, azimuth = 35, transform = list(x = "identity"))
    a <- draw_plot_3d(plain)[[1L]]$scene; b <- draw_plot_3d(curved)[[1L]]$scene
    # Curved panes arrive as triangles; strokes and partitions per kind match.
    classes <- function(scene) tapply(scene$fragments$partition, scene$fragments$kind, function(x) sort(unique(x)))
    expect_equal(classes(a), classes(b))
    strokes <- function(scene) table(scene$fragments$partition[scene$fragments$kind == 2L])
    expect_equal(strokes(a), strokes(b))
  }
})

test_that("a coordinate transformation bends lines and expands in transformed space", {
  d <- data.frame(x = c(0, 1), y = c(0, 1), z = c(1, 1000))
  p <- ggplot2::ggplot(d, ggplot2::aes(x, y, z = z)) + geom_path_3d() + coord_3d(transform = list(z = "log10"))
  built <- ggplot2::ggplot_build(p)
  params <- built$layout$panel_params[[1L]]
  ends <- log10(params$z.range)
  expect_equal(diff(ends) / (1.1 * 3), 1, tolerance = 1e-8)
  frame <- params$frame
  expect_false(frame$linear)
  u <- (100 - params$z.range[1L]) / diff(params$z.range)
  expect_equal(unname(frame$map(cbind(0, 0, u))[1L, 3L]), (2 - ends[1L]) / diff(ends))
  packet <- ggplot2::layer_grob(p)[[1L]]$packet
  expect_gt(length(packet$vertices$x), 2L)
  expect_error(suppressWarnings(ggplot2::ggplot_build(ggplot2::ggplot(data.frame(x = 1, y = 1, z = c(-1, 1)), ggplot2::aes(x, y, z = z)) +
    geom_point_3d() + coord_3d(transform = list(z = "log10")))), "undefined")
})

test_that("munching keeps shared edges watertight and within tolerance", {
  frame <- coord_cylindrical_3d()$frame
  xyz <- rbind(c(0, 0.2, 0.1), c(0.7, 0.2, 0.1), c(0.7, 1, 0.9), c(0, 1, 0.9))
  packet <- face_packet_3d(xyz, list(c(1L, 2L, 3L), c(1L, 3L, 4L)))
  munched <- munch_packet_3d(frame, packet)
  model <- scene_vertices_xyz_3d(munched$vertices)
  scale <- attr(munched, "scale")
  parent <- attr(munched, "parent")$faces
  on_diagonal <- function(rows) {
    s <- scale[rows, , drop = FALSE]
    t <- (s[, 1L] - 0) / 0.7
    abs(s[, 2L] - (0.2 + 0.8 * t)) < 1e-12 & abs(s[, 3L] - (0.1 + 0.8 * t)) < 1e-12
  }
  vertices_of <- function(face) unique(munched$corners$vertex[unlist(lapply(which(parent == face), function(k) seq.int(munched$faces$offset[k], length.out = 3L)))])
  first <- vertices_of(1L); second <- vertices_of(2L)
  expect_setequal(first[on_diagonal(first)], second[on_diagonal(second)])
  line <- polyline_model_3d(frame, rbind(c(0, 1, 0), c(1, 1, 0)))
  expect_true(all(abs(sqrt((line$model[, 1L] - 0.5)^2 + (line$model[, 2L] - 0.5)^2) - 0.5) < 1e-12))
  mid <- frame$map((line$scale[-1L, ] + line$scale[-nrow(line$scale), ]) / 2)
  chord <- (line$model[-1L, ] + line$model[-nrow(line$model), ]) / 2
  expect_lt(max(sqrt(rowSums((mid - chord)^2))), frame$tolerance)
})

test_that("curved frames clip in scale space before mapping", {
  frame <- coord_cylindrical_3d()$frame
  packet <- new_scene_packet(vertices = list(x = c(0.2, 0.4), y = c(0.5, 1.6), z = c(0.5, 0.5)),
    corners = list(vertex = 1:2, source_row = c(1L, 1L)), polylines = list(offset = 1L, count = 2L),
    styles = data.frame(style_id = 1L, colour = "black"))
  out <- munch_packet_3d(frame, packet)
  radius <- sqrt((out$vertices$x - 0.5)^2 + (out$vertices$y - 0.5)^2)
  expect_lte(max(radius), 0.5 + 1e-12)
})

test_that("cylindrical and spherical frames place angle, radius and height", {
  cyl <- coord_cylindrical_3d()$frame
  expect_equal(cyl$map(cbind(0.25, 1, 0.3)), cbind(1, 0.5, 0.3))
  expect_equal(cyl$map(cbind(0, 1, 0)), cbind(0.5, 1, 0))
  anti <- coord_cylindrical_3d(direction = -1)$frame
  expect_equal(anti$map(cbind(0.25, 1, 0)), cbind(0, 0.5, 0))
  swapped <- coord_cylindrical_3d(theta = "y", r = "x")$frame
  expect_equal(swapped$map(cbind(1, 0.25, 0.7)), cbind(1, 0.5, 0.7))
  expect_setequal(vapply(cyl$panes(), `[[`, character(1), "name"), c("wall", "floor", "top"))
  expect_length(cyl$edges(), 2L)
  partial <- coord_cylindrical_3d(end = pi, inner.radius = 0.3)$frame
  expect_setequal(vapply(partial$panes(), `[[`, character(1), "name"), c("start", "end", "inner", "wall", "floor", "top"))
  sphere <- coord_spherical_3d()$frame
  expect_equal(sphere$map(cbind(0.3, 1, 1)), cbind(0.5, 0.5, 1))
  expect_equal(sphere$map(cbind(0, 0.5, 1)), cbind(0.5, 1, 0.5))
  expect_identical(vapply(sphere$panes(), `[[`, character(1), "name"), "shell")
  expect_error(coord_cylindrical_3d(panes = "xmin"), "Unknown pane")
  expect_error(coord_cylindrical_3d(theta = "x", r = "x"), "different")
  expect_error(coord_cylindrical_3d(aspect = "data"), "Cartesian")
})

test_that("curved frames draw with rear panes and curved axes", {
  d <- data.frame(month = 1:12, ring = 1, rain = 1:12)
  p <- ggplot2::ggplot(d, ggplot2::aes(month, ring, z = rain)) + geom_path_3d() + coord_cylindrical_3d()
  panel <- draw_plot_3d(p)[[1L]]
  expect_s3_class(panel$scene, "ggplot3d_compiled_scene")
  furniture <- panel_furniture_3d(panel$panel_params, panel$theme, panel$outer_viewport)
  rear <- furniture$packet$faces$partition
  expect_true(length(rear) > 0L && all(rear == 0L))
  g <- ggplot2::ggplot(expand.grid(lon = seq(0, 300, 60), lat = c(-30, 30)), ggplot2::aes(lon, lat, z = 1)) +
    geom_point_3d() + coord_spherical_3d()
  expect_s3_class(draw_plot_3d(g)[[1L]]$scene, "ggplot3d_compiled_scene")
})

test_that("ternary and quaternary frames close parts and map barycentrically", {
  frame <- coord_ternary_3d()$frame
  closed <- frame$setup_aesthetics(data.frame(a = 1, b = 1, c = 2))
  expect_equal(c(closed$x, closed$y), c(0.25, 0.25))
  v <- ternary_vertices_3d()
  expect_equal(frame$map(rbind(c(1, 0, 0.4), c(0, 1, 0.4), c(0, 0, 0.4))), cbind(v, 0.4), ignore_attr = TRUE)
  expect_error(frame$setup_aesthetics(data.frame(a = 1, b = 1)), "needs all")
  expect_error(frame$setup_aesthetics(data.frame(a = -1, b = 1, c = 1)), "nonnegative")
  tetra <- coord_simplex_3d()$frame
  corners <- tetra$map(rbind(c(1, 0, 0), c(0, 1, 0), c(0, 0, 1), c(0, 0, 0)))
  lengths <- as.vector(stats::dist(corners))
  expect_equal(lengths, rep(1, 6))
  expect_true(all(corners >= 0 & corners <= 1))
  soil <- data.frame(sand = c(6, 2, 3), silt = c(3, 5, 2), clay = c(1, 3, 5), yield = c(2, 5, 4))
  p <- ggplot2::ggplot(soil, ggplot2::aes(a = sand, b = silt, c = clay, z = yield)) + geom_point_3d() + coord_ternary_3d()
  built <- ggplot2::ggplot_build(p)
  expect_equal(built$layout$panel_params[[1L]]$x.range, c(0, 1))
  panel <- draw_plot_3d(p)[[1L]]
  expect_setequal(names(panel$panel_params$axis_guides), c("a", "b", "c", "z"))
  expect_identical(panel$panel_params$axis_guides$c$title, "clay")
})

test_that("camera presets resolve to named coord_3d arguments", {
  expect_identical(camera_3d("isometric")$elevation, atan(1 / sqrt(2)) * 180 / pi)
  expect_identical(camera_3d("top", zoom = 2)$zoom, 2)
  expect_identical(camera_3d("cabinet")$projection, "oblique")
  coord <- do.call(coord_3d, camera_3d("cavalier"))
  expect_equal(coord$camera$shear, c(cos(pi / 4), sin(pi / 4)))
  expect_error(camera_3d("nope"))
  expect_error(coord_3d(projection = "oblique", oblique = c(angle = 45, ratio = 0)), "positive")
})

screen_axes_3d <- function(camera) {
  packet <- new_scene_packet(vertices = list(x = c(0, 1, 0, 0), y = c(0, 0, 1, 0), z = c(0, 0, 0, 1)),
    corners = list(vertex = 1:4, source_row = 1:4),
    markers = list(offset = 1:4, footprint_id = rep(1L, 4L)),
    footprints = list(kind = 1L, offset = 1L, count = 0L, radius_x_mm = 0, radius_y_mm = 0, angle = 0),
    styles = data.frame(style_id = 1L))
  camera$clip_data <- FALSE; camera$clip <- FALSE
  scene <- scene_compile(packet, camera, new_scene_viewport(100, 100))
  v <- scene$vertices; f <- scene$fragments
  xy <- cbind(v$x_camera, v$y_camera)[f$offset[order(f$source_row)], ]
  sweep(xy[2:4, ], 2L, xy[1L, ])
}

test_that("isometric, dimetric and oblique views foreshorten as specified", {
  lengths <- function(preset) {
    args <- camera_3d(preset)
    camera <- new_camera_3d(args$projection, args$azimuth, args$elevation, args$roll,
      shear = oblique_shear_3d(args$projection, args$oblique %||% c(45, 1)))
    sqrt(rowSums(screen_axes_3d(camera)^2))
  }
  expect_equal(lengths("isometric"), rep(lengths("isometric")[1L], 3L))
  dimetric <- lengths("dimetric")
  expect_equal(dimetric / dimetric[3L], c(0.5, 1, 1))
  cabinet <- screen_axes_3d(new_camera_3d("oblique", -90, 0, shear = oblique_shear_3d("oblique", c(angle = 45, ratio = 0.5))))
  expect_equal(cabinet[1L, ], c(1, 0))
  expect_equal(cabinet[3L, ], c(0, 1))
  expect_equal(cabinet[2L, ], 0.5 * c(cos(pi / 4), sin(pi / 4)))
  expect_error(new_camera_3d("perspective", shear = c(0.1, 0)), "oblique")
})

test_that("R framing and the engine agree on oblique and rotated-up cameras", {
  for (camera in list(new_camera_3d("oblique", -70, 15, 10, shear = c(0.3, -0.2)),
                      new_camera_3d(azimuth = 20, elevation = 40, up = 2L),
                      new_camera_3d("perspective", azimuth = 20, elevation = 40, up = 1L))) {
    viewport <- new_scene_viewport(100, 100)
    xyz <- as.matrix(expand.grid(0:1, 0:1, 0:1))
    packet <- new_scene_packet(vertices = list(x = xyz[, 1L], y = xyz[, 2L], z = xyz[, 3L]),
      corners = list(vertex = 1:8, source_row = 1:8), markers = list(offset = 1:8, footprint_id = rep(1L, 8L)),
      footprints = list(kind = 1L, offset = 1L, count = 0L, radius_x_mm = 0, radius_y_mm = 0, angle = 0),
      styles = data.frame(style_id = 1L))
    scene <- scene_compile(packet, camera, viewport)
    f <- scene$fragments; rows <- f$offset[order(f$source_row)]
    engine <- cbind(scene$vertices$x_npc[rows], scene$vertices$y_npc[rows])
    r <- frame_positions_3d(xyz, camera, frame_basis_3d(camera, viewport))[, 1:2]
    expect_equal(unname(engine), unname(r), tolerance = 1e-10)
  }
  vertical <- frame_basis_3d(new_camera_3d(elevation = 90, up = 2L), new_scene_viewport(100, 100))
  expect_equal(vertical$forward, c(0, -1, 0))
})

test_that("oblique scenes keep exact occlusion", {
  packet <- new_scene_packet(vertices = list(x = c(0.2, 0.8, 0.8, 0.2, 0.2, 0.8, 0.8, 0.2), y = c(0.2, 0.2, 0.2, 0.2, 0.7, 0.7, 0.7, 0.7),
      z = c(0.2, 0.2, 0.8, 0.8, 0.2, 0.2, 0.8, 0.8)),
    corners = list(vertex = 1:8, source_row = rep(1:2, each = 4L)),
    faces = list(offset = c(1L, 5L), count = c(4L, 4L), source_row = 1:2),
    styles = data.frame(style_id = 1L, fill = "grey"))
  scene <- scene_compile(packet, new_camera_3d("oblique", -90, 0, shear = c(0.3, 0.3)), new_scene_viewport(100, 100))
  order <- scene$fragments$source_row[scene$draw_order]
  expect_identical(order[length(order)], 1L)
  expect_identical(order[1L], 2L)
})

test_that("the viewer receives frame-generic furniture and axis candidates", {
  skip_if_not_installed("htmlwidgets")
  d <- data.frame(month = 1:12, ring = 1, rain = 1:12)
  p <- ggplot2::ggplot(d, ggplot2::aes(month, ring, z = rain)) + geom_point_3d() + coord_cylindrical_3d()
  w <- view_3d(p, panel = 1)
  expect_identical(w$x$schema_version, 4L)
  view <- w$x$panels[[1L]]
  tagged <- Filter(function(m) !is.null(m$frame), view$furniture$materials)
  expect_gt(length(tagged), 0L)
  expect_true(all(vapply(tagged, function(m) all(lengths(m$frame$normals) == 3L), logical(1))))
  expect_true(all(vapply(view$axes, function(a) length(a$candidates$options) >= 1L, logical(1))))
  expect_identical(unlist(view$framing$back), "wall")
})

test_that("log-ratio statistics stay inside the simplex", {
  x <- matrix(c(0.1, 0.2, 0.3, 0.4, 0.25, 0.25, 0.25, 0.25), 2L, byrow = TRUE)
  expect_equal(ilr_inverse_3d(ilr_3d(x), 4L), x)
  expect_equal(crossprod(ilr_basis_3d(4L)), diag(3))
  set.seed(5)
  g <- exp(matrix(stats::rnorm(400, sd = 0.5), ncol = 4))
  comp <- data.frame(a = g[, 1], b = g[, 2], c = g[, 3], d = g[, 4])
  p <- ggplot2::ggplot(comp, ggplot2::aes(a = a, b = b, c = c, d = d)) +
    stat_density_3d(space = "ilr", levels = probs(0.5), n = 12) + coord_simplex_3d()
  out <- ggplot2::layer_data(p)
  expect_gt(nrow(out), 0L)
  expect_true(all(out$x > 0 & out$y > 0 & out$z > 0 & out$x + out$y + out$z < 1))
  hull <- ggplot2::layer_data(ggplot2::ggplot(comp, ggplot2::aes(a = a, b = b, c = c, d = d)) +
    stat_hull_3d(space = "ilr") + coord_simplex_3d())
  expect_true(all(hull$.vertex %in% seq_len(nrow(comp))))
  expect_error(ggplot2::layer_data(ggplot2::ggplot(data.frame(x = 1:4, y = 1:4, z = 1:4), ggplot2::aes(x, y, z = z)) +
    stat_hull_3d(space = "ilr") + coord_3d()), "composition parts")
})

test_that("spherical graticule shells follow the radius breaks", {
  g <- expand.grid(lon = c(0, 120, 240), lat = c(-30, 30), r = c(1, 2, 3))
  p <- ggplot2::ggplot(g, ggplot2::aes(lon, lat, z = r)) + geom_point_3d() + coord_spherical_3d(shells = "breaks")
  params <- ggplot2::ggplot_build(p)$layout$panel_params[[1L]]
  inner <- grep("^shell[0-9]+$", vapply(params$frame$panes(), `[[`, character(1), "name"), value = TRUE)
  breaks <- params$z$break_positions()
  expect_length(inner, sum(is.finite(breaks) & breaks > 0 & breaks < 1))
  expect_true(all(inner %in% params$frame$back_panes(stats::setNames(integer(length(inner)), inner))))
})
