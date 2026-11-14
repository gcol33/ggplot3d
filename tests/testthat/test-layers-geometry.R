geometry_scene <- function(plot) {
  grDevices::pdf(tempfile(fileext = ".pdf"))
  on.exit(grDevices::dev.off(), add = TRUE)
  built <- ggplot2::ggplot_build(plot)
  grobs <- lapply(seq_along(built$plot@layers), function(i) built$plot@layers[[i]]$draw_geom(built$data[[i]], built$layout)[[1L]])
  packets <- collect_scene_layers(grobs, built$layout$panel_params[[1L]], built$plot@theme)$packets
  list(packet = merge_scene_packets(packets, 1L), built = built, params = built$layout$panel_params[[1L]])
}

geometry_plot <- function(data, layer, mapping = ggplot2::aes(x, y, z = z)) {
  ggplot2::ggplot(data, mapping) + layer + coord_3d(expand = FALSE)
}

test_that("columns emit outward closed prisms with original row provenance", {
  d <- data.frame(x = c(1, 3), y = 1, z = c(2, -3))
  scene <- geometry_scene(geometry_plot(d, geom_col_3d(colour = "black", width = 1, depth = 0.5)))
  p <- scene$packet; b <- scene$built$data[[1L]]
  expect_equal(b$xmax - b$xmin, c(1, 1)); expect_equal(b$ymax - b$ymin, c(0.5, 0.5))
  expect_length(p$faces$primitive_id, 12L); expect_length(p$polylines$primitive_id, 24L)
  expect_true(all(p$faces$count == 4L))
  expect_equal(sort(unique(p$corners$source_row)), 1:2)
  expect_true(all(is.na(p$styles$colour[p$faces$style_id])))
  expect_true(all(!is.na(p$styles$colour[p$polylines$style_id])))
  xyz <- cbind(p$vertices$x, p$vertices$y, p$vertices$z)
  for (i in seq_along(p$faces$primitive_id)) {
    ids <- p$corners$vertex[seq.int(p$faces$offset[i], length.out = 3L)]
    t <- xyz[ids, ]; a <- t[2, ] - t[1, ]; b <- t[3, ] - t[1, ]
    normal <- c(a[2]*b[3]-a[3]*b[2], a[3]*b[1]-a[1]*b[3], a[1]*b[2]-a[2]*b[1])
    source <- p$faces$source_row[i]
    centre <- colMeans(xyz[p$corners$vertex[p$corners$source_row == source], , drop = FALSE])
    expect_gt(sum(normal * (colMeans(t) - centre)), 0)
  }
  expect_silent(validate_scene_packet(p))
})

test_that("columns use mapped bounds, transformed baselines and panel spacing", {
  d <- data.frame(x = c(1, 4), y = 1, z = c(10, 100), low = c(1, 10))
  s <- geometry_scene(geometry_plot(d, geom_col_3d(ggplot2::aes(zmin = low))) + scale_z_log10())
  expect_equal(s$built$data[[1L]]$zmin, c(0, 1))
  expect_equal(s$built$data[[1L]]$xmax - s$built$data[[1L]]$xmin, c(2.7, 2.7))
  s <- geometry_scene(geometry_plot(d, geom_col_3d(base = 1)) + scale_z_log10())
  expect_equal(s$built$data[[1L]]$zmin, c(0, 0))
  p <- ggplot2::ggplot(d, ggplot2::aes(x, y, zmax = z, zmin = low)) + geom_col_3d() + coord_3d()
  expect_equal(ggplot2::ggplot_build(p)$data[[1L]]$z, d$z)
  expect_error(ggplot2::ggplot_build(geometry_plot(rbind(d, d[1, ]), geom_col_3d())), "unique")
  expect_error(geom_col_3d(wdith = 1), "Unknown")
  expect_error(ggplot2::ggplot_build(geometry_plot(d, geom_col_3d(position = "stack"))), "Unsupported")
})

test_that("counts aggregate weights by explicit lattice keys and stack signed heights", {
  d <- data.frame(x = c("a", "a", "b", "a"), y = "c", w = c(1, 2, 4, 3))
  p <- ggplot2::ggplot(d, ggplot2::aes(x, y, weight = w)) + geom_bar_3d() + coord_3d()
  s <- geometry_scene(p)
  expect_equal(s$built$data[[1L]]$count, c(6, 4)); expect_equal(s$built$data[[1L]]$z, c(6, 4))
  expect_length(s$packet$faces$primitive_id, 12L)
  p <- ggplot2::ggplot(d, ggplot2::aes(x, y)) + stat_count_3d() + coord_3d()
  expect_equal(sort(ggplot2::ggplot_build(p)$data[[1L]]$count), c(1, 3))
  d <- data.frame(x = 1, y = 1, z = c(2, 3, -2, -4), part = letters[1:4])
  p <- geometry_plot(d, geom_col_3d(position = position_stack_3d()), ggplot2::aes(x, y, z = z, fill = part))
  b <- geometry_scene(p)$built$data[[1L]]
  expect_equal(b$zmin, c(3, 0, -4, 0)); expect_equal(b$zmax, c(5, 3, -6, -4))
  multi <- expand.grid(x = 1:3, y = 1:3, part = c("a", "b")); multi$z <- 1
  built <- geometry_scene(geometry_plot(multi, geom_col_3d(position = position_stack_3d()), ggplot2::aes(x, y, z = z, fill = part)))$built$data[[1L]]
  expect_equal(sort(built$zmax), rep(1:2, each = 9L))
  expect_error(suppressWarnings(ggplot2::ggplot_build(p + scale_z_log10())), "linear")
  expect_error(ggplot2::ggplot_build(geometry_plot(d, geom_col_3d(base = 1, position = position_stack_3d()))), "unique|zero")
})

test_that("nudge shifts all endpoint and bound families once", {
  d <- data.frame(x = 1, y = 2, z = 3)
  p <- geometry_plot(d, geom_segment_3d(zend = 0, position = position_nudge_3d(1, 2, nudge_z = 3)))
  b <- geometry_scene(p)$built$data[[1L]]
  expect_equal(b[c("x", "y", "z", "xend", "yend", "zend")], data.frame(x = 2, y = 4, z = 6, xend = 2, yend = 4, zend = 3))
  expect_error(position_nudge_3d(z = 1, nudge_z = 2), "only one")
  grid <- expand.grid(x = 0:1, y = 0:1); grid$z <- 1
  p <- geometry_plot(grid, geom_surface_3d(position = position_nudge_3d(z = 2)))
  expect_equal(geometry_scene(p)$built$data[[1L]]$z, rep(3, 6L))
})

test_that("mesh adapters join attributes by keys and retain original connectivity", {
  v <- data.frame(.vertex = c(3L, 1L, 4L, 2L), x = c(1, 0, 0, 1), y = c(1, 0, 1, 0), z = 0)
  a <- data.frame(.vertex = c(4L, 3L, 2L, 1L), value = c(4, 3, 2, 1))
  m <- mesh_3d(v, matrix(c(1L, 2L, 3L, 4L), 1L), vertex_data = a)
  expect_equal(m$value, 1:4); expect_equal(m$x, c(0, 1, 1, 0))
  s <- geometry_scene(ggplot2::ggplot() + geom_mesh_3d(ggplot2::aes(fill = value), data = m) + coord_3d())
  expect_equal(s$built$data[[1L]]$fill, rep(s$built$data[[1L]]$fill[1L], 4))
  expect_length(s$packet$faces$primitive_id, 2L)
  expect_equal(sort(unique(s$packet$corners$source_row)), 1:4)
  p <- ggplot2::ggplot() + geom_mesh_3d(data = m, wireframe = TRUE) + coord_3d()
  expect_length(geometry_scene(p)$packet$polylines$primitive_id, 4L)
  m <- mesh_3d(v, matrix(c(1L, 2L, 3L, 1L, 3L, 4L), 2L, byrow = TRUE),
    face_data = data.frame(.face = c(2L, 1L), value = c(9, 7)))
  expect_equal(m$value, rep(c(7, 9), each = 3L))
  p <- ggplot2::ggplot() + geom_mesh_3d(data = m, wireframe = TRUE) + coord_3d()
  expect_length(geometry_scene(p)$packet$polylines$primitive_id, 5L)
  expect_error(mesh_3d(v, matrix(c(1, 2, 8), 1)), "missing vertices")
  expect_error(mesh_3d(v, matrix(c(1, 2, 3), 1), vertex_data = rbind(a, a[1, ])), "unique")
  p <- ggplot2::ggplot() + geom_mesh_3d(data = v, faces = matrix(1:4, 1)) + coord_3d()
  expect_length(geometry_scene(p)$packet$faces$primitive_id, 2L)
  p <- ggplot2::ggplot() + geom_mesh_3d(data = v, faces = matrix(1:4, 1), colour = "black") + coord_3d()
  s <- geometry_scene(p)
  expect_length(s$packet$faces$primitive_id, 2L)
  expect_length(s$packet$polylines$primitive_id, 4L)
})

test_that("rgl homogeneous triangles and quads fortify with material provenance", {
  skip_if_not_installed("rgl")
  v <- rbind(c(0, 2, 2, 0), c(0, 0, 2, 2), c(0, 0, 0, 0), rep(2, 4))
  m <- rgl::mesh3d(vertices = v, quads = matrix(1:4, 4))
  f <- fortify.mesh3d(m)
  expect_equal(f$x, c(0, 1, 1, 0)); expect_equal(f$.vertex, 1:4)
  s <- geometry_scene(ggplot2::ggplot() + geom_mesh_3d(data = m) + coord_3d())
  expect_length(s$packet$faces$primitive_id, 2L)
  m$vb[4L, 1L] <- 0
  expect_error(fortify.mesh3d(m), "nonzero w")
  m$vb[4L, 1L] <- 2
  m$material$texture <- "texture.png"
  m$texcoords <- rbind(c(0, 1, 1, 0), c(0, 0, 1, 1))
  f <- fortify.mesh3d(m)
  expect_equal(f$.texture_u, c(0, 1, 1, 0)); expect_equal(f$.texture_v, c(0, 0, 1, 1))
})

test_that("extrusions preserve hole area, cap choice and boundary wall strokes", {
  d <- data.frame(x = c(0, 4, 4, 0, 1, 1, 3, 3), y = c(0, 0, 4, 4, 1, 3, 3, 1), z = 2,
    ring = rep(1:2, each = 4))
  p <- geometry_plot(d, geom_extrude_3d(colour = "black"), ggplot2::aes(x, y, z = z, subgroup = ring))
  s <- geometry_scene(p); q <- s$packet
  expect_length(q$faces$primitive_id, 24L); expect_length(q$polylines$primitive_id, 24L)
  for (cap in c("top", "bottom", "none")) {
    s <- geometry_scene(geometry_plot(d, geom_extrude_3d(cap = cap), ggplot2::aes(x, y, z = z, subgroup = ring)))
    expect_length(s$packet$faces$primitive_id, if (cap == "none") 8L else 16L)
  }
  d$z <- -2
  expect_silent(validate_scene_packet(geometry_scene(geometry_plot(d, geom_extrude_3d(), ggplot2::aes(x, y, z = z, subgroup = ring)))$packet))
  d$z[1L] <- NA
  expect_warning(s <- geometry_scene(geometry_plot(d, geom_extrude_3d(), ggplot2::aes(x, y, z = z, subgroup = ring))), "Removed")
  expect_length(s$packet$faces$primitive_id, 0L)
})

test_that("sf polygons, multipolygons and polygon Z meshes retain feature identities", {
  skip_if_not_installed("sf")
  outer <- rbind(c(0, 0), c(4, 0), c(4, 4), c(0, 4), c(0, 0))
  hole <- rbind(c(1, 1), c(1, 3), c(3, 3), c(3, 1), c(1, 1))
  sf <- sf::st_sf(height = c(2, -1), geometry = sf::st_sfc(sf::st_polygon(list(outer, hole)),
    sf::st_multipolygon(list(list(outer + 6), list(outer + 12))), crs = 3857))
  p <- ggplot2::ggplot(sf) + geom_extrude_3d(ggplot2::aes(height = height), colour = "black") + coord_3d(crs = 3857)
  s <- geometry_scene(p)
  expect_equal(sort(unique(s$packet$corners$source_row)), 1:2)
  expect_length(s$packet$faces$primitive_id, 40L)
  expect_equal(sort(unique(s$built$data[[1L]]$z)), c(-1, 2))
  geographic <- sf::st_transform(sf, 4326)
  expect_error(ggplot2::ggplot_build(ggplot2::ggplot(geographic) + geom_extrude_3d(ggplot2::aes(z = height)) + coord_3d()), "projected")
  s <- geometry_scene(ggplot2::ggplot(geographic) + geom_extrude_3d(ggplot2::aes(z = height)) + coord_3d(crs = 3857))
  expect_equal(sort(unique(s$packet$corners$source_row)), 1:2)
  xyz <- cbind(outer, c(0, 0, 1, 1, 0))
  m <- sf::st_sf(geometry = sf::st_sfc(sf::st_polygon(list(xyz)), crs = 3857))
  s <- geometry_scene(ggplot2::ggplot() + geom_mesh_3d(data = m) + coord_3d())
  expect_length(s$packet$faces$primitive_id, 2L)
  tin <- sf::st_sfc(structure(list(list(xyz[c(1, 2, 3, 1), ]), list(xyz[c(1, 3, 4, 1), ])),
    class = c("XYZ", "TIN", "sfg")), crs = 3857)
  s <- geometry_scene(ggplot2::ggplot() + geom_mesh_3d(data = tin) + coord_3d())
  expect_length(s$packet$faces$primitive_id, 2L)
})

test_that("bands join paired boundaries and break at missing endpoints", {
  d <- data.frame(x = 1:5, y = 0, zmin = 1:5, zmax = 2:6)
  p <- geometry_plot(d, geom_band_3d(colour = "black"), ggplot2::aes(x, y, zmin = zmin, zmax = zmax))
  s <- geometry_scene(p)
  expect_length(s$packet$faces$primitive_id, 8L)
  expect_equal(sort(unique(s$packet$corners$source_row)), 1:5)
  d$zmax[3L] <- NA; p$data <- d
  expect_warning(s <- geometry_scene(p), "Removed")
  expect_length(s$packet$faces$primitive_id, 4L)
  expect_false(3L %in% s$packet$corners$source_row)
  collapsed <- data.frame(x = 0:2, y = 0, zmin = 0, zmax = c(1, 0, 1))
  s <- geometry_scene(geometry_plot(collapsed, geom_band_3d(), ggplot2::aes(x, y, zmin = zmin, zmax = zmax)))
  expect_length(s$packet$faces$primitive_id, 2L)
})

test_that("contours use shared triangle interpolation and plane overrides", {
  d <- expand.grid(x = 0:2, y = 0:2); d$z <- d$x + d$y
  p <- geometry_plot(d, geom_contour_3d(breaks = c(1.2, 2.4)))
  s <- geometry_scene(p); b <- s$built$data[[1L]]
  expect_equal(b$z, b$level); expect_equal(b$x + b$y, b$z)
  expect_equal(s$params$ranges$z, c(0, 4))
  expect_true(all(s$packet$corners$source_row %in% 1:9))
  p <- geometry_plot(d, geom_contour_3d(breaks = 2.4, plane = "floor"))
  expect_true(all(geometry_scene(p)$packet$vertices$z == 0))
  p <- geometry_plot(d, geom_contour_3d(breaks = 2.4, plane = 1))
  expect_equal(unique(geometry_scene(p)$packet$vertices$z), 0.25)
  d$z <- 10^d$z
  p <- geometry_plot(d, geom_contour_3d(breaks = 2.4, plane = 10)) + scale_z_log10()
  expect_equal(unique(geometry_scene(p)$packet$vertices$z), 0.25)
  expect_error(geom_contour_3d(plane = "ceiling"), "arg")
  expect_error(contour_levels_3d(c(0, 1), NULL, 2, 0.1), "bins or binwidth")
})

test_that("filled contour polygons follow surface triangles with band attributes", {
  d <- expand.grid(x = 0:2, y = 0:2); d$z <- d$x + d$y
  p <- geometry_plot(d, geom_contour_filled_3d(breaks = c(1, 2, 3)))
  s <- geometry_scene(p); b <- s$built$data[[1L]]
  expect_true(all(b$z >= b$level_low - 1e-12 & b$z <= b$level_high + 1e-12))
  expect_equal(b$x + b$y, b$z)
  expect_gt(length(s$packet$faces$primitive_id), 8L)
  expect_silent(validate_scene_packet(s$packet))
})

test_that("contour groups stay separate and saddle contours follow the surface diagonal", {
  d <- expand.grid(x = 0:1, y = 0:1); d$z <- d$x * d$y
  s <- geometry_scene(geometry_plot(d, geom_contour_3d(breaks = 0.4)))
  b <- s$built$data[[1L]]
  expect_equal(b$z, rep(0.4, nrow(b)))
  expect_equal(pmin(b$x, b$y), b$z)
  d <- rbind(transform(d, part = "a"), transform(d, y = y + 3, part = "b"))
  p <- geometry_plot(d, geom_contour_3d(breaks = 0.4), ggplot2::aes(x, y, z = z, colour = part))
  b <- geometry_scene(p)$built$data[[1L]]
  expect_true(all(vapply(split(b$colour, b$group), function(v) length(unique(v)) == 1L, logical(1))))
  p <- geometry_plot(d, geom_contour_3d(breaks = 0.4, plane = -1), ggplot2::aes(x, y, z = z, group = part))
  s <- geometry_scene(p)
  expect_equal(s$params$ranges$z, c(-1, 1))
  expect_true(all(s$packet$vertices$z == 0))
})

test_that("negative extrusions select endpoint caps and zero heights emit no degenerate faces", {
  d <- data.frame(x = c(0, 1, 1, 0), y = c(0, 0, 1, 1), z = -2)
  p <- geometry_plot(d, geom_extrude_3d(cap = "top"))
  s <- geometry_scene(p); q <- s$packet
  levels <- lapply(seq_along(q$faces$count), function(i) {
    corners <- seq.int(q$faces$offset[i], length.out = q$faces$count[i])
    unique(q$vertices$z[q$corners$vertex[corners]])
  })
  expect_equal(unlist(levels[lengths(levels) == 1L], use.names = FALSE), 0)
  d$z <- 0
  expect_length(geometry_scene(geometry_plot(d, geom_extrude_3d()))$packet$faces$primitive_id, 0L)
  expect_length(geometry_scene(geometry_plot(d[1L, ], geom_col_3d()))$packet$faces$primitive_id, 0L)
})

test_that("fixed interval and height aesthetics are prepared before scale transformation", {
  d <- data.frame(x = c(0, 1, 1, 0), y = c(0, 0, 1, 1))
  p <- ggplot2::ggplot(d, ggplot2::aes(x, y)) + geom_extrude_3d(height = 2, zmin = 3) + coord_3d()
  b <- geometry_scene(p)$built$data[[1L]]
  expect_equal(b$z, rep(5, 4)); expect_equal(b$zmin, rep(3, 4))
  p <- ggplot2::ggplot(d[1L, ], ggplot2::aes(x, y)) + geom_col_3d(zmax = 10, zmin = 1) + coord_3d() + scale_z_log10()
  b <- geometry_scene(p)$built$data[[1L]]
  expect_equal(b$z, 1); expect_equal(b$zmin, 0)
  p <- ggplot2::ggplot(d[1:2, ], ggplot2::aes(x, y)) + geom_band_3d(zmin = 1, zmax = 10) + coord_3d() + scale_z_log10()
  b <- geometry_scene(p)$built$data[[1L]]
  expect_equal(b$z, rep(0, 2)); expect_equal(b$zend, rep(1, 2))
  expect_length(geometry_scene(p)$packet$faces$primitive_id, 2L)
})

test_that("droplines end on each normalized box face", {
  d <- data.frame(x = 1:2, y = 1:2, z = 1:2)
  for (face in c("xmin", "xmax", "ymin", "ymax", "zmin", "zmax")) {
    s <- geometry_scene(geometry_plot(d, geom_dropline_3d(face = face)))
    p <- s$packet
    endpoints <- p$corners$vertex[p$polylines$offset + 1L]
    expect_equal(p$vertices[[substr(face, 1L, 1L)]][endpoints], rep(if (endsWith(face, "min")) 0 else 1, 2))
    expect_equal(p$polylines$source_row, 1:2)
  }
})

test_that("new geometry layers render through the common scene backend", {
  skip_if_not_installed("ragg")
  d <- data.frame(x = c(0, 1, 1, 0), y = c(0, 0, 1, 1), z = c(0, 1, 2, 1))
  plots <- list(
    geometry_plot(d, geom_col_3d()),
    ggplot2::ggplot(d, ggplot2::aes(x, y)) + geom_bar_3d() + coord_3d(),
    ggplot2::ggplot() + geom_mesh_3d(vertices = d, faces = matrix(1:4, 1)) + coord_3d(),
    geometry_plot(transform(d, z = 1), geom_extrude_3d()),
    geometry_plot(d, geom_band_3d(ggplot2::aes(zend = z + 0.2))),
    geometry_plot(d, geom_contour_3d(breaks = 0.8)),
    geometry_plot(d, geom_contour_filled_3d(breaks = 0.8)),
    geometry_plot(d, geom_dropline_3d()))
  for (p in plots) {
    file <- tempfile(fileext = ".png")
    expect_silent(ggplot2::ggsave(file, p, device = ragg::agg_png, width = 3, height = 3, dpi = 72))
    expect_gt(file.info(file)$size, 1000)
    unlink(file)
  }
})
