test_that("arbitrary normals place all glyph vertices in the specified plane", {
  skip_if_not_installed("systemfonts")
  for (aspect in list(c(1, 1, 1), c(3, 1, 2))) {
    s <- text_facing_scene(normal = c(1, 1, 1), up = c(0, 0, 1), angle = 27,
      coord = coord_3d(aspect = aspect))
    glyph <- attr(s$packet, "glyph_mm")
    for (viewport in list(new_scene_viewport(100, 80), new_scene_viewport(160, 120))) {
      resolved <- resolve_glyph_packets_3d(list(s$packet), s$camera, viewport)[[1L]]
      offset <- scene_vertices_xyz_3d(resolved$vertices) - glyph$centre
      expect_equal(drop(offset %*% c(1, 1, 1)), rep(0, nrow(offset)), tolerance = 1e-10)
      scaled <- text_facing_scene(normal = c(1e200, 1e200, 1e200), up = c(0, 0, 1), angle = 27,
        coord = coord_3d(aspect = aspect))
      expect_equal(scaled$packet$vertices, s$packet$vertices, tolerance = 1e-12)
    }
  }
})

test_that("normal and coordinate-plane specifications give equivalent justified glyphs", {
  skip_if_not_installed("systemfonts")
  specs <- list(xy = list(normal = c(0, 0, 1), up = c(0, 1, 0)),
    xz = list(normal = c(0, -1, 0), up = c(0, 0, 1)), yz = list(normal = c(1, 0, 0), up = c(0, 0, 1)))
  for (elevation in c(-30, 30)) for (plane in names(specs)) {
    args <- list(angle = 33, hjust = .2, vjust = .8, coord = coord_3d(elevation = elevation))
    a <- do.call(text_facing_scene, c(list(plane = plane), args))
    b <- do.call(text_facing_scene, c(specs[[plane]], args))
    expect_equal(a$packet$vertices, b$packet$vertices, tolerance = 1e-12)
    expect_identical(a$packet$faces, b$packet$faces)
    expect_identical(a$packet$corners, b$packet$corners)
  }
})

test_that("text orientation specifications reject invalid or ambiguous directions", {
  skip_if_not_installed("systemfonts")
  for (normal in list(c(0, 0, 0), c(1, NA, 1), c(1, Inf, 1), c(1, 2), "xy", numeric())) {
    expect_error(text_facing_scene(normal = normal), "normal must be a finite nonzero numeric triple")
  }
  for (up in list(c(0, 0, 0), c(1, NaN, 1), 1:2)) {
    expect_error(text_facing_scene(normal = c(1, 1, 1), up = up), "up must be")
  }
  expect_error(text_facing_scene(plane = "xy", normal = c(1, 1, 1)), "only one")
  expect_error(text_facing_scene(normal = c(1, 1, 1), facing = "camera_plane"), "only one")
  expect_error(text_facing_scene(plane = "xy", facing = "camera_point"), "only one")
  expect_error(text_facing_scene(facing = "camera"), "facing must be")
  expect_error(geom_text_3d(normal = c(1, 1, 1)), "Unknown 3D layer arguments")
  expect_no_error(text_facing_scene(normal = c(0, 0, 1), up = c(0, 0, 1)))
})

test_that("curved frame normals follow the inverse transpose of the local map", {
  skip_if_not_installed("systemfonts")
  s <- text_facing_scene(normal = c(1, 1, 1), coord = coord_cylindrical_3d(aspect = c(3, 1, 2)))
  glyph <- attr(s$packet, "glyph_mm")
  spec <- glyph$orientation
  centre <- as.matrix(normalise_positions_3d(data.frame(x = c(.3, .7),
    y = c(.4, .6), z = c(.35, .65)), s$params)[c("x", "y", "z")])
  scale <- s$camera$aspect / max(s$camera$aspect)
  for (j in 1:2) {
    i <- c(1L, nrow(glyph$centre))[j]
    phi <- 2 * pi * centre[j, 1L]; radius <- centre[j, 2L] / 2
    jacobian <- cbind(c(2 * pi * radius * cos(phi), -2 * pi * radius * sin(phi), 0),
      c(sin(phi) / 2, cos(phi) / 2, 0), c(0, 0, 1))
    normal <- solve(t(diag(scale) %*% jacobian), c(1, 1, 1))
    expect_equal(sum(normal * spec$u[i, ]), 0, tolerance = 1e-8)
    expect_equal(sum(normal * spec$v[i, ]), 0, tolerance = 1e-8)
  }
})

test_that("camera-facing outlines use the actual eye and panel viewport at draw time", {
  skip_if_not_installed("systemfonts")
  viewport <- new_scene_viewport(140, 80)
  for (projection in c("perspective", "orthographic", "oblique")) {
    coord <- coord_3d(projection = projection, aspect = c(2, 1, 3), azimuth = 35, elevation = 25)
    for (facing in c("camera_plane", "camera_point")) {
      s <- text_facing_scene(facing = facing, angle = 22, coord = coord)
      glyph <- attr(s$packet, "glyph_mm")
      for (azimuth in c(-70, 80)) {
        camera <- s$camera; camera$azimuth <- azimuth
        basis <- frame_basis_3d(camera, viewport)
        resolved <- resolve_glyph_packets_3d(list(s$packet), camera, viewport)[[1L]]
        offset <- sweep(scene_vertices_xyz_3d(resolved$vertices) - glyph$centre, 2L, basis$scale, "*")
        normal <- if (facing == "camera_point") toward_camera_3d(basis, camera, glyph$centre) else
          matrix(if (basis$perspective) -basis$forward else -basis$ray, nrow(offset), 3L, byrow = TRUE)
        expect_equal(rowSums(offset * unit_rows_3d(normal)), rep(0, nrow(offset)), tolerance = 1e-10)
        again <- resolve_glyph_packets_3d(list(resolved), camera, viewport)[[1L]]
        expect_identical(again$vertices, resolved$vertices)
      }
    }
  }
})

test_that("all static backends resolve camera-facing outlines before drawing", {
  skip_if_not_installed("systemfonts")
  skip_if_not_installed("ragg")
  for (type in c("vector", "raster", "hybrid", "pathtrace")) {
    backend <- backend_3d(type, dpi = 50, antialias = 1, samples = 1, bounces = 1)
    s <- text_facing_scene(facing = "camera_point", labels = c("O", "O"),
      coord = coord_3d(backend = backend, projection = "perspective", aspect = c(2, 1, 3)))
    path <- tempfile(fileext = ".png")
    ragg::agg_png(path, width = 160, height = 160, res = 72)
    tryCatch({
      table <- grid::grid.force(ggplot2::ggplotGrob(s$plot))
      panel <- motion_panels_3d(table)[[1L]]
      packet <- panel$packets[[1L]]
      glyph <- attr(packet, "glyph_mm")
      basis <- frame_basis_3d(s$camera, panel$inner_viewport)
      offset <- sweep(scene_vertices_xyz_3d(packet$vertices) - glyph$centre, 2L, basis$scale, "*")
      toward <- unit_rows_3d(toward_camera_3d(basis, s$camera, glyph$centre))
      expect_equal(rowSums(offset * toward), rep(0, nrow(offset)), tolerance = 1e-10)
      expect_false(isTRUE(all.equal(packet$vertices, s$packet$vertices)))
    }, finally = { grDevices::dev.off(); unlink(path) })
  }
})
