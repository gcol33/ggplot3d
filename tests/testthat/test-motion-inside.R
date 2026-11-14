test_that("inside animations rotate the observer with a fixed eye", {
  skip_if_not_installed("ragg")
  original <- motion_panel_grob_3d
  captured <- list()
  record <- function(...) {
    panel <- original(...)
    captured[[length(captured) + 1L]] <<- list(camera = panel$panel_params$camera,
      frame = panel$panel_params$coord_params$frame)
    panel
  }
  d <- data.frame(x = c(-.4, .4, 0, 0, .2, -.1), y = c(0, 0, -.4, .4, .1, -.2),
    z = c(.1, -.1, .2, -.2, .4, -.4), colour = seq_len(6))
  testthat::with_mocked_bindings({
    for (coord in list(coord_hyperbolic_3d(inside = TRUE, observer = c(.1, 0, 0)),
                      coord_spherical_geometry_3d(inside = TRUE, observer = c(.1, 0, 0)))) {
      p <- ggplot2::ggplot(d, ggplot2::aes(x, y, z = z, colour = colour)) +
        geom_point_3d(size = 4) + coord + ggplot2::theme_void() + ggplot2::theme(legend.position = "none")
      for (renderer in c("raster", "vector")) for (path in list(NULL, orbit_3d(degrees = 240, duration = 1))) {
        captured <- list()
        animation <- animate_3d(p, path = path, renderer = renderer, frames = 3, width = 140, height = 140, progress = FALSE)
        expect_identical(animation$frame_count, 3L)
        expect_identical(animation$frame, "sphere")
        expect_true(all(file.exists(animation$frames)))
        expect_length(unique(unname(tools::md5sum(animation$frames))), 3L)
        expect_gte(length(captured), 3L)
        for (frame in captured) {
          camera <- frame$camera
          basis <- frame_basis_3d(camera, new_scene_viewport(100, 100))
          eye <- camera$target * basis$scale - camera$distance * basis$forward
          expect_equal(eye, .5 * basis$scale, tolerance = 1e-12)
          expect_identical(frame$frame, "sphere")
        }
        for (extent in animation$panel_extents) expect_identical(extent, animation$panel_extents[[1L]])
        unlink(dirname(animation$frames[1L]), recursive = TRUE)
      }
    }
  }, motion_panel_grob_3d = record)
})
