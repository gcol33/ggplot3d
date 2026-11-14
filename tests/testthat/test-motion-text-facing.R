test_that("viewer outline buffers follow camera movement and match static geometry", {
  skip_if_not_installed("htmlwidgets")
  skip_if_not_installed("systemfonts")
  for (facing in c("camera_plane", "camera_point")) {
    s <- text_facing_scene(facing = facing, angle = 17,
      coord = coord_3d(projection = "perspective", aspect = c(2, 1, 3)))
    w <- view_3d(s$plot, panel = 1, width = 420, height = 360, elementId = "text-follow")
    expect_identical(w$x$schema_version, 4L)
    expect_length(w$x$panels[[1L]]$glyphs, 1L)
    b <- viewer_browser(list(w))
    tryCatch({
      b$eval(paste0("window.uploaded = []; const el = document.getElementById('text-follow');",
        "const gl = el.querySelector('canvas').getContext('webgl2');",
        "const upload = gl.bufferSubData.bind(gl); gl.bufferSubData = (target, offset, bytes) => {",
        "if (bytes instanceof Uint8Array) { const d = new DataView(bytes.buffer, bytes.byteOffset, bytes.byteLength);",
        "for (let i=0; i<bytes.length; i+=36) uploaded.push([0,4,8].map(k => d.getFloat32(i+k, true))); }",
        "return upload(target, offset, bytes); };"))
      for (azimuth in c(-25, 75)) {
        actual <- b$eval(sprintf(paste0("uploaded = []; el.ggplot3d.setCamera({azimuth: %f});",
          "el.ggplot3d.redraw(); uploaded.slice();"), azimuth))
        actual <- matrix(unlist(actual), ncol = 3L, byrow = TRUE)
        camera <- s$camera
        camera$azimuth <- azimuth
        camera$zoom <- camera$zoom * w$x$panels[[1L]]$framing$zoom_scale
        vp <- w$x$panels[[1L]]$viewport
        viewport <- new_scene_viewport(vp$width_mm, vp$height_mm, vp$dpi)
        packet <- resolve_glyph_packets_3d(list(s$packet), camera, viewport)[[1L]]
        xyz <- do.call(cbind, packet$vertices)[packet$corners$vertex, , drop = FALSE]
        expect_equal(unname(actual), unname(xyz), tolerance = 2e-6)
        hits <- viewer_hits(b, "text-follow", step = 4)
        expect_gt(length(hits), 0L)
        expect_setequal(unique(vapply(hits, `[[`, integer(1), "layer")), 1L)
      }
      b$session$Emulation$setDeviceMetricsOverride(width = 640, height = 480, deviceScaleFactor = 1, mobile = FALSE)
      b$eval("HTMLWidgets.find('#text-follow').resize(560, 440); el.ggplot3d.redraw();")
      expect_gt(length(viewer_hits(b, "text-follow", step = 5)), 0L)
      expect_equal(b$eval("gl.getError()"), 0)
    }, finally = b$close())
  }
})

test_that("facetted outline labels keep independent camera metadata and surface occlusion", {
  skip_if_not_installed("htmlwidgets")
  skip_if_not_installed("systemfonts")
  d <- data.frame(x = .5, y = .5, z = c(.3, .7), label = c("Behind", "Front"), facet = c("a", "b"))
  sheet <- data.frame(x = c(0, 1, 1, 0), y = c(0, 0, 1, 1), z = .5)
  p <- ggplot2::ggplot(d, ggplot2::aes(x, y, z = z)) +
    geom_text_3d(ggplot2::aes(label = label, data_id = label), orientation = "plane",
      facing = "camera_point", size = 12) +
    geom_polygon_3d(data = sheet, fill = "steelblue", colour = NA) +
    ggplot2::lims(x = c(0, 1), y = c(0, 1)) + zlim(0, 1) +
    ggplot2::facet_wrap(~facet) + coord_3d(azimuth = c(-60, 30), elevation = 90, panes = character())
  w <- view_3d(p, width = 720, height = 360, elementId = "facets-text", link = FALSE)
  expect_length(w$x$panels, 2L)
  expect_equal(vapply(w$x$panels, function(x) x$camera$azimuth, numeric(1)), c(-60, 30))
  expect_true(all(vapply(w$x$panels, function(x) length(x$glyphs) == 1L, logical(1))))
  b <- viewer_browser(list(w))
  on.exit(b$close(), add = TRUE)
  hits <- viewer_hits(b, "facets-text", step = 4)
  labels <- Filter(function(h) h$layer == 1L, hits)
  expect_gt(length(labels), 0L)
  expect_setequal(unique(vapply(labels, `[[`, integer(1), "panel")), 2L)
  expect_setequal(unique(vapply(labels, `[[`, character(1), "key")), "Front")
  b$eval("document.getElementById('facets-text').ggplot3d.setCamera({elevation: -90});document.getElementById('facets-text').ggplot3d.redraw()")
  hits <- viewer_hits(b, "facets-text", step = 4)
  labels <- Filter(function(h) h$layer == 1L, hits)
  expect_gt(length(labels), 0L)
  expect_setequal(unique(vapply(labels, `[[`, integer(1), "panel")), 1L)
  expect_setequal(unique(vapply(labels, `[[`, character(1), "key")), "Behind")
})
