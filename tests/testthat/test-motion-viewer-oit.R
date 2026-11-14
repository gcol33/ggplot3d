viewer_crossing_plot <- function(reverse = FALSE, opaque = FALSE) {
  red <- data.frame(x = c(.1, .9, .9, .1), y = c(.1, .1, .9, .9), z = c(.2, .8, .8, .2))
  blue <- red; blue$z <- 1 - red$z
  layers <- list(geom_polygon_3d(data = red, fill = "red", colour = NA, alpha = .5, shade = FALSE),
    geom_polygon_3d(data = blue, fill = "blue", colour = NA, alpha = .5, shade = FALSE))
  if (reverse) layers <- rev(layers)
  if (opaque) {
    green <- red; green$z <- .95
    layers <- c(layers, list(geom_polygon_3d(data = green, fill = "green", colour = NA, shade = FALSE)))
  }
  ggplot2::ggplot(NULL, ggplot2::aes(x, y, z = z)) + layers +
    ggplot2::scale_x_continuous(limits = c(0, 1)) + ggplot2::scale_y_continuous(limits = c(0, 1)) +
    scale_z_continuous(limits = c(0, 1)) + coord_3d(azimuth = 0, elevation = 90, light = NULL)
}

test_that("weighted transparency is invariant to crossing sheet order", {
  skip_if_not_installed("htmlwidgets")
  b <- viewer_browser(list(
    view_3d(viewer_crossing_plot(), panel = 1, width = 240, height = 240, elementId = "forward"),
    view_3d(viewer_crossing_plot(TRUE), panel = 1, width = 240, height = 240, elementId = "reverse")))
  on.exit(b$close(), add = TRUE)
  result <- b$eval(paste0("(() => { const pixels=id=>{ const el=document.getElementById(id); el.ggplot3d.redraw();",
    "const c=el.querySelector('canvas'),g=c.getContext('webgl2'),p=new Uint8Array(c.width*c.height*4);",
    "g.readPixels(0,0,c.width,c.height,g.RGBA,g.UNSIGNED_BYTE,p); if(g.getError()) throw Error('WebGL error');return p;};",
    "const a=pixels('forward'),b=pixels('reverse');let max=0,coloured=0;",
    "for(let i=0;i<a.length;i++)max=Math.max(max,Math.abs(a[i]-b[i]));",
    "for(let i=0;i<a.length;i+=4)if(a[i+1]+20<Math.min(a[i],a[i+2]))coloured++;return {max,coloured};})()"))
  expect_lte(result$max, 2)
  expect_gt(result$coloured, 100)
})

test_that("opaque geometry occludes transparent sheets and their pick ids", {
  skip_if_not_installed("htmlwidgets")
  b <- viewer_browser(list(view_3d(viewer_crossing_plot(opaque = TRUE), panel = 1,
    width = 240, height = 240, elementId = "opaque")))
  on.exit(b$close(), add = TRUE)
  hits <- viewer_hits(b, "opaque", step = 8)
  expect_gt(length(hits), 0)
  expect_setequal(unique(vapply(hits, `[[`, integer(1), "layer")), 3L)
  expect_equal(b$eval("document.querySelector('#opaque canvas').getContext('webgl2').getError()"), 0)
})

test_that("disc clouds use four-vertex instanced draws through selection and resize", {
  skip_if_not_installed("htmlwidgets")
  d <- expand.grid(x = seq(0, 1, length.out = 16), y = seq(0, 1, length.out = 16))
  d$z <- d$x * d$y; d$id <- as.character(seq_len(nrow(d)))
  p <- ggplot2::ggplot(d, ggplot2::aes(x, y, z = z, data_id = id)) +
    geom_point_3d(shape = 21, fill = "#CC4422", colour = "black", size = 2) + coord_3d()
  b <- viewer_browser(list(view_3d(p, panel = 1, width = 240, height = 240, elementId = "cloud")))
  on.exit(b$close(), add = TRUE)
  result <- b$eval(paste0("(() => { const el=document.getElementById('cloud'),g=el.querySelector('canvas').getContext('webgl2');",
    "const fn=g.drawArraysInstanced.bind(g),calls=[];g.drawArraysInstanced=(...a)=>{calls.push(a);return fn(...a);};",
    "el.ggplot3d.redraw();g.drawArraysInstanced=fn;return {calls,error:g.getError()};})()"))
  calls <- do.call(rbind, result$calls)
  expect_true(all(calls[, 3L] == 4))
  expect_true(any(calls[, 4L] == nrow(d)))
  expect_equal(result$error, 0)
  b$eval("document.getElementById('cloud').ggplot3d.select(['1']);document.getElementById('cloud').ggplot3d.redraw()")
  hits <- viewer_hits(b, "cloud", step = 8)
  expect_gt(length(hits), 0)
  expect_true(all(vapply(hits, `[[`, character(1), "key") %in% d$id))
  b$eval("document.getElementById('cloud').style.width='280px';document.getElementById('cloud').style.height='200px'")
  Sys.sleep(.2)
  expect_equal(b$eval("document.querySelector('#cloud canvas').getContext('webgl2').getError()"), 0)
})
