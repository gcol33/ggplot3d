test_that("texture_3d() reads colour matrices, arrays, native rasters and PNG files", {
  colours <- matrix(c("red", "#00FF0080", "blue", "white"), 2L)
  tex <- texture_3d(colours)
  expect_s3_class(tex, "ggplot3d_texture")
  expect_identical(dim(tex$image), c(2L, 2L))
  expect_false(tex$opaque)
  channels <- texture_channels_3d(tex$image)
  expect_equal(channels[, 1L], c(255, 0, 0, 255))
  expect_equal(channels[, 3L], c(0, 255, 0, 128))
  rgba <- array(0, c(2L, 3L, 4L)); rgba[, , 1L] <- 1; rgba[, , 4L] <- 1; rgba[2L, 3L, 4L] <- 0
  arr <- texture_3d(rgba)
  expect_identical(dim(arr$image), c(2L, 3L))
  expect_equal(texture_channels_3d(arr$image)[4L, 6L], 0)
  grey <- texture_3d(matrix(c(0, 0.5, 1, 1), 2L))
  expect_true(grey$opaque)
  expect_equal(texture_channels_3d(grey$image)[1:3, 2L], rep(round(255 * 1), 3L))
  expect_identical(texture_3d(tex$image)$image, tex$image)
  kept <- texture_3d(texture_3d(colours, interpolate = FALSE, extent = c(0, 2, 0, 1)))
  expect_false(kept$interpolate)
  expect_identical(kept$extent, c(0, 2, 0, 1))
  expect_output(print(kept), "2 x 2 pixels")
  expect_error(texture_3d(array(2, c(2, 2, 3))), "\\[0, 1\\]")
  expect_error(texture_3d(colours, extent = c(0, 0, 0, 1)), "distinct")
  skip_if_not_installed("png")
  file <- tempfile(fileext = ".png")
  writeBin(texture_png_3d(tex$image), file)
  expect_identical(unclass(png::readPNG(file, native = TRUE)), unclass(tex$image))
  expect_identical(unclass(texture_3d(file)$image), unclass(tex$image))
})

test_that("draped surfaces emit texture coordinates from x and y over the texture extent", {
  d <- expand.grid(x = seq(0, 4, length.out = 5), y = seq(0, 2, length.out = 3))
  d$z <- d$x + d$y
  p <- ggplot2::ggplot(d, ggplot2::aes(x, y, z = z))
  tex <- texture_3d(matrix(c("red", "blue"), 1L))
  packet <- stats_scene(p + geom_surface_3d(texture = tex) + coord_3d())$packet
  expect_true(all(c("u", "v") %in% names(packet$corners)))
  expect_equal(range(packet$corners$u), c(0, 1))
  expect_equal(range(packet$corners$v), c(0, 1))
  expect_identical(unique(packet$styles$texture_id), 1L)
  expect_identical(packet$styles$texture[[1L]]$image, tex$image)
  expect_true(all(packet$faces$opacity == 1))
  placed <- stats_scene(p + geom_surface_3d(texture = texture_3d(tex, extent = c(0, 8, 0, 2))) + coord_3d())$packet
  expect_equal(range(placed$corners$u), c(0, 0.5))
  expect_true(all(placed$faces$opacity == 1))
  inside <- stats_scene(p + geom_surface_3d(texture = texture_3d(tex, extent = c(0, 8, 0, 2)), fill = NA) + coord_3d())$packet
  expect_identical(length(inside$faces$offset), length(placed$faces$offset))
  expect_true(all(inside$faces$opacity == 1))
  bare <- stats_scene(p + geom_surface_3d(texture = texture_3d(tex, extent = c(2, 6, 0, 2)), fill = NA) + coord_3d())$packet
  expect_true(all(bare$faces$opacity == 1))
  expect_true(any(!bare$faces$coverage_opaque) && any(bare$faces$coverage_opaque))
  mapped <- stats_scene(ggplot2::ggplot(transform(d, s = x / 8, t = y / 4), ggplot2::aes(x, y, z = z, u = s, v = t)) +
    geom_surface_3d(texture = texture_3d(tex, extent = c(0, 1, 0, 1))) + coord_3d())$packet
  expect_equal(range(mapped$corners$u), c(0, 0.5))
  expect_equal(range(mapped$corners$v), c(0, 0.5))
  holed <- stats_scene(p + geom_surface_3d(texture = texture_3d(matrix(c("red", NA), 1L))) + coord_3d())$packet
  expect_true(all(holed$faces$opacity == 1))
  expect_true(all(holed$faces$coverage_opaque))
  bare_holed <- stats_scene(p + geom_surface_3d(texture = texture_3d(matrix(c("red", NA), 1L)), fill = NA) + coord_3d())$packet
  expect_true(all(bare_holed$faces$opacity == 1))
  expect_true(all(!bare_holed$faces$coverage_opaque))
})

test_that("parametric surfaces drape by their parameters, seams included", {
  torus <- function(u, v) cbind((2 + cos(v)) * cos(u), (2 + cos(v)) * sin(u), sin(v))
  tex <- texture_3d(matrix(grDevices::rainbow(6), 2L))
  packet <- stats_scene(ggplot2::ggplot() +
    stat_parametric_3d(fun = torus, closed = TRUE, n = c(12L, 8L), texture = tex) + coord_3d())$packet
  expect_equal(range(packet$corners$u), c(0, 1))
  expect_equal(range(packet$corners$v), c(0, 1))
  u <- matrix(packet$corners$u[seq_len(3L * length(packet$faces$offset))], 3L)
  expect_lt(max(apply(u, 2L, function(x) diff(range(x)))), 0.2)
})

test_that("merged layers keep distinct texture ids", {
  d <- expand.grid(x = 0:2, y = 0:2); d$z <- d$x
  a <- texture_3d(matrix("red")); b <- texture_3d(matrix("blue"))
  packet <- stats_scene(ggplot2::ggplot(d, ggplot2::aes(x, y, z = z)) +
    geom_surface_3d(texture = a) + geom_surface_3d(ggplot2::aes(z = z + 1), texture = b) + coord_3d())$packet
  textures <- panel_textures_3d(packet$styles)
  expect_length(textures$textures, 2L)
  face <- textures$index[match(packet$faces$style_id, packet$styles$style_id)]
  expect_setequal(face, 1:2)
  expect_identical(textures$textures[[1L]]$image, a$image)
  expect_identical(textures$textures[[2L]]$image, b$image)
})

test_that("rgl meshes keep their texture coordinates and texture file", {
  skip_if_not_installed("rgl")
  skip_if_not_installed("png")
  file <- tempfile(fileext = ".png")
  writeBin(texture_png_3d(texture_3d(matrix(c("red", "green", "blue", "white"), 2L))$image), file)
  mesh <- structure(list(vb = rbind(c(0, 1, 1, 0), c(0, 0, 1, 1), 0, 1), ib = matrix(1:4, 4L),
    texcoords = rbind(c(0, 1, 1, 0), c(0, 0, 1, 1)), material = list(texture = file)), class = c("mesh3d", "shape3d"))
  layer <- geom_mesh_3d(data = mesh, shade = FALSE)
  expect_s3_class(layer$geom_params$texture, "ggplot3d_texture")
  packet <- stats_scene(ggplot2::ggplot() + layer + coord_3d())$packet
  quad <- packet$corners
  expect_setequal(round(quad$u, 6), c(0, 1))
  expect_setequal(round(quad$v, 6), c(0, 1))
  x <- packet$vertices$x[quad$vertex]
  expect_identical(order(quad$u, quad$v), order(x, packet$vertices$y[quad$vertex]))
})

test_that("images lie on planes of the panel box and depth-sort with the scene", {
  logo <- matrix(c("red", "green", "blue", "white"), 2L)
  d <- data.frame(x = c(0, 2), y = c(0, 1), z = c(0, 3))
  p <- ggplot2::ggplot(d, ggplot2::aes(x, y, z = z)) + geom_point_3d()
  wall <- stats_scene(p + annotation_raster_3d(logo, 0, 2, 0, 3, plane = "xz", at = Inf) + coord_3d())$packet
  face <- wall$faces
  expect_length(face$offset, 1L)
  corners <- face$offset + 0:3
  expect_equal(wall$vertices$y[wall$corners$vertex[corners]], rep(1, 4L))
  expect_equal(wall$corners$u[corners], c(0, 1, 1, 0))
  expect_equal(wall$corners$v[corners], c(0, 0, 1, 1))
  points <- wall$corners$vertex[wall$markers$offset]
  expect_equal(range(wall$vertices$z[wall$corners$vertex[corners]]), range(wall$vertices$z[points]))
  floor <- stats_scene(p + annotation_raster_3d(logo, plane = "floor") + coord_3d())$packet
  expect_equal(unique(floor$vertices$z[floor$corners$vertex[floor$faces$offset + 0:3]]), 0)
  ann <- stats_scene(p + ggplot2::annotation_raster(logo, 0, 2, 0, 1) + coord_3d())$packet
  expect_length(ann$faces$offset, 1L)
  expect_identical(unique(ann$styles$texture_id[ann$faces$style_id]), 1L)
  expect_error(annotation_raster_3d(logo, plane = "xx"), "distinct")
})

test_that("geom_raster_3d() draws a grid as one image per level", {
  d <- expand.grid(x = 1:4, y = 1:3)
  d$value <- seq_len(nrow(d))
  d$value[5L] <- NA
  p <- ggplot2::ggplot(d, ggplot2::aes(x, y, fill = value)) + geom_raster_3d() +
    ggplot2::scale_fill_gradient(na.value = NA) + coord_3d()
  packet <- stats_scene(p)$packet
  expect_length(packet$faces$offset, 1L)
  tex <- packet$styles$texture[[packet$faces$style_id]]
  expect_identical(dim(tex$image), c(3L, 4L))
  expect_false(tex$interpolate)
  alpha <- matrix(texture_channels_3d(tex$image)[4L, ], 3L, byrow = TRUE)
  expect_identical(sum(alpha == 0), 1L)
  expect_equal(alpha[3L, 1L], 255)
  fill <- matrix(texture_channels_3d(tex$image)[1L, ], 3L, byrow = TRUE)
  expect_false(fill[3L, 1L] == fill[1L, 4L])
  slices <- expand.grid(x = 1:3, y = 1:3, z = c(1, 2))
  slices$value <- seq_len(nrow(slices))
  stack <- stats_scene(ggplot2::ggplot(slices, ggplot2::aes(x, y, z = z, fill = value)) + geom_raster_3d() + coord_3d())$packet
  expect_length(stack$faces$offset, 2L)
  side <- stats_scene(ggplot2::ggplot(d, ggplot2::aes(x, z = y, fill = value)) +
    geom_raster_3d(plane = "xz", at = 2) + coord_3d())$packet
  expect_length(side$faces$offset, 1L)
  ragged <- data.frame(x = c(1, 2, 2.3), y = 1, value = 1:3)
  expect_error(stats_scene(ggplot2::ggplot(ragged, ggplot2::aes(x, y, fill = value)) + geom_raster_3d() + coord_3d()),
    "regular grid")
})

test_that("the viewer receives textures, texture coordinates and texture layers", {
  skip_if_not_installed("htmlwidgets")
  d <- expand.grid(x = 0:3, y = 0:3); d$z <- 0
  p <- ggplot2::ggplot(d, ggplot2::aes(x, y, z = z)) +
    geom_surface_3d(texture = texture_3d(matrix(c("red", "blue"), 1L), interpolate = FALSE), shade = FALSE) +
    coord_3d(elevation = 90, azimuth = -90)
  w <- view_3d(p, width = 300, height = 300, elementId = "texture-view")
  scene <- w$x$panels[[1L]]$scene
  expect_length(scene$textures, 1L)
  expect_false(scene$textures[[1L]]$interpolate)
  expect_true(all(c("u", "v") %in% names(scene$corners)))
  layers <- vapply(scene$materials, `[[`, 0L, "texture")
  expect_true(any(layers == 0L))
  b <- viewer_browser(list(w), 320, 320)
  on.exit(b$close(), add = TRUE)
  start <- Sys.time()
  repeat {
    rgba <- viewer_canvas_pixels(b, "texture-view")[[1L]]$rgba
    red <- sum(rgba[, 1L] > 200 & rgba[, 2L] < 60 & rgba[, 3L] < 60)
    blue <- sum(rgba[, 3L] > 200 & rgba[, 1L] < 60 & rgba[, 2L] < 60)
    if ((red > 100 && blue > 100) || Sys.time() - start > 10) break
    Sys.sleep(0.2)
  }
  expect_gt(red, 100)
  expect_gt(blue, 100)
})

test_that("glTF export writes texture coordinates and the embedded image", {
  skip_if_not_installed("jsonlite")
  skip_if_not_installed("png")
  d <- expand.grid(x = 0:2, y = 0:2); d$z <- d$x * d$y
  tex <- texture_3d(matrix(c("red", "green", "blue", "white"), 2L), interpolate = FALSE)
  p <- ggplot2::ggplot(d, ggplot2::aes(x, y, z = z)) + geom_surface_3d(texture = tex, shade = FALSE) + coord_3d()
  file <- tempfile(fileext = ".glb")
  export_3d(p, file)
  glb <- read_glb(file)
  expect_length(glb$json$textures, 1L)
  expect_identical(glb$json$samplers[[1L]]$magFilter, 9728L)
  material <- glb$json$materials[[1L]]
  expect_identical(material$pbrMetallicRoughness$baseColorTexture$index, 0L)
  prim <- glb$json$meshes[[glb_node(glb, "layer1_surface")$mesh + 1L]]$primitives[[1L]]
  uv <- glb_accessor(glb, prim$attributes$TEXCOORD_0)
  expect_equal(range(uv), c(0, 1), tolerance = 1e-6)
  view <- glb$json$bufferViews[[glb$json$images[[1L]]$bufferView + 1L]]
  png_file <- tempfile(fileext = ".png")
  writeBin(glb$bin[view$byteOffset + seq_len(view$byteLength)], png_file)
  expect_identical(unclass(png::readPNG(png_file, native = TRUE)), unclass(tex$image))
})

test_that("the viewer blends texture coverage independently of face opacity", {
  skip_if_not_installed("htmlwidgets")
  packet <- stats_scene(texture_viewer_plot())$packet
  expect_true(all(packet$faces$opacity == 1))
  expect_true(all(!packet$faces$coverage_opaque))
  b <- viewer_browser(list(view_3d(texture_viewer_plot(), width = 240, height = 240, elementId = "texture-forward"),
    view_3d(texture_viewer_plot(TRUE), width = 240, height = 240, elementId = "texture-reverse")))
  on.exit(b$close(), add = TRUE)
  start <- Sys.time()
  repeat {
    result <- b$eval(paste0("(() => {const pixels=id=>{const el=document.getElementById(id);el.ggplot3d.redraw();",
      "const c=el.querySelector('canvas'),g=c.getContext('webgl2'),p=new Uint8Array(c.width*c.height*4);",
      "g.readPixels(0,0,c.width,c.height,g.RGBA,g.UNSIGNED_BYTE,p);return p;};",
      "const a=pixels('texture-forward'),b=pixels('texture-reverse');let max=0,first=0,second=0;",
      "for(let i=0;i<a.length;i++)max=Math.max(max,Math.abs(a[i]-b[i]));",
      "for(let i=0;i<a.length;i+=4){if(a[i+1]+2<Math.min(a[i],a[i+2]))first++;",
      "if(b[i+1]+2<Math.min(b[i],b[i+2]))second++;}return {max,first,second};})()"))
    if ((result$first > 100 && result$second > 100) || Sys.time() - start > 10) break
    Sys.sleep(.2)
  }
  expect_gt(result$first, 100)
  expect_gt(result$second, 100)
  expect_lte(result$max, 2)
})
