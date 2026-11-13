test_that("GLB files have a valid container, one node per layer and a camera", {
  skip_if_not_installed("jsonlite")
  p <- export_surface_plot(ggplot2::aes(fill = z), colour = NA) +
    geom_point_3d(data = data.frame(x = 0, y = 0, z = 0.2), colour = "red", size = 3)
  file <- tempfile(fileext = ".glb")
  expect_identical(export_3d(p, file), file)
  glb <- read_glb(file)
  expect_identical(glb$header, c(0x46546C67L, 2L, as.integer(file.size(file))))
  expect_identical(glb$json$asset$version, "2.0")
  names <- vapply(glb$json$nodes, `[[`, "", "name")
  expect_setequal(names, c("layer1_surface", "layer2_point", "camera"))
  expect_identical(unlist(glb$json$scenes[[1]]$nodes), seq_along(names) - 1L)
  surface <- glb$json$meshes[[glb_node(glb, "layer1_surface")$mesh + 1L]]
  expect_identical(vapply(surface$primitives, `[[`, 0L, "mode"), 4L)
  position <- glb_accessor(glb, surface$primitives[[1]]$attributes$POSITION)
  accessor <- glb$json$accessors[[surface$primitives[[1]]$attributes$POSITION + 1L]]
  expect_equal(unlist(accessor$min), apply(position, 2L, min))
  expect_equal(unlist(accessor$max), apply(position, 2L, max))
  expect_true(all(position >= -1 - 1e-6 & position <= 1 + 1e-6))
  normal <- glb_accessor(glb, surface$primitives[[1]]$attributes$NORMAL)
  expect_equal(sqrt(rowSums(normal^2)), rep(1, nrow(normal)), tolerance = 1e-6)
  index <- glb_accessor(glb, surface$primitives[[1]]$indices)
  expect_identical(length(index), 3L * 2L * 25L)
  expect_true(all(index >= 0L & index < nrow(position)))
})

test_that("vertical plot axis becomes +y and size scales the model", {
  skip_if_not_installed("jsonlite")
  corners <- function(...) ggplot2::ggplot(data.frame(x = c(0, 1), y = c(0, 1), z = c(0, 1)), ggplot2::aes(x, y, z = z)) +
    geom_point_3d(size = 1) + coord_3d(expand = FALSE, aspect = c(1, 1, 2), ...)
  p <- corners()
  file <- tempfile(fileext = ".glb")
  export_3d(p, file, points = "point", size = 10)
  glb <- read_glb(file)
  mesh <- glb$json$meshes[[glb_node(glb, "layer1_point")$mesh + 1L]]
  expect_identical(mesh$primitives[[1]]$mode, 0L)
  position <- glb_accessor(glb, mesh$primitives[[1]]$attributes$POSITION)
  expect_equal(position, rbind(c(0, 0, 0), c(5, 10, -5)), tolerance = 1e-6)
  export_3d(corners(up = "x"), file, points = "point")
  glb <- read_glb(file)
  mesh <- glb$json$meshes[[glb_node(glb, "layer1_point")$mesh + 1L]]
  position <- glb_accessor(glb, mesh$primitives[[1]]$attributes$POSITION)
  expect_equal(position[2, ], c(0.5, 0.5, -1), tolerance = 1e-6)
})

test_that("vertex colours are linear and materials follow lighting and alpha", {
  skip_if_not_installed("jsonlite")
  file <- tempfile(fileext = ".glb")
  export_3d(export_surface_plot(fill = "#808080", colour = NA, alpha = 0.5), file)
  glb <- read_glb(file)
  mesh <- glb$json$meshes[[glb_node(glb, "layer1_surface")$mesh + 1L]]
  colour <- glb_accessor(glb, mesh$primitives[[1]]$attributes$COLOR_0)
  expect_equal(unique(round(colour, 5)), matrix(c(rep(round(((128 / 255 + 0.055) / 1.055)^2.4, 5), 3), 0.5), 1L))
  material <- glb$json$materials[[mesh$primitives[[1]]$material + 1L]]
  expect_identical(material$alphaMode, "BLEND")
  expect_true(material$doubleSided)
  expect_null(material$extensions$KHR_materials_unlit)
  export_3d(export_surface_plot(fill = "grey50", colour = NA, coord = coord_3d(light = NULL)), file)
  glb <- read_glb(file)
  material <- glb$json$materials[[1]]
  expect_identical(material$alphaMode, "OPAQUE")
  expect_length(material$extensions$KHR_materials_unlit, 0L)
  expect_false(is.null(material$extensions$KHR_materials_unlit))
  expect_identical(unlist(glb$json$extensionsUsed), "KHR_materials_unlit")
})

test_that("face outlines, paths and markers become tubes, solids or primitives", {
  skip_if_not_installed("jsonlite")
  p <- export_surface_plot(fill = "grey80", colour = "black") +
    geom_path_3d(data = data.frame(x = c(-1, 0, 1), y = c(-1, 1, -1), z = 0), colour = "red") +
    geom_point_3d(data = data.frame(x = 0, y = 0, z = 0), shape = 15)
  file <- tempfile(fileext = ".glb")
  export_3d(p, file, lines = "line", points = "point")
  glb <- read_glb(file)
  modes <- function(name) vapply(glb$json$meshes[[glb_node(glb, name)$mesh + 1L]]$primitives, `[[`, 0L, "mode")
  expect_setequal(modes("layer1_surface"), c(4L, 1L))
  expect_identical(modes("layer2_path"), 1L)
  expect_identical(modes("layer3_point"), 0L)
  surface <- glb$json$meshes[[glb_node(glb, "layer1_surface")$mesh + 1L]]
  lines <- Filter(function(p) p$mode == 1L, surface$primitives)[[1]]
  expect_identical(length(glb_accessor(glb, lines$indices)), 2L * (2L * 5L * 6L + 25L))
  export_3d(p, file)
  glb <- read_glb(file)
  expect_identical(modes("layer2_path"), 4L)
  cube <- glb$json$meshes[[glb_node(glb, "layer3_point")$mesh + 1L]]$primitives[[1]]
  expect_identical(length(glb_accessor(glb, cube$indices)), 36L)
})

test_that("furniture is grouped into panes, grid and box nodes", {
  skip_if_not_installed("jsonlite")
  file <- tempfile(fileext = ".glb")
  export_3d(export_surface_plot(), file, furniture = TRUE)
  glb <- read_glb(file)
  furniture <- glb_node(glb, "furniture")
  children <- vapply(glb$json$nodes[unlist(furniture$children) + 1L], `[[`, "", "name")
  expect_setequal(children, c("panes", "grid", "box"))
})

test_that("the glTF camera reproduces the plot's view", {
  skip_if_not_installed("jsonlite")
  p <- export_surface_plot(coord = coord_3d(azimuth = 20, elevation = 40, roll = 10))
  file <- tempfile(fileext = ".glb")
  export_3d(p, file, size = 2)
  glb <- read_glb(file)
  scene <- export_scene_3d(p, 1L, export_options_3d(FALSE, "tube", "mesh", 2), 640L, 640L, 96)
  node <- glb_node(glb, "camera")
  y_up <- function(v) drop(export_axes_3d(matrix(v, 1L), 3L, "y"))
  q <- unlist(node$rotation)
  expect_equal(quaternion_rotate(q, c(0, 0, -1)), y_up(scene$camera$forward), tolerance = 1e-6)
  expect_equal(quaternion_rotate(q, c(0, 1, 0)), y_up(scene$camera$up), tolerance = 1e-6)
  expect_equal(unlist(node$translation), y_up(scene$camera$eye), tolerance = 1e-6)
  camera <- glb$json$cameras[[1]]
  expect_identical(camera$type, "orthographic")
  expect_equal(camera$orthographic$ymag, scene$camera$ymag, tolerance = 1e-7)
  expect_equal(camera$orthographic$xmag / camera$orthographic$ymag, scene$camera$aspect, tolerance = 1e-7)
  perspective <- export_surface_plot(coord = coord_3d(projection = "perspective", fov = 30, zoom = 2))
  export_3d(perspective, file)
  camera <- read_glb(file)$json$cameras[[1]]
  scene <- export_scene_3d(perspective, 1L, export_options_3d(FALSE, "tube", "mesh", 1), 640L, 640L, 96)
  expect_identical(camera$type, "perspective")
  expect_equal(camera$perspective$yfov, scene$camera$yfov, tolerance = 1e-7)
  expect_equal(camera$perspective$aspectRatio, scene$camera$aspect, tolerance = 1e-7)
  expect_lt(camera$perspective$znear, camera$perspective$zfar)
})

test_that(".gltf output writes a separate binary buffer", {
  skip_if_not_installed("jsonlite")
  dir <- tempfile()
  dir.create(dir)
  file <- file.path(dir, "scene.gltf")
  export_3d(export_surface_plot(), file)
  json <- jsonlite::fromJSON(file, simplifyVector = FALSE)
  expect_identical(json$buffers[[1]]$uri, "scene.bin")
  expect_identical(file.size(file.path(dir, "scene.bin")), as.double(json$buffers[[1]]$byteLength))
})

test_that("animate_3d() writes camera tracks and per-frame geometry to glTF", {
  skip_if_not_installed("jsonlite")
  waves <- expand.grid(x = seq(0, 1, length.out = 5), y = seq(0, 1, length.out = 5), t = c(0, 0.5, 1))
  waves$z <- sin(pi * waves$x) * cos(pi * waves$t)
  p <- ggplot2::ggplot(waves, ggplot2::aes(x, y, z = z)) + geom_surface_3d() +
    geom_point_3d(data = data.frame(x = 0.5, y = 0.5, z = 1), inherit.aes = FALSE,
      mapping = ggplot2::aes(x, y, z = z)) + coord_3d()
  file <- tempfile(fileext = ".glb")
  movie <- animate_3d(p, time = "t", azimuth = c(-60, -30, 0), fps = 2, file = file, progress = FALSE)
  expect_identical(movie$frame_count, 3L)
  expect_identical(movie$frames, character())
  glb <- read_glb(file)
  layer <- glb_node(glb, "layer1_surface")
  children <- glb$json$nodes[unlist(layer$children) + 1L]
  expect_identical(vapply(children, `[[`, "", "name"), sprintf("layer1_surface_frame%d", 1:3))
  expect_null(children[[1]]$scale)
  expect_equal(unlist(children[[2]]$scale), c(0, 0, 0))
  point <- glb_node(glb, "layer2_point")
  expect_false(is.null(point$mesh))
  animation <- glb$json$animations[[1]]
  paths <- vapply(animation$channels, function(ch) ch$target$path, "")
  expect_identical(sum(paths == "scale"), 3L)
  expect_true(all(c("translation", "rotation") %in% paths))
  times <- glb_accessor(glb, animation$samplers[[1]]$input)
  expect_equal(times, c(0, 0.5, 1))
  scale_channels <- animation$channels[paths == "scale"]
  shown <- vapply(scale_channels, function(ch) {
    values <- glb_accessor(glb, animation$samplers[[ch$sampler + 1L]]$output)
    expect_identical(animation$samplers[[ch$sampler + 1L]]$interpolation, "STEP")
    which(values[, 1L] == 1)
  }, integer(1))
  expect_identical(shown, 1:3)
  rotation <- Filter(function(ch) ch$target$path == "rotation", animation$channels)[[1]]
  q <- glb_accessor(glb, animation$samplers[[rotation$sampler + 1L]]$output)
  expect_equal(sqrt(rowSums(q^2)), rep(1, 3), tolerance = 1e-6)
  animate_3d(export_surface_plot(), frames = 2, zoom = c(1, 2), file = file, progress = FALSE)
  glb <- read_glb(file)
  pointers <- Filter(function(ch) identical(ch$target$path, "pointer"), glb$json$animations[[1]]$channels)
  expect_setequal(vapply(pointers, function(ch) ch$target$extensions$KHR_animation_pointer$pointer, ""),
    c("/cameras/0/orthographic/ymag", "/cameras/0/orthographic/xmag"))
  expect_true("KHR_animation_pointer" %in% unlist(glb$json$extensionsUsed))
})

test_that("camera paths export the same endpoint orientations as static scenes", {
  skip_if_not_installed("jsonlite")
  p <- export_surface_plot()
  file <- tempfile(fileext = ".glb")
  movie <- animate_3d(p, path = orbit_3d(60, 1), frames = 3, file = file, progress = FALSE)
  glb <- read_glb(file)
  animation <- glb$json$animations[[1L]]
  channel <- Filter(function(ch) identical(ch$target$path, "rotation"), animation$channels)[[1L]]
  rotations <- glb_accessor(glb, animation$samplers[[channel$sampler + 1L]]$output)
  for (i in c(1L, movie$frame_count)) {
    camera <- movie$camera[i, ]
    export_3d(export_surface_plot(coord = coord_3d(azimuth = camera$azimuth,
      elevation = camera$elevation, roll = camera$roll, zoom = camera$zoom)), file)
    expected <- unlist(glb_node(read_glb(file), "camera")$rotation)
    expect_equal(abs(sum(rotations[i, ] * expected)), 1, tolerance = 1e-6)
  }
})

test_that("deferred screen-sized glyphs export their resolved meshes", {
  skip_if_not_installed("jsonlite")
  p <- ggplot2::ggplot(data.frame(x = 0, y = 0, z = 0), ggplot2::aes(x, y, z = z)) +
    geom_sphere_3d(radius = 2, radius.unit = "mm", detail = 1) + coord_3d()
  file <- tempfile(fileext = ".glb")
  export_3d(p, file)
  mesh <- read_glb(file)$json$meshes[[1L]]
  expect_equal(length(glb_accessor(read_glb(file), mesh$primitives[[1L]]$indices)), 240L)
})

test_that("export and animation arguments are checked", {
  p <- export_surface_plot()
  expect_error(export_3d(p, tempfile(fileext = ".xyz")), "file must end in .glb, .gltf, .obj, .ply, .stl")
  expect_error(export_3d(p, tempfile(fileext = ".stl"), lines = "line"), "STL files hold triangles only")
  expect_error(export_3d(p, tempfile(fileext = ".glb"), size = 0), "size must be a positive finite number")
  expect_error(export_3d(p, tempfile(fileext = ".glb"), panel = 2), "panel is outside the plot layout")
  expect_error(export_3d(ggplot2::ggplot(), tempfile(fileext = ".glb")), "coord_3d")
  expect_error(animate_3d(p, frames = 2, file = tempfile(fileext = ".ply")), "file must end in")
  expect_error(animate_3d(p, frames = 2, file = tempfile(fileext = ".gif"), size = 2), "apply to .glb and .gltf output only")
  expect_error(animate_3d(p, frames = 2, file = tempfile(fileext = ".glb"), colour = 2), "must be named furniture")
  expect_error(animate_3d(export_surface_plot(coord = coord_3d(projection = "perspective")),
    path = dolly_zoom_3d(0, 1), frames = 2, file = tempfile(fileext = ".glb"), progress = FALSE), "one projection type")
})

test_that("the JSON encoder writes objects, arrays, scalars and escapes", {
  expect_identical(json_3d(list(a = 1L, b = c(1.5, 2), c = I(3), d = "x\"y\\", e = TRUE, f = json_object_3d(),
    g = list(list(h = NULL)))), "{\"a\":1,\"b\":[1.5,2],\"c\":[3],\"d\":\"x\\\"y\\\\\",\"e\":true,\"f\":{},\"g\":[{\"h\":null}]}")
  expect_identical(json_3d(0.1), "0.1")
  expect_error(json_3d(Inf), "finite")
})
