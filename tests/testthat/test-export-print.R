printing_scene <- function(nodes, box = c(1, 1, 1)) {
  list(nodes = nodes, box = box, up = 3L, mm = 100,
    camera = list(right = c(1, 0, 0), up = c(0, 1, 0)),
    layers = data.frame(name = vapply(nodes, `[[`, character(1), "name"), text = FALSE))
}

printing_node <- function(mesh, name = "layer1_mesh") {
  list(name = name, triangles = list(position = mesh$position[mesh$index, , drop = FALSE],
    colour = mesh$colour[mesh$index[1L, ], , drop = FALSE]))
}

test_that("printing settings validate physical sizes and layer methods", {
  expect_equal(print_3d(min_mm = 2, shell_mm = 1, base_mm = 1)$shell_mm, 2)
  expect_equal(print_3d(base_mm = 0)$base_mm, 0)
  expect_error(print_3d(size_mm = c(1, 0, 1)), "size_mm")
  expect_error(print_3d(min_mm = 0), "min_mm")
  expect_error(print_3d(layers = c("base")), "layers")
  expect_error(print_3d(layers = c(foo = "thin")), "layers")
  expect_error(print_3d(sides = 5), "sides")
  expect_error(export_3d(NULL, tempfile(fileext = ".3mf")), "requires print")
  expect_error(export_3d(NULL, tempfile(fileext = ".ply"), print = print_3d()), "does not support")
})

test_that("native printing checks find boundary, winding, incidence and intersections", {
  cube <- print_box_mesh_3d(c(2, 2, 2))
  check <- ggplot3d:::print_mesh_cpp(cube$position, t(cube$index))
  expect_length(check$parts, 1)
  expect_true(check$parts[[1]]$closed)
  expect_equal(check$parts[[1]]$volume, 8)
  plane <- rbind(c(0, 0, 0), c(2, 0, 0), c(2, 2, 0), c(0, 2, 0))
  triangles <- rbind(c(1L, 2L, 3L), c(1L, 3L, 4L))
  open <- ggplot3d:::print_mesh_cpp(plane, triangles)$parts[[1]]
  expect_false(open$closed)
  expect_equal(nrow(open$boundary), 4)
  expect_error(ggplot3d:::print_mesh_cpp(plane, rbind(triangles[1, ], triangles[2, 3:1])), "winding")
  expect_error(ggplot3d:::print_mesh_cpp(plane, triangles[rep(1L, 3L), ]), "non-manifold")
  crossing <- rbind(c(-1, -1, 0), c(1, -1, 0), c(0, 1, 0),
    c(0, -.5, -1), c(0, -.5, 1), c(0, .5, 0))
  expect_error(ggplot3d:::print_mesh_cpp(crossing, matrix(1:6, 2L, byrow = TRUE), TRUE), "self-intersection")
})

test_that("shells use millimetre thickness and base closures fill their footprint", {
  plane <- rbind(c(0, 0, .5), c(1, 0, .5), c(1, 1, .5), c(0, 1, .5))
  mesh <- export_soup_3d(plane, matrix(c(1L, 2L, 3L, 1L, 3L, 4L), 3L), matrix(1, 4, 4), FALSE)
  scene <- printing_scene(list(printing_node(mesh)))
  settings <- print_3d(size_mm = c(20, 40, 30), shell_mm = 1.5, furniture = character(), text = "none")
  solid <- print_scene_3d(scene, settings)
  checked <- ggplot3d:::print_mesh_cpp(solid$printing_mesh$position, solid$printing_mesh$triangles, TRUE)
  expect_equal(checked$parts[[1]]$volume, 20 * 20 * 1.5)
  expect_equal(apply(solid$printing_mesh$position, 2L, function(x) diff(range(x))), c(20, 20, 1.5))
  settings$surface <- "base"
  solid <- print_scene_3d(scene, settings)
  expect_equal(ggplot3d:::print_mesh_cpp(solid$printing_mesh$position, solid$printing_mesh$triangles)$parts[[1]]$volume, 20 * 20 * 10)
  settings$layers <- c(absent = "shell")
  expect_error(print_scene_3d(scene, settings), "unknown exported layer")
})

test_that("Boolean union removes interior overlap and preserves cavity boundaries", {
  settings <- print_3d(size_mm = rep(3, 3), furniture = character(), text = "none")
  a <- print_box_mesh_3d(c(2, 2, 2), colour = c(1, 0, 0, 1))
  b <- print_box_mesh_3d(c(2, 2, 2), c(1, 0, 0), colour = c(0, 0, 1, 1))
  solid <- print_scene_3d(printing_scene(list(printing_node(a, "red"), printing_node(b, "blue")), rep(3, 3)), settings)
  checked <- ggplot3d:::print_mesh_cpp(solid$printing_mesh$position, solid$printing_mesh$triangles, TRUE)
  expect_length(checked$parts, 1)
  expect_equal(checked$parts[[1]]$volume, 12)
  expect_true(all(c("#FF0000", "#0000FF") %in% grDevices::rgb(solid$printing_mesh$colour[, 1:3])))
  cavity <- print_box_mesh_3d(c(1, 1, 1), c(.5, .5, .5))
  cavity$index <- cavity$index[3:1, , drop = FALSE]
  hollow <- export_join_soups_3d(list(a, cavity))
  parts <- print_parts_3d(hollow, "hollow", settings)
  united <- print_union_3d(parts, list())
  checked <- ggplot3d:::print_mesh_cpp(united$position, united$triangles, TRUE)
  expect_equal(sum(vapply(checked$parts, `[[`, numeric(1), "volume")), 7)
  expect_equal(sort(vapply(checked$parts, `[[`, numeric(1), "volume")), c(-1, 8))
})

test_that("printing points and paths respect minimum dimensions", {
  node <- list(name = "marks", markers = list(position = matrix(c(.5, .5, .5), 1L),
    colour = matrix(c(1, 0, 0, 1), 1L), radius = .001, shape = 15))
  settings <- print_3d(size_mm = rep(10, 3), min_mm = 2, furniture = character(), text = "none")
  solid <- print_scene_3d(printing_scene(list(node)), settings)
  expect_equal(apply(solid$printing_mesh$position, 2L, function(x) diff(range(x))), rep(2, 3))
  node$markers <- NULL
  node$paths <- list(position = rbind(c(0, 0, 0), c(1, 0, 0)), path = c(1L, 1L),
    colour = matrix(c(0, 0, 1, 1), 1L), radius = .001)
  solid <- print_scene_3d(printing_scene(list(node)), settings)
  dimensions <- apply(solid$printing_mesh$position, 2L, function(x) diff(range(x)))
  expect_equal(dimensions[1L], 10)
  expect_true(all(dimensions[2:3] >= 2))
  expect_lte(max(dimensions[2:3]), 2 / cos(pi / settings$sides) + 1e-10)
})

test_that("STL and 3MF printing exports carry closed solids and triangle colours", {
  p <- export_surface_plot(n = 3, colour = NA, fill = "#336699", coord = coord_3d(light = NULL))
  settings <- print_3d(size_mm = c(30, 30, 15), surface = "base", furniture = "base", text = "none")
  stl <- tempfile(fileext = ".stl"); mf <- tempfile(fileext = ".3mf")
  export_3d(p, stl, print = settings, width = 240, height = 240)
  raw <- readBin(stl, "raw", file.size(stl))
  count <- readBin(raw[81:84], "integer", size = 4L, endian = "little")
  expect_equal(length(raw), 84 + 50 * count)
  imported <- read_mesh_3d(stl)
  pos <- as.matrix(imported[c("x", "y", "z")])
  checked <- ggplot3d:::print_mesh_cpp(pos, t(matrix(seq_len(nrow(pos)), 3L)), TRUE)
  expect_true(all(vapply(checked$parts, `[[`, logical(1), "closed")))
  export_3d(p, mf, print = settings, width = 240, height = 240)
  members <- utils::unzip(mf, list = TRUE)$Name
  expect_setequal(members, c("[Content_Types].xml", "_rels/.rels", "3D/3dmodel.model"))
  skip_if_not_installed("xml2")
  xml <- xml2::read_xml(unz(mf, "3D/3dmodel.model"))
  expect_equal(xml2::xml_attr(xml, "unit"), "millimeter")
  expect_gt(length(xml2::xml_find_all(xml, "//*[local-name()='triangle']")), 0)
  expect_true("#336699FF" %in% xml2::xml_attr(xml2::xml_find_all(xml, "//*[local-name()='color']"), "color"))
})

test_that("raised and engraved text are extruded with counters and axis furniture", {
  skip_if_not_installed("systemfonts")
  for (mode in c("raised", "engraved")) {
    settings <- print_3d(size_mm = rep(20, 3), text = mode, furniture = character())
    text <- print_text_3d("O", 12, "", "plain", c(0, 0, 0), c(1, 0, 0), c(0, 1, 0),
      c(1, 0, 0, 1), settings, 1, "label")
    solid <- print_union_3d(list(), list(text))
    checked <- ggplot3d:::print_mesh_cpp(solid$position, solid$triangles, TRUE)
    expect_true(all(vapply(checked$parts, `[[`, logical(1), "closed")))
    expect_gt(sum(vapply(checked$parts, `[[`, numeric(1), "volume")), 0)
  }
  p <- ggplot2::ggplot(data.frame(x = c(0, 1), y = c(0, 1), z = c(0, 1)), ggplot2::aes(x, y, z = z)) +
    geom_point_3d(size = 2) + coord_3d()
  file <- tempfile(fileext = ".3mf")
  expect_silent(export_3d(p, file, print = print_3d(size_mm = rep(20, 3)), width = 240, height = 240))
  expect_gt(file.size(file), 100)
})
