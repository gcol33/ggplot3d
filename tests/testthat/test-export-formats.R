closed_and_outward <- function(mesh) {
  key <- apply(round(mesh$position, 9), 1L, paste, collapse = ",")
  vertex <- match(key, unique(key))
  tri <- matrix(vertex[mesh$index], 3L)
  edges <- rbind(t(tri[1:2, ]), t(tri[2:3, ]), t(tri[c(3, 1), ]))
  undirected <- table(paste(pmin(edges[, 1], edges[, 2]), pmax(edges[, 1], edges[, 2])))
  directed <- table(paste(edges[, 1], edges[, 2]))
  a <- mesh$position[mesh$index[1, ], ]; b <- mesh$position[mesh$index[2, ], ]; c <- mesh$position[mesh$index[3, ], ]
  volume <- sum(rowSums(a * cross_rows_3d(b, c))) / 6
  list(closed = all(undirected == 2L) && all(directed == 1L), volume = volume,
    normals_agree = all(rowSums(cross_rows_3d(b - a, c - a) * mesh$normal[mesh$index[1, ], ]) > 0))
}

test_that("tubes are closed, consistently wound and follow their paths", {
  paths <- list(position = rbind(c(0, 0, 0), c(1, 0, 0), c(1, 1, 0), c(1, 1, 0), c(0, 0, 0), c(0, 0, 1)),
    path = c(1L, 1L, 1L, 1L, 2L, 2L), colour = rbind(c(1, 0, 0, 1), c(0, 0, 1, 0.5)), radius = c(0.1, 0.05))
  tube <- export_tubes_3d(paths, sides = 8L)
  expect_identical(nrow(tube$index), 3L)
  check <- closed_and_outward(tube)
  expect_true(check$closed)
  expect_true(check$normals_agree)
  expect_gt(check$volume, 0)
  expect_equal(sqrt(rowSums(tube$normal^2)), rep(1, nrow(tube$normal)))
  expect_setequal(unique(tube$colour[, 4L]), c(1, 0.5))
  single <- export_tubes_3d(list(position = rbind(c(0, 0, 0), c(0, 0, 2)), path = c(1L, 1L),
    colour = matrix(1, 1L, 4L), radius = 0.5))
  expect_equal(range(single$position[, 3L]), c(0, 2))
  expect_equal(max(sqrt(rowSums(single$position[, 1:2]^2))), 0.5)
  expect_null(export_tubes_3d(list(position = rbind(c(0, 0, 0), c(0, 0, 0)), path = c(1L, 1L),
    colour = matrix(1, 1L, 4L), radius = 1)))
})

test_that("marker solids are closed, outward and fit their radius", {
  for (template in export_solid_templates_3d()) {
    check <- closed_and_outward(template)
    expect_true(check$closed)
    expect_true(check$normals_agree)
    expect_gt(check$volume, 0)
    expect_equal(max(sqrt(rowSums(template$position^2))), 1)
  }
  expect_identical(export_solid_shape_3d(c(15, 17, 18, 19, 22, 1)),
    c("cube", "tetrahedron", "octahedron", "sphere", "cube", "sphere"))
})

test_that("PLY files carry colours, faces and edges and read back", {
  p <- export_surface_plot(fill = "#336699", colour = NA, coord = coord_3d(light = NULL))
  file <- tempfile(fileext = ".ply")
  export_3d(p, file)
  mesh <- read_mesh_3d(file)
  expect_identical(length(unique(mesh$.face)), 50L)
  expect_identical(unique(mesh$vertex_colour), "#336699FF")
  export_3d(export_surface_plot(fill = "#336699", colour = "black"), file, lines = "line")
  header <- readLines(file, n = 20L, warn = FALSE)
  expect_true("element edge 85" %in% header)
  expect_true("property uchar alpha" %in% header)
})

test_that("STL files hold whole triangles with the vertical axis as z", {
  p <- ggplot2::ggplot(data.frame(x = c(0, 1), y = c(0, 1), z = c(0, 1)), ggplot2::aes(x, y, z = z)) +
    geom_point_3d(shape = 15) + coord_3d(expand = FALSE, aspect = c(2, 1, 1), up = "x")
  file <- tempfile(fileext = ".stl")
  export_3d(p, file, size = 10)
  expect_identical(file.size(file), 84 + 50 * 24)
  mesh <- read_mesh_3d(file)
  expect_identical(length(unique(mesh$.face)), 24L)
  expect_equal(range(mesh$z), c(0, 10), tolerance = 0.05)
  expect_lt(diff(range(mesh$x)), 5.5)
})

test_that("OBJ files write one object per node and an sRGB material library", {
  dir <- tempfile()
  dir.create(dir)
  file <- file.path(dir, "scene.obj")
  p <- export_surface_plot(fill = "#336699", colour = NA) +
    geom_point_3d(data = data.frame(x = 0, y = 0, z = 0), colour = "#FF0000", alpha = 0.5)
  export_3d(p, file, furniture = TRUE, points = "point")
  lines <- readLines(file)
  expect_identical(lines[2], "mtllib scene.mtl")
  expect_true(all(c("o layer1_surface", "o layer2_point", "o furniture_panes") %in% lines))
  expect_true(any(startsWith(lines, "p ")))
  expect_true("usemtl colour_336699ff" %in% lines)
  library <- readLines(file.path(dir, "scene.mtl"))
  at <- match("newmtl colour_336699ff", library)
  expect_identical(library[at + 1L], "Kd 0.2 0.4 0.6")
  expect_identical(library[match("newmtl colour_ff000080", library) + 4L], "d 0.5019608")
  surface <- export_surface_plot(fill = "#336699", colour = NA)
  export_3d(surface, file)
  stl <- tempfile(fileext = ".stl")
  export_3d(surface, stl)
  from_obj <- read_mesh_3d(file)
  from_stl <- read_mesh_3d(stl)
  expect_equal(range(from_obj$y), range(from_stl$z), tolerance = 1e-6)
  expect_equal(range(from_obj$z), -rev(range(from_stl$y)), tolerance = 1e-6)
})
