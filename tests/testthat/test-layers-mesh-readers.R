mesh_fixture <- function(name) test_path("fixtures", "meshes", name)

mesh_face_sets <- function(m) {
  faces <- split(m[order(m$.face, m$.corner), c("x", "y", "z")], m$.face[order(m$.face, m$.corner)])
  sort(vapply(faces, function(f) paste(sort(paste(f$x, f$y, f$z)), collapse = ";"), character(1)))
}

test_that("OBJ reads n-gons, mixed references, relative indices and groups", {
  m <- read_mesh_3d(mesh_fixture("cube.obj"))
  expect_s3_class(m, "ggplot3d_mesh")
  expect_equal(length(unique(m$.face)), 6L)
  expect_true(all(table(m$.face) == 4L))
  expect_equal(nrow(unique(m[c("x", "y", "z")])), 8L)
  expect_equal(unique(m$group), c("cube", "sides"))
  # Every edge of the closed cube is shared by two faces.
  rows <- order(m$.face, m$.corner); v <- m$.vertex[rows]; f <- m$.face[rows]
  nxt <- ave(v, f, FUN = function(x) c(x[-1L], x[1L]))
  expect_true(all(table(paste(pmin(v, nxt), pmax(v, nxt))) == 2L))
  s <- stats_scene(ggplot2::ggplot() + geom_mesh_3d(data = m) + coord_3d())
  expect_length(s$packet$faces$primitive_id, 12L)
})

test_that("ASCII and binary STL agree and weld shared corners", {
  a <- read_mesh_3d(mesh_fixture("tetra-ascii.stl")); b <- read_mesh_3d(mesh_fixture("tetra-binary.stl"))
  expect_equal(length(unique(a$.vertex)), 4L); expect_equal(length(unique(b$.vertex)), 4L)
  expect_equal(mesh_face_sets(a), mesh_face_sets(b), ignore_attr = TRUE)
  expect_equal(nrow(a), 12L)
})

test_that("ASCII and binary PLY read geometry and colours", {
  a <- read_mesh_3d(mesh_fixture("cube-ascii.ply")); b <- read_mesh_3d(mesh_fixture("cube-binary.ply"))
  expect_equal(mesh_face_sets(a), mesh_face_sets(b), ignore_attr = TRUE)
  expect_equal(nrow(unique(a[c("x", "y", "z")])), 8L)
  expect_equal(a$vertex_colour[a$x == 1 & a$y == 0 & a$z == 0][1L], "#00FF00")
  expect_equal(unique(b$face_colour), grDevices::rgb(200, 100, 50, maxColorValue = 255))
  expect_null(a$face_colour)
  expect_error(read_mesh_3d(tempfile(fileext = ".obj")), "existing file")
  bad <- tempfile(fileext = ".off"); writeLines("OFF", bad)
  expect_error(read_mesh_3d(bad), "No mesh reader")
})
