read_mesh_fixture <- function(name) {
  path <- testthat::test_path("fixtures", "meshes", name)
  if (tools::file_ext(name) == "obj") {
    path <- tempfile(fileext = ".obj")
    on.exit(unlink(path))
    file.copy(testthat::test_path("fixtures", "meshes", paste0(name, ".txt")), path)
  }
  read_mesh_3d(path)
}
