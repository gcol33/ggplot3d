printing_settings <- function(...) {
  spec <- print_3d(...)
  python <- tryCatch(print_python_3d(spec$python), error = function(e) NULL)
  if (is.null(python)) skip("The configured native Python interpreter is unavailable")
  status <- suppressWarnings(system2(python, c("-c", shQuote("import manifold3d, numpy")), stdout = FALSE, stderr = FALSE))
  if (status != 0L) skip("Python manifold3d and numpy are unavailable")
  spec$python <- python
  spec
}

printing_volume <- function(mesh) {
  a <- mesh$position[mesh$triangles[, 1L], , drop = FALSE]
  b <- mesh$position[mesh$triangles[, 2L], , drop = FALSE]
  c <- mesh$position[mesh$triangles[, 3L], , drop = FALSE]
  sum(rowSums(a * cross_rows_3d(b, c))) / 6
}

test_that("print settings validate physical dimensions and layer keys", {
  expect_equal(print_3d(min_mm = 1, shell_mm = .5, base_mm = .2)$shell_mm, 1)
  expect_equal(print_3d(min_mm = 1, base_mm = .2)$base_mm, 1)
  expect_error(print_3d(size_mm = c(1, 0, 1)), "three positive")
  expect_error(print_3d(min_mm = 0), "invalid physical")
  expect_error(print_3d(layers = "base"), "layers must name")
  expect_error(print_3d(furniture = "panes"), "base and/or axes")
})

test_that("native topology detects manifold edges and inconsistent winding", {
  box <- print_box_mesh_3d(c(2, 2, 2))
  checked <- print_mesh_cpp(box$position, t(box$index))
  expect_length(checked$parts, 1L)
  expect_true(checked$parts[[1L]]$closed)
  expect_equal(checked$parts[[1L]]$volume, 8, tolerance = 1e-12)
  expect_equal(nrow(checked$position), 8L)
  bad <- t(box$index);bad[1L, ] <- rev(bad[1L, ])
  expect_error(print_mesh_cpp(box$position, bad), "inconsistent face winding")
  p <- rbind(c(0, 0, 0), c(1, 0, 0), c(0, 1, 0), c(0, 0, 1), c(0, -1, 0))
  expect_error(print_mesh_cpp(p, rbind(c(1, 2, 3), c(2, 1, 4), c(1, 2, 5))), "non-manifold edge")
})

test_that("self-intersection predicates include coplanar overlap and folded solids", {
  p <- rbind(c(0, 0, 0), c(2, 0, 0), c(0, 2, 0), c(.2, .2, 0), c(1.2, .2, 0), c(.2, 1.2, 0))
  expect_error(print_mesh_cpp(p, rbind(1:3, 4:6), TRUE), "self-intersection")
  tetra <- rbind(c(0, 0, 0), c(2, 0, 0), c(0, 2, 0), c(0, 0, 2))
  p <- rbind(tetra, tetra + .5)
  faces <- rbind(c(1, 2, 4), c(1, 4, 3), c(2, 3, 4), c(5, 7, 6), c(5, 6, 8), c(5, 8, 7))
  a <- c(1, 3, 2);b <- c(6, 8, 7);next_row <- c(2, 3, 1)
  bridge <- rbind(cbind(a, a[next_row], b[next_row]), cbind(a, b[next_row], b))
  expect_error(print_mesh_cpp(p, rbind(faces, bridge)), "self-intersection")
})

test_that("open sheets gain true millimetre shells and base closures", {
  p <- rbind(c(0, 0, 2), c(1, 0, 2), c(1, 1, 2), c(0, 1, 2))
  source <- list(position = p, index = rbind(c(1L, 1L), c(2L, 3L), c(3L, 4L)), colour = matrix(1, 4L, 4L))
  spec <- print_3d(min_mm = .5, shell_mm = .8)
  shell <- print_parts_3d(source, "sheet", spec)[[1L]]
  expect_equal(diff(range(shell$position[, 3L])), .8)
  expect_equal(printing_volume(shell), .8, tolerance = 1e-12)
  base <- print_parts_3d(source, "height", spec, method = "base")[[1L]]
  expect_equal(range(base$position[, 3L]), c(0, 2))
  expect_equal(printing_volume(base), 2, tolerance = 1e-12)
  expect_true(print_mesh_cpp(shell$position, shell$triangles)$parts[[1L]]$closed)
})

test_that("mesh keys preserve touching closed objects before union", {
  cube <- expand.grid(x = 0:1, y = 0:1, z = 0:1)
  v <- rbind(cube, transform(cube, x = x + 1))
  f <- rbind(c(1, 3, 4, 2), c(5, 6, 8, 7), c(1, 2, 6, 5), c(3, 7, 8, 4), c(1, 5, 7, 3), c(2, 4, 8, 6))
  p <- ggplot2::ggplot() + geom_mesh_3d(data = mesh_3d(v, rbind(f, f + 8L))) + coord_3d()
  scene <- export_scene_3d(p, 1, export_options_3d(FALSE, "tube", "mesh", 1), 400, 400, 96)
  t <- scene$nodes[[1L]]$triangles
  expect_equal(length(unique(t$identity)), 16L)
  geometry <- print_mesh_cpp(t$position, matrix(seq_len(nrow(t$position)), ncol = 3L, byrow = TRUE), identity = t$identity)
  expect_length(geometry$parts, 2L)
  expect_true(all(vapply(geometry$parts, `[[`, logical(1), "closed")))
})

test_that("Manifold unions overlaps and preserves nested cavities", {
  spec <- printing_settings(text = "none", furniture = character())
  a <- print_box_mesh_3d(c(2, 2, 2));b <- print_box_mesh_3d(c(2, 2, 2), c(1, 0, 0))
  parts <- c(print_parts_3d(a, "left", spec), print_parts_3d(b, "right", spec))
  joined <- print_union_3d(parts, list(), spec$python)
  expect_equal(printing_volume(joined), 12, tolerance = 1e-10)
  outer <- print_box_mesh_3d(c(2, 2, 2));inner <- print_box_mesh_3d(c(1, 1, 1), c(.5, .5, .5))
  inner$index <- inner$index[3:1, , drop = FALSE]
  cavity <- export_join_soups_3d(list(outer, inner))
  parts <- print_parts_3d(cavity, "hollow", spec)
  expect_setequal(vapply(parts, `[[`, integer(1), "role"), c(0L, 1L))
  hollow <- print_union_3d(parts, list(), spec$python)
  expect_equal(printing_volume(hollow), 7, tolerance = 1e-10)
  expect_true(all(vapply(print_mesh_cpp(hollow$position, hollow$triangles, TRUE)$parts, `[[`, logical(1), "closed")))
})

test_that("print preparation clamps markers and writes closed STL and coloured 3MF", {
  spec <- printing_settings(size_mm = c(20, 40, 60), min_mm = 1, text = "none", furniture = character())
  p <- ggplot2::ggplot(data.frame(x = 0, y = 0, z = 0), ggplot2::aes(x, y, z = z)) +
    geom_point_3d(size = .001, shape = 18, colour = "#336699") + coord_3d(aspect = c(1, 2, 3))
  scene <- export_scene_3d(p, 1, export_options_3d(FALSE, "tube", "mesh", 1), 400, 400, 96)
  printed <- print_scene_3d(scene, spec)
  mesh <- printed$printing_mesh
  expect_true(all(apply(mesh$position, 2L, function(x) diff(range(x))) >= spec$min_mm))
  expect_gt(printing_volume(mesh), 0)
  dir <- tempfile();dir.create(dir);on.exit(unlink(dir, recursive = TRUE))
  stl <- file.path(dir, "solid.stl");mf <- file.path(dir, "solid.3mf")
  export_3d(p, stl, print = spec)
  expect_equal(file.size(stl), 84 + 50 * length(unique(read_mesh_3d(stl)$.face)))
  export_3d(p, mf, print = spec)
  expect_setequal(utils::unzip(mf, list = TRUE)$Name, c("[Content_Types].xml", "_rels/.rels", "3D/3dmodel.model"))
  skip_if_not_installed("xml2")
  utils::unzip(mf, exdir = dir)
  doc <- xml2::read_xml(file.path(dir, "3D/3dmodel.model"))
  expect_identical(xml2::xml_attr(doc, "unit"), "millimeter")
  palette <- xml2::xml_attr(xml2::xml_find_all(doc, ".//*[local-name()='color']"), "color")
  expect_true("#336699FF" %in% palette)
})

test_that("raised and engraved font outlines produce closed physical plaques", {
  skip_if_not_installed("systemfonts")
  for (mode in c("raised", "engraved")) {
    spec <- printing_settings(text = mode, furniture = character())
    text <- print_text_3d("BO", 18, "sans", "plain", c(0, 0, 2), c(1, 0, 0), c(0, 1, 0), c(0, 0, 0, 1), spec, 1, "label")
    mesh <- print_union_3d(list(), list(text), spec$python)
    check <- print_mesh_cpp(mesh$position, mesh$triangles, TRUE)
    expect_true(all(vapply(check$parts, `[[`, logical(1), "closed")))
    expect_gt(printing_volume(mesh), 0)
    expect_equal(diff(range(mesh$position[, 3L])), 2 * spec$min_mm, tolerance = 1e-10)
  }
})

test_that("invalid layers fail before a model file is written", {
  spec <- printing_settings(text = "none", furniture = character())
  v <- rbind(c(0, 0, 0), c(1, 0, 0), c(0, 1, 0), c(0, 0, 1), c(0, -1, 0))
  p <- ggplot2::ggplot() + geom_mesh_3d(vertices = v, faces = rbind(c(1, 2, 3), c(2, 1, 4), c(1, 2, 5))) + coord_3d()
  file <- tempfile(fileext = ".stl")
  expect_error(export_3d(p, file, print = spec), "layer1_mesh: non-manifold edge")
  expect_false(file.exists(file))
})
