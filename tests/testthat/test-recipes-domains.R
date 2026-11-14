test_that("tensor maps preserve eigenvalue lengths and directions", {
  angle <- pi / 6
  rotation <- matrix(c(cos(angle), sin(angle), 0, -sin(angle), cos(angle), 0, 0, 0, 1), 3L)
  tensor <- rotation %*% diag(c(3, 2, 1)) %*% t(rotation)
  d <- data.frame(x = 0, y = 0, z = 0);d$tensor <- list(tensor)
  params <- list(glyph = "ellipsoid", gamma = 3, scale = .5, detail = 1L)
  geometry <- tensor_geometry_3d(d, params)
  map <- geometry$maps[, , 1L]
  expect_equal(tcrossprod(map), tensor %*% tensor / 4, tolerance = 1e-12)
  expect_equal(sort(sqrt(colSums(map^2))), c(.5, 1, 1.5))
  expect_gt(det(map), 0)
  d$tensor <- list(matrix(c(1, 1, 0, 0, 1, 0, 0, 0, 1), 3L))
  expect_error(tensor_geometry_3d(d, params), "symmetric")
  d$tensor <- list(diag(c(1, 1, -1)))
  expect_error(tensor_geometry_3d(d, params), "positive semidefinite")
  expect_error(geom_tensor_3d(gamma = -1), "nonnegative")
})

test_that("Kindlmann superquadrics satisfy their implicit surface equations", {
  sphere <- glyph_mesh_3d("sphere", 1L)
  for (values in list(c(3, 1, 1), c(3, 3, 1), c(3, 2, 1))) {
    shape <- tensor_shape_3d(values, 3)
    v <- superquadric_mesh_3d(shape, 1L)$vertices
    polar <- if (shape[3L]) 1L else 3L
    equator <- setdiff(1:3, polar)
    implicit <- rowSums(abs(v[, equator, drop = FALSE])^(2 / shape[1L]))^(shape[1L] / shape[2L]) + abs(v[, polar])^(2 / shape[2L])
    expect_equal(implicit, rep(1, nrow(v)), tolerance = 1e-12)
    expect_identical(superquadric_mesh_3d(tensor_shape_3d(values, 0), 1L), sphere)
  }
  expect_identical(superquadric_mesh_3d(tensor_shape_3d(c(1, 1, 1), 3), 1L), sphere)
})

test_that("tensor layers reuse scene meshes and train their data extents", {
  d <- data.frame(x = c(0, 4), y = 0, z = 0)
  d$tensor <- list(diag(c(1, .4, .2)), diag(c(1, 1, .3)))
  p <- ggplot2::ggplot(d, ggplot2::aes(x, y, z = z, tensor = tensor)) +
    geom_tensor_3d(glyph = "superquadric", detail = 1) + coord_3d(aspect = "data")
  built <- ggplot2::ggplot_build(p)
  expect_lte(built$data[[1L]]$xmin[1L], -.8)
  expect_gte(built$data[[1L]]$xmax[2L], 4.8)
  expect_silent(ggplot2::ggplotGrob(p))
  expect_error(ggplot2::ggplot_build(p + geom_tensor_3d(gamma = -1)), "nonnegative")
})

test_that("biplots preserve source scores and join groups by identifier", {
  fit <- stats::prcomp(iris[1:4], scale. = TRUE)
  scores <- ordination_scores_3d(fit, 1:3, list())
  expect_equal(as.matrix(scores$sites[1:3]), unname(fit$x[, 1:3]), ignore_attr = TRUE)
  expect_equal(as.matrix(scores$species[1:3]), unname(fit$rotation[, 1:3]), ignore_attr = TRUE)
  groups <- stats::setNames(iris$Species, rownames(iris))
  joined <- ordination_groups_3d(scores$sites, rev(groups))
  expect_identical(joined$.biplot_group, iris$Species)
  expect_error(ordination_groups_3d(scores$sites, unname(groups)), "identify every site")
  expect_error(ordination_scores_3d(fit, c(1, 1, 2), list()), "distinct")
  for (region in c("none", "ellipsoid", "hull")) {
    p <- ggplot2::ggplot() + stat_biplot_3d(fit, groups = rev(groups), region = region,
      loading_scale = 2) + coord_3d()
    expect_silent(ggplot2::ggplotGrob(p))
  }
  label_layer <- function(...) {
    layers <- stat_biplot_3d(fit, ...)
    Filter(function(l) inherits(l$geom, "GeomText"), layers)[[1L]]
  }
  expect_equal(label_layer()$aes_params$size, 4)
  expect_equal(label_layer(label_size = 5.5)$aes_params$size, 5.5)
  expect_error(stat_biplot_3d(fit, label_size = -1), "label_size")
})

test_that("biplot arrows draw their heads and labels sit beyond the tips on the page", {
  scores <- list(sites = cbind(c(-1, 1, 0, 0), c(0, 0, -1, 1), c(-1, 1, 1, -1)),
    species = rbind(a = c(1, 0, 0), b = c(-1, 0, 0), c = c(0, 0, 1), d = c(0.9, 0.1, 0)))
  p <- ggplot2::ggplot() + stat_biplot_3d(scores, arrow_colour = "red") +
    coord_3d(azimuth = -90, elevation = 90, frame = "view")
  packet <- stats_scene(p)$packet
  q <- packet$quadrics
  expect_length(q$style_id, 4L)
  expect_true(all(is.finite(unlist(q[ggplot3d:::quadric_transform_columns]))))
  heads <- packet$styles$fill[match(q$style_id, packet$styles$style_id)]
  expect_true(all(grDevices::col2rgb(heads) == grDevices::col2rgb("red")[, rep(1L, 4L)]))
  text <- packet$text
  labels <- packet$styles$label[match(text$style_id, packet$styles$style_id)]
  a <- match("a", labels); b <- match("b", labels)
  expect_equal(c(text$hjust[a] + text$hjust[b], text$vjust[a] + text$vjust[b]), c(1, 1), tolerance = 1e-6)
  expect_equal(max(abs(c(text$hjust[a], text$vjust[a]) - 0.5)), 0.5, tolerance = 1e-6)
})

test_that("vegan ordinations use the scores generic and its scaling", {
  skip_if_not_installed("vegan")
  fit <- vegan::rda(iris[1:4])
  actual <- ordination_scores_3d(fit, c(3, 1, 2), list(scaling = 2))
  expected <- vegan::scores(fit, choices = c(3, 1, 2), display = c("sites", "species"), scaling = 2)
  expect_equal(as.matrix(actual$sites[1:3]), unname(expected$sites), ignore_attr = TRUE)
  expect_equal(as.matrix(actual$species[1:3]), unname(expected$species), ignore_attr = TRUE)
  sites_only <- ordination_scores_3d(expected$sites, 1:3, list())
  expect_equal(as.matrix(sites_only$sites[1:3]), unname(expected$sites), ignore_attr = TRUE)
  expect_equal(nrow(sites_only$species), 0L)
})

test_that("molecular endpoints use serial keys independently of atom order", {
  skip_if_not_installed("bio3d")
  atoms <- data.frame(eleno = c(40, 10, 90), elesy = c("H", "O", "H"), x = c(.96, 0, -.24),
    y = c(0, 0, .93), z = 0)
  pdb <- structure(list(atom = atoms), class = "pdb")
  fortified <- fortify.pdb(pdb)
  edges <- get_edges_3d()(fortified)
  expect_equal(nrow(edges), 2L)
  expect_true(all(10 %in% c(edges$from[1L], edges$to[1L])))
  expect_equal(edges$x, atoms$x[match(edges$from, atoms$eleno)])
  expect_equal(edges$xend, atoms$x[match(edges$to, atoms$eleno)])
  explicit <- data.frame(from = c(90, 10), to = c(10, 90))
  original <- get_edges_3d()(fortify.pdb(pdb, bonds = explicit))
  pdb$atom <- atoms[c(3, 1, 2), ]
  expect_equal(get_edges_3d()(fortify.pdb(pdb, bonds = explicit)), original)
  expect_equal(nrow(original), 1L)
  expect_error(fortify.pdb(pdb, bonds = data.frame(from = 90, to = 999)), "serial numbers")
  p <- ggplot2::ggplot(fortified) + geom_edge_3d(shape = "tube") +
    geom_sphere_3d(ggplot2::aes(x, y, z = z, radius = covalent_radius * .3, fill = I(colour)), detail = 1) +
    coord_3d(aspect = "data")
  expect_silent(ggplot2::ggplotGrob(p))
  real <- bio3d::read.pdb(system.file("examples/1hel.pdb", package = "bio3d"), verbose = FALSE)
  expect_equal(nrow(ggplot2::fortify(real)), nrow(real$atom))
})
