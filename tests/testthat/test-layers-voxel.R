voxel_faces <- function(material, dims, translucent = FALSE, greedy = TRUE)
  ggplot3d:::voxel_faces_cpp(as.integer(dims), as.integer(material), c(FALSE, rep_len(translucent, max(material))), greedy)

voxel_area <- function(r) sum((r[, "u1"] - r[, "u0"]) * (r[, "v1"] - r[, "v0"]))

test_that("hidden faces are culled and greedy meshing keeps the visible area", {
  solid <- voxel_faces(rep(1L, 27), c(3, 3, 3))
  expect_equal(nrow(solid), 6L)
  expect_equal(voxel_area(solid), 54)
  plain <- voxel_faces(rep(1L, 27), c(3, 3, 3), greedy = FALSE)
  expect_equal(nrow(plain), 54L)
  set.seed(3)
  m <- sample(0:2, 6^3, replace = TRUE, prob = c(0.4, 0.3, 0.3))
  merged <- voxel_faces(m, c(6, 6, 6)); unmerged <- voxel_faces(m, c(6, 6, 6), greedy = FALSE)
  expect_equal(voxel_area(merged), voxel_area(unmerged))
  expect_lt(nrow(merged), nrow(unmerged))
  # Opaque neighbours of different classes hide their shared face; a
  # translucent neighbour shows the face behind it.
  two <- c(1L, 2L)
  expect_equal(nrow(voxel_faces(two, c(2, 1, 1))), 10L)
  expect_equal(nrow(voxel_faces(two, c(2, 1, 1), translucent = c(FALSE, TRUE))), 11L)
  expect_equal(nrow(voxel_faces(two, c(2, 1, 1), translucent = TRUE)), 12L)
  # Each face points away from its voxel.
  one <- voxel_faces(1L, c(1, 1, 1))
  expect_setequal(paste(one[, "axis"], one[, "side"], one[, "plane"]), c("0 -1 0", "0 1 1", "1 -1 0", "1 1 1", "2 -1 0", "2 1 1"))
})

test_that("voxel layers emit outward faces and bin counts", {
  d <- expand.grid(x = 1:3, y = 1:3, z = 1:3)
  s <- stats_scene(ggplot2::ggplot(d, ggplot2::aes(x, y, z = z)) + geom_voxel_3d() + coord_3d())
  expect_length(s$packet$faces$primitive_id, 6L)
  expect_silent(validate_scene_packet(s$packet))
  b <- s$built$data[[1L]]
  expect_equal(range(b$xmin), c(0.5, 2.5)); expect_equal(range(b$zmax), c(1.5, 3.5))
  d$class <- ifelse(d$z == 3, "top", "rest")
  s <- stats_scene(ggplot2::ggplot(d, ggplot2::aes(x, y, z = z, fill = class)) + geom_voxel_3d() + coord_3d())
  expect_length(s$packet$faces$primitive_id, 10L)
  set.seed(5)
  p <- data.frame(x = runif(200), y = runif(200), z = runif(200))
  b <- ggplot2::ggplot_build(ggplot2::ggplot(p, ggplot2::aes(x, y, z = z)) + stat_bin_3d(bins = 3) + coord_3d())$data[[1L]]
  expect_equal(sum(b$count), 200)
  expect_lte(nrow(b), 27L)
  expect_equal(sum(b$density * (b$xmax - b$xmin) * (b$ymax - b$ymin) * (b$zmax - b$zmin)), 1)
  expect_error(stats_scene(ggplot2::ggplot(rbind(d, d), ggplot2::aes(x, y, z = z)) + geom_voxel_3d() + coord_3d()), "unique")
})
