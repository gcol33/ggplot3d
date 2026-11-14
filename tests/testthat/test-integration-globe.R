test_that("globe charts, height and intrinsic geodesics agree analytically", {
  s <- coord_globe_3d(radius = 2, exaggeration = 3)$space
  X <- s$to_canonical(data.frame(x = c(0, 90), y = 0, z = c(0, 1)), "lonlat")
  expect_equal(unname(s$from_canonical(X, "embedded")), rbind(c(2, 0, 0), c(0, 5, 0)), tolerance = 1e-14)
  m <- s$geodesic(X[1,,drop=FALSE], X[2,,drop=FALSE], 0.5)
  expect_equal(unname(m), matrix(c(1/sqrt(2), 1/sqrt(2), 0, 3.5), 1), tolerance = 1e-14)
  expect_equal(unname(s$distance(X[1,,drop=FALSE], X[2,,drop=FALSE])), sqrt(pi^2+9))
  expect_error(sphere_slerp_3d(matrix(c(1,0,0),1), matrix(c(-1,0,0),1), .5), "Antipodal")
  expect_error(s$to_canonical(data.frame(x=0,y=91,z=0), "lonlat"), "Latitude")
})

test_that("globe paths follow the surface and furniture shares the adapter", {
  d <- data.frame(x = c(170,-170), y = c(0,0), z = 0.05)
  p <- ggplot2::ggplot(d, ggplot2::aes(x,y,z=z)) + geom_path_3d() +
    coord_globe_3d(inner = FALSE, graticule = c(0,0), zlim = c(0,.1))
  out <- stats_scene(p)$packet
  xyz <- do.call(cbind,out$vertices) * 2.2 - 1.1
  expect_gt(nrow(xyz),2)
  expect_equal(sqrt(rowSums(xyz^2)), rep(1.05,nrow(xyz)), tolerance = 1e-10)
  expect_true(all(xyz[,1] < -1))
  p@coordinates <- coord_globe_3d()
  out <- stats_scene(p)$packet
  expect_gt(length(out$faces$offset), 100)
  expect_gt(length(out$polylines$offset), 10)
  expect_silent(validate_scene_packet(out))
})

test_that("globe sf data are reprojected to longitude latitude", {
  skip_if_not_installed("sf")
  d <- sf::st_as_sf(data.frame(x=c(0,10),y=c(0,20)), coords=c("x","y"), crs=4326)
  d <- sf::st_transform(d,3857)
  p <- ggplot2::ggplot(d) + geom_sf_3d() + coord_globe_3d(inner=FALSE,graticule=c(0,0))
  b <- ggplot2::ggplot_build(p)
  expect_equal(b$data[[1]]$x,c(0,10),tolerance=1e-8)
  expect_equal(b$data[[1]]$y,c(0,20),tolerance=1e-8)
  expect_equal(b$data[[1]]$z,c(0,0))
  expect_silent(stats_scene(p))
})
