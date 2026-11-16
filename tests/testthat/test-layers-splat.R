splat_spec <- function(position = c(.5, .5, .5), scale = rep(.08, 3), colour = c(1, 0, 0, .8)) {
  list(position = matrix(position, ncol = 3),
    covariance = ggplot3d:::splat_covariance_cpp(matrix(scale, ncol = 3), matrix(c(1, 0, 0, 0), 1), rep(1, 3)),
    rgba = matrix(colour, 1), sh = matrix(numeric(), 1, 0), span = rep(1, 3), layer_id = 1L)
}
splat_packet <- function(spec = splat_spec()) {
  p <- ggplot3d:::new_scene_packet(); attr(p, "splats") <- list(spec); p
}
splat_camera <- function(...) ggplot3d:::new_camera_3d(azimuth = -90, elevation = 90, ...)
splat_viewport <- function(n = 101) ggplot3d:::new_scene_viewport(25.4, 25.4, n)
splat_rgba <- function(result) {
  packed <- as.double(result$image); packed[packed < 0] <- packed[packed < 0] + 2^32
  vapply(0:3, function(k) (packed %/% 256^k) %% 256 / 255, numeric(length(packed)))
}

test_that("capture activations and quaternion normalisation preserve their values", {
  d <- data.frame(x = 0, y = 0, z = 0, scale_0 = log(2), scale_1 = log(3), scale_2 = log(4),
    opacity = log(3), rot_0 = 2, rot_1 = 0, rot_2 = 0, rot_3 = 0, f_dc_0 = 1, f_dc_1 = 0, f_dc_2 = -1)
  a <- ggplot3d:::splat_aesthetics_3d(d[c("x", "y", "z")], d, NULL, list())
  expect_equal(unname(unlist(a[c("scale_x", "scale_y", "scale_z")])), c(2, 3, 4))
  expect_equal(as.numeric(a$alpha), .75)
  cov <- ggplot3d:::splat_covariance_cpp(matrix(c(2, 3, 4), 1), matrix(c(2, 0, 0, 0), 1), c(1, 1, 1))
  expect_equal(as.vector(cov), c(4, 0, 0, 9, 0, 16))
  rotated <- ggplot3d:::splat_covariance_cpp(matrix(c(2, 3, 4), 1), matrix(c(2, 0, 0, 2), 1), c(1, 1, 1))
  expect_equal(as.vector(rotated), c(9, 0, 0, 4, 0, 16), tolerance = 1e-12)
  expect_error(ggplot3d:::splat_covariance_cpp(matrix(rep(1, 3), 1), matrix(rep(0, 4), 1), rep(1, 3)), "quaternion")
  explicit <- ggplot3d:::splat_aesthetics_3d(transform(d[c("x", "y", "z")], colour = "red", alpha = .2), d, NULL, list(size = 1))
  expect_false(any(grepl("^\\.splat_", names(explicit))))
  expect_equal(explicit$alpha, .2)
  expect_null(explicit$scale_x)
})

test_that("SH degree zero and one use the camera-to-splat direction", {
  s <- splat_spec(); s$sh <- matrix(c(1, 0, -1), 1)
  resolved <- ggplot3d:::splat_resolve_cpp(s, splat_camera(), splat_viewport())
  expect_equal(as.vector(resolved$rgba[1, 1:3]), .5 + .28209479177387814 * c(1, 0, -1))
  s$sh <- matrix(c(0, 0, .2, 0, 0, 0, -.3, 0, 0, 0, .4, 0), 1)
  resolved <- ggplot3d:::splat_resolve_cpp(s, splat_camera(), splat_viewport())
  expect_equal(as.vector(resolved$rgba[1, 1:3]), .5 - .4886025119029199 * c(.2, -.3, .4))
  s$sh[] <- 10
  expect_true(all(ggplot3d:::splat_resolve_cpp(s, splat_camera(), splat_viewport())$rgba[, 1:3] %in% c(0, 1)))
})

test_that("projected covariance agrees with J W Sigma W transpose J transpose", {
  camera <- ggplot3d:::new_camera_3d(azimuth = 27, elevation = 31, projection = "perspective", aspect = c(1, 2, 3))
  viewport <- splat_viewport()
  s <- splat_spec(c(.3, .4, .6), c(.04, .08, .12))
  p <- ggplot3d:::splat_project_cpp(s, camera, viewport)
  resolved <- ggplot3d:::scene_compile(ggplot3d:::new_scene_packet(), camera, viewport)$camera
  W <- rbind(resolved$right, resolved$up, resolved$forward)
  centre <- s$position[1, ] * resolved$model_scale
  xyz <- as.vector(W %*% (centre - resolved$eye))
  focal <- 101 / 2 * camera$zoom / tan(camera$fov * pi / 360)
  J <- rbind(c(focal / xyz[3], 0, -focal * xyz[1] / xyz[3]^2),
             c(0, focal / xyz[3], -focal * xyz[2] / xyz[3]^2))
  Sigma <- diag((c(.04, .08, .12) * resolved$model_scale)^2)
  expected <- J %*% W %*% Sigma %*% t(W) %*% t(J) + diag(.3, 2)
  expect_equal(as.vector(p[1, 4:6]), c(expected[1, 1], expected[1, 2], expected[2, 2]), tolerance = 1e-10)
})

test_that("isotropic raster alpha follows the analytic Gaussian", {
  skip_on_cran()
  s <- splat_spec(); camera <- splat_camera(); viewport <- splat_viewport()
  projected <- ggplot3d:::splat_project_cpp(s, camera, viewport)
  r <- ggplot3d:::raster_scene_3d(splat_packet(s), camera, viewport, 1L)
  alpha <- matrix(splat_rgba(r)[, 4], r$height, r$width, byrow = TRUE)
  dx <- (seq_len(r$width) - .5) - projected[1, 1]
  expected <- .8 * exp(-.5 * dx^2 / projected[1, 4]); expected[dx^2 > 9 * projected[1, 4]] <- 0
  expect_equal(alpha[51, ], expected, tolerance = 1 / 255)
  expect_gt(sum(alpha), 1)
})

test_that("opaque surfaces and Gaussian layers composite in depth order", {
  skip_on_cran()
  source(testthat::test_path("..", "fixtures", "scenes", "builders.R"), local = TRUE)
  render <- function(z) {
    p <- ref_packet(faces = list(ref_quad(z))); p$styles$fill <- "blue"
    p <- ggplot3d:::merge_scene_packets(list(p, splat_packet()), 1L)
    ggplot3d:::raster_scene_3d(p, splat_camera(), splat_viewport(), 1L, blend = "srgb")
  }
  front <- splat_rgba(render(.8))[5101, ]; back <- splat_rgba(render(.2))[5101, ]
  expect_equal(front, c(0, 0, 1, 1), tolerance = 1 / 255)
  expect_equal(back, c(.8, 0, .2, 1), tolerance = 1 / 255)
})

test_that("splat renders are deterministic across worker counts", {
  skip_on_cran()
  s <- splat_spec(); s$position <- rbind(c(.4, .5, .5), c(.6, .5, .6));
  s$covariance <- s$covariance[c(1, 1), ]; s$rgba <- s$rgba[c(1, 1), ]; s$sh <- matrix(numeric(), 2, 0)
  old <- options(ggplot3d.threads = 1L); on.exit(options(old))
  one <- ggplot3d:::raster_scene_3d(splat_packet(s), splat_camera(), splat_viewport(), 2L)
  options(ggplot3d.threads = 4L)
  four <- ggplot3d:::raster_scene_3d(splat_packet(s), splat_camera(), splat_viewport(), 2L)
  expect_identical(one$image, four$image)
})

test_that("the registered layer retains identity colours and raw capture columns", {
  d <- data.frame(x = c(0, 1), y = c(0, 1), z = c(0, 1), colour = c("#123456", "#ABCDEF"))
  p <- ggplot2::ggplot(d, ggplot2::aes(x, y, z = z, colour = I(colour))) + geom_splat_3d(size = .1) + coord_3d()
  spec <- attr(stats_scene(p)$packet, "splats")[[1]]
  expect_equal(spec$rgba[, 1:3], t(grDevices::col2rgb(d$colour) / 255), ignore_attr = TRUE)
  expect_equal(nrow(spec$covariance), 2L)
  d$scale_0 <- d$scale_1 <- d$scale_2 <- log(.1); d$opacity <- qlogis(.7)
  d$rot_0 <- 2; d$rot_1 <- d$rot_2 <- d$rot_3 <- 0
  d$f_dc_0 <- .2; d$f_dc_1 <- .3; d$f_dc_2 <- .4
  p <- ggplot2::ggplot(d, ggplot2::aes(x, y, z = z)) + geom_splat_3d() + coord_3d()
  spec <- attr(stats_scene(p)$packet, "splats")[[1]]
  expect_equal(spec$rgba[, 4], rep(.7, 2))
  expect_equal(spec$sh, matrix(rep(c(.2, .3, .4), each = 2), 2))
})

test_that("vector and hybrid splat panels share the raster image", {
  skip_on_cran()
  skip_if_not_installed("ragg")
  capture <- function(type, alpha) {
    d <- data.frame(x = c(.4, .6), y = .5, z = .5)
    p <- ggplot2::ggplot(d, ggplot2::aes(x, y, z = z)) + geom_splat_3d(size = .12, colour = "red") +
      geom_plane_3d(normal = c(1, 0, 1), point = c(.5, .5, .5), fill = "blue", alpha = alpha) +
      ggplot2::xlim(0, 1) + ggplot2::ylim(0, 1) + scale_z_continuous(limits = c(0, 1)) +
      coord_3d(backend = backend_3d(type, dpi = 96, antialias = 1))
    file <- tempfile(fileext = ".png"); ragg::agg_png(file, 300, 300, res = 72)
    on.exit({grDevices::dev.off(); unlink(file)})
    g <- grid::grid.force(ggplot2::ggplotGrob(p))
    find <- function(g) {
      if (!is.null(g$raster_result)) return(list(g$raster_result$image))
      unlist(lapply(c(as.list(g$children), g$grobs), find), recursive = FALSE)
    }
    find(g)[[1]]
  }
  for (alpha in c(1, .4)) {
    raster <- capture("raster", alpha)
    expect_identical(capture("hybrid", alpha), raster)
    expect_identical(capture("vector", alpha), raster)
  }
})

test_that("the path tracer renders Gaussian albedo and point exports retain splats", {
  skip_on_cran()
  packet <- splat_packet(splat_spec(scale = rep(.18, 3)))
  camera <- splat_camera(); viewport <- splat_viewport(25)
  traced <- ggplot3d:::pathtrace_scene_3d(packet, list(), camera, viewport,
    backend_3d("pathtrace", samples = 16, bounces = 2, seed = 1), NULL)
  rgba <- splat_rgba(traced)
  expect_gt(sum(rgba[, 4]), 0)
  expect_gt(sum(rgba[, 1] * rgba[, 4]), sum(rgba[, 3] * rgba[, 4]))
  points <- ggplot3d:::splat_points_packet_3d(packet, camera, viewport)
  expect_length(points$markers$primitive_id, 1L)
  expect_null(attr(points, "splats"))
})

test_that("capture degree limits and reversed axes preserve covariance and SH channels", {
  d <- data.frame(x = c(0, 1), y = c(0, 1), z = c(0, 1),
    scale_0 = log(.1), scale_1 = log(.2), scale_2 = log(.3),
    rot_0 = cos(pi / 8), rot_1 = 0, rot_2 = 0, rot_3 = sin(pi / 8),
    f_dc_0 = .1, f_dc_1 = .2, f_dc_2 = .3)
  for (k in 0:44) d[[paste0("f_rest_", k)]] <- k / 100
  plot <- function(degree) ggplot2::ggplot() + geom_splat_3d(data = d, sh_degree = degree) + coord_3d()
  full <- attr(stats_scene(plot(NULL))$packet, "splats")[[1]]
  zero <- attr(stats_scene(plot(0))$packet, "splats")[[1]]
  one <- attr(stats_scene(plot(1))$packet, "splats")[[1]]
  expect_equal(dim(full$sh), c(2, 48))
  expect_equal(zero$sh[1, ], c(.1, .2, .3))
  expect_equal(one$sh[1, ], c(.1, 0, .01, .02, .2, .15, .16, .17, .3, .30, .31, .32))
  reversed <- attr(stats_scene(plot(NULL) + ggplot2::scale_x_reverse())$packet, "splats")[[1]]
  expect_equal(reversed$covariance[, 2], -full$covariance[, 2])
  expect_equal(reversed$span[1], -full$span[1])
  expect_error(geom_splat_3d(sh_degree = 4), "sh_degree")
  expect_error(suppressWarnings(stats_scene(plot(NULL) + ggplot2::scale_x_log10())), "affine")
})

test_that("colour and fill train ordinary point legends", {
  d <- data.frame(x = 1:3, y = 1:3, z = 1:3, label = letters[1:3])
  for (aes in c("colour", "fill")) {
    mapping <- ggplot2::aes(x, y, z = z); mapping[[aes]] <- rlang::quo(label)
    p <- ggplot2::ggplot(d, mapping) + geom_splat_3d(size = .1) + coord_3d()
    built <- ggplot2::ggplot_build(p)
    expect_equal(built$plot@scales$get_scales(aes)$get_limits(), letters[1:3])
    expect_s3_class(ggplot2::ggplotGrob(p), "gtable")
  }
})

test_that("viewer and exports contain point representations and plotter omits splats", {
  skip_on_cran()
  skip_if_not_installed("htmlwidgets")
  p <- ggplot2::ggplot() + geom_splat_3d(data = data.frame(x = 0:1, y = 0:1, z = 0:1), colour = "red") + coord_3d()
  widget <- view_3d(p, width = 200, height = 200)
  expect_length(decode_column(widget$x$panels[[1]]$scene$markers$primitive_id), 2L)
  file <- tempfile(fileext = ".ply"); on.exit(unlink(file))
  export_3d(p, file, points = "point", lines = "line")
  expect_equal(nrow(read_points_3d(file)), 2L)
  p@coordinates$config$backend <- backend_3d("plotter")
  grDevices::pdf(tempfile(fileext = ".pdf")); on.exit(grDevices::dev.off(), add = TRUE)
  expect_silent(grid::grid.force(ggplot2::ggplotGrob(p)))
})

test_that("coincident traced Gaussians each contribute opacity", {
  skip_on_cran()
  spec <- splat_spec(scale = rep(10, 3), colour = c(1, 0, 0, .5))
  packet <- splat_packet(spec); attr(packet, "splats") <- list(spec, spec)
  result <- ggplot3d:::pathtrace_scene_3d(packet, list(), splat_camera(), splat_viewport(5),
    backend_3d("pathtrace", samples = 1024, bounces = 2, seed = 7), NULL)
  expect_equal(splat_rgba(result)[13, 4], .75, tolerance = .035)
})
