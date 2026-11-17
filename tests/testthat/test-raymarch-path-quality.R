test_that("physical controls validate their complete domains", {
  for (name in c("roughness", "metallic", "transmission")) for (value in list(-1, 2, NA_real_, Inf, c(0, 1), "x"))
    expect_error(do.call(material_3d, setNames(list(value), name)), name)
  expect_error(material_3d(ior = 0), "ior")
  expect_error(material_3d(emission = "not-a-colour"), "emission")
  expect_error(material_3d(emission_strength = -1), "emission_strength")
  expect_error(backend_3d("pathtrace", noise_threshold = -1), "noise_threshold")
  expect_error(backend_3d("pathtrace", min_samples = 1.5), "min_samples")
  expect_error(backend_3d("pathtrace", denoise = NA), "denoise")
  expect_error(backend_3d("pathtrace", exposure = Inf), "exposure")
  expect_error(backend_3d("pathtrace", tone = "unknown"), "arg")
  expect_error(geom_volume_3d(g = 1), "g")
  expect_equal(material_3d("glass")$transmission, 1)
  expect_equal(material_3d("mirror")$metallic, 1)
  expect_equal(material_3d()$specular, 0)
})

test_that("Sobol half streams are stratified and deterministic", {
  a <- ggplot3d:::raymarch_sampler_cpp(512L, 12L, 19L)
  expect_identical(a, ggplot3d:::raymarch_sampler_cpp(512L, 12L, 19L))
  expect_false(identical(a, ggplot3d:::raymarch_sampler_cpp(512L, 12L, 20L)))
  expect_true(all(a > 0 & a < 1))
  for (half in 1:2) for (d in 1:12) expect_equal(sort(floor(a[seq(half, 512, 2), d] * 256)), 0:255)
})

test_that("BSDF samples conserve energy and match their evaluated densities", {
  skip_on_cran()
  probe <- function(material, colour = rep(1, 3), view = c(0, 0, 1)) ggplot3d:::raymarch_bsdf_cpp(
    ggplot3d:::kernel_materials_3d(ggplot3d:::material_table_3d(material)), colour, view, 32768L, 19L)
  for (roughness in c(0, .2, .6, 1)) {
    x <- probe(material_3d(roughness = roughness, metallic = 1))
    expect_equal(colMeans(x$weight), rep(1, 3), tolerance = .025)
    if (roughness > 0) {
      take <- x$pdf > 0
      expect_equal(x$weight[take, ], x$value[take, ] * abs(x$direction[take, 3]) / x$pdf[take], tolerance = 1e-12)
    }
  }
  diffuse <- probe(material_3d(), c(.2, .4, .8))
  expect_equal(colMeans(diffuse$weight), c(.2, .4, .8), tolerance = 1e-12)
  for (roughness in c(0, .3, .8)) {
    glass <- probe(material_3d("glass", roughness = roughness))
    expect_true(all(is.finite(glass$weight)))
    expect_gt(mean(glass$direction[, 3] < 0), .5)
    expect_lt(max(colMeans(glass$weight)), 1.02)
  }
})

test_that("white diffuse and rough metal spheres pass the furnace", {
  skip_on_cran()
  spec <- list(kind = "sdf", tape = ggplot3d:::sdf_compile_3d(sdf_sphere(c(.5, .5, .5), .25)),
    ranges = list(x = c(0, 1), y = c(0, 1), z = c(0, 1)), fill = "white", tolerance = 1e-6, max_steps = 512L)
  for (material in list(material_3d(), material_3d(metallic = 1, roughness = .6))) {
    spec$material <- material
    result <- pathtrace_test_scene(backend_3d("pathtrace", samples = 2048, bounces = 3), new_scene_packet(), list(spec))
    expect_equal(result$linear[41, ], rep(1, 3), tolerance = .04)
  }
  spec$material <- material_3d("glass")
  glass <- pathtrace_test_scene(backend_3d("pathtrace", samples = 1024, bounces = 12), new_scene_packet(), list(spec))
  expect_gt(min(glass$linear[41, ]), .9)
})

test_that("MIS, light-only and BSDF-only estimates agree", {
  skip_on_cran()
  environment <- array(1, c(8, 16, 3)); environment[1:4, , 1] <- 3
  light <- light_3d(light_environment_3d(environment), ambient = 0)
  values <- lapply(0:2, function(estimator) {
    backend <- backend_3d("pathtrace", samples = 4096, bounces = 1, seed = 33)
    backend$estimator <- estimator
    pathtrace_test_scene(backend, light = light)$linear[41, ]
  })
  expect_equal(values[[1]], values[[2]], tolerance = .04)
  expect_equal(values[[1]], values[[3]], tolerance = .04)
})

test_that("adaptive pixels stop independently across progressive batches", {
  skip_on_cran()
  backend <- backend_3d("pathtrace", samples = 64, noise_threshold = .01, min_samples = 8,
    progressive = TRUE, batch_size = 3)
  a <- pathtrace_test_scene(backend)
  b <- pathtrace_test_scene(backend)
  expect_equal(a$counts[41], 8L)
  expect_equal(b$counts[41], 8L)
  expect_true(all(a$counts <= 64))
  expect_equal(a$linear[41, ], rep(1, 3), tolerance = 1e-12)
  expect_equal(a$variance[41, ], rep(0, 4), tolerance = 1e-12)
})

test_that("NFOR reduces error on a lit surface against a reference", {
  skip_on_cran()
  env <- array(.05, c(8, 16, 3)); env[2:3, 4:6, ] <- 6
  light <- light_3d(light_environment_3d(env), ambient = 0)
  reference <- pathtrace_test_scene(backend_3d("pathtrace", samples = 4096, bounces = 1), light = light)
  raw <- pathtrace_test_scene(backend_3d("pathtrace", samples = 16, bounces = 1, seed = 37), light = light)
  clean <- pathtrace_test_scene(backend_3d("pathtrace", samples = 16, bounces = 1, seed = 37, denoise = TRUE), light = light)
  take <- reference$sum[, 4] == 4096
  expect_lt(mean((clean$linear[take, ] - reference$linear[take, ])^2), mean((raw$linear[take, ] - reference$linear[take, ])^2))
  expect_identical(raw$sum, clean$sum)
  expect_true(all(is.finite(clean$features)))
})

test_that("homogeneous ratio and delta tracking reproduce Beer transmittance", {
  skip_on_cran()
  for (sigma in c(.1, 1, 3)) {
    x <- ggplot3d:::raymarch_medium_cpp(pathtrace_test_volume(sigma), c(.5, .5, 0), c(0, 0, 1), 1, 32768L, 7L)
    expect_equal(colMeans(x), rep(exp(-sigma), 2), tolerance = .012)
  }
})

test_that("path tracing accepts scattering and iso volumes and rejects MIP", {
  skip_on_cran()
  backend <- backend_3d("pathtrace", samples = 8, bounces = 4)
  volume <- pathtrace_test_volume()
  render <- pathtrace_test_scene(backend, new_scene_packet(), list(volume))
  expect_gt(sum(render$sum[, 4]), 0)
  expect_true(all(is.finite(render$linear)))
  volume$mode <- 2L; volume$value <- rep(c(0, 2), each = 4)
  expect_gt(sum(pathtrace_test_scene(backend, new_scene_packet(), list(volume))$sum[, 4]), 0)
  volume$mode <- 1L
  expect_error(pathtrace_test_scene(backend, new_scene_packet(), list(volume)), "MIP")
})

test_that("physical material mapping is shared by shading and the viewer", {
  material <- material_3d(roughness = .5, metallic = .7, transmission = .2, emission = "red", emission_strength = 2)
  table <- ggplot3d:::material_table_3d(material)
  kernel <- ggplot3d:::kernel_materials_3d(table)
  viewer <- ggplot3d:::view_materials_3d(list(list()), data.frame(material_id = 1L), table)[[1]]
  expect_equal(kernel$shininess, 30)
  expect_equal(kernel$diffuse, .24)
  expect_equal(kernel$specular_colour, .3)
  expect_equal(kernel$emission[1, ], c(2, 0, 0))
  expect_equal(viewer$material, c(kernel$ambient, kernel$diffuse, kernel$specular, kernel$shininess))
  expect_equal(viewer$emission, c(2, 0, 0))
  expect_no_error(new_scene_packet(materials = table))
  table$roughness <- -1
  expect_error(new_scene_packet(materials = table), "roughness")
})

test_that("tone transforms operate in linear light", {
  x <- matrix(c(0, .18, 1, 3, 10, 100), ncol = 3)
  expect_equal(ggplot3d:::pathtrace_tone_3d(x, 0, "clip"), pmin(x, 1))
  expect_equal(ggplot3d:::pathtrace_tone_3d(x, 1, "clip"), pmin(2 * x, 1))
  for (tone in c("aces", "agx")) {
    y <- ggplot3d:::pathtrace_tone_3d(x, 0, tone)
    expect_true(all(is.finite(y) & y >= 0 & y <= 1))
    expect_false(identical(y, pmin(x, 1)))
  }
})

test_that("legacy material rows retain matte defaults when combined with glass", {
  legacy <- ggplot3d:::material_table_3d(material_3d())
  legacy <- legacy[c("material_id", "ambient", "diffuse", "specular", "shininess", "specular_colour", "back")]
  glass <- ggplot3d:::material_table_3d(material_3d("glass", emission = "red"))
  merged <- ggplot3d:::merge_scene_packets(list(new_scene_packet(materials = legacy), new_scene_packet(materials = glass)), 1L)
  expect_equal(merged$materials$transmission, c(0, 1))
  expect_equal(merged$materials$roughness, c(1, 0))
  expect_equal(merged$materials$emission, c(NA_character_, "red"))
  packet <- ref_packet(faces = list(ref_quad(.2)))
  packet$materials <- legacy; packet$styles$shade <- TRUE
  inherited <- ggplot3d:::light_material_packet_3d(packet, light_3d(material = material_3d("glass")))
  expect_equal(inherited$materials$transmission, c(0, 1))
  expect_equal(inherited$styles$material_id, 2L)
})

test_that("sphere materials survive analytic and tessellated scene paths", {
  skip_on_cran()
  plot <- ggplot2::ggplot(data.frame(x = .5, y = .5, z = .5), ggplot2::aes(x, y, z = z)) +
    geom_sphere_3d(radius = .2, fill = "white", colour = NA, material = material_3d("glass")) +
    coord_3d()
  packet <- stats_scene(plot)$packet
  expect_equal(packet$materials$transmission, 1)
  expect_true(all(packet$styles$material_id == 1L))
  finite <- ggplot3d:::scene_finite_packet_3d(packet)
  expect_gt(length(finite$faces$offset), 0)
  expect_equal(finite$materials$transmission, 1)
})
