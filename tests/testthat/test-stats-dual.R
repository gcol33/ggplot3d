dual_extract <- function(v, axes, level, method, caps = TRUE)
  ggplot3d:::geometry_isosurface_cpp(axes[[1]], axes[[2]], axes[[3]], as.double(v), level, caps, method)

# Closed, consistently oriented, and every vertex link a single cycle.
dual_manifold <- function(b) {
  f <- matrix(b$.vertex, ncol = 3L, byrow = TRUE)
  e <- rbind(f[, 1:2], f[, 2:3], f[, c(3, 1)])
  counts <- table(paste(pmin(e[, 1], e[, 2]), pmax(e[, 1], e[, 2])))
  apex <- rbind(cbind(f[, 1], f[, 2], f[, 3]), cbind(f[, 2], f[, 3], f[, 1]), cbind(f[, 3], f[, 1], f[, 2]))
  single <- vapply(split(seq_len(nrow(apex)), apex[, 1]), function(rows) {
    nxt <- stats::setNames(apex[rows, 3], apex[rows, 2])
    if (anyDuplicated(names(nxt))) return(FALSE)
    start <- names(nxt)[1L]; at <- start; steps <- 0L
    repeat { at <- as.character(unname(nxt[at])); steps <- steps + 1L; if (is.na(at) || at == start || steps > length(nxt)) break }
    identical(at, start) && steps == length(nxt)
  }, logical(1))
  list(closed = all(counts == 2), oriented = !anyDuplicated(paste(e[, 1], e[, 2])), vertices = all(single),
    chi = length(unique(b$.vertex)) - length(counts) + nrow(f))
}

test_that("surface nets and dual contouring give closed manifolds with caps", {
  set.seed(9)
  for (size in c(2L, 3L, 4L)) for (trial in 1:40) {
    axes <- lapply(rep(size, 3L), seq_len); v <- rnorm(size^3)
    m <- dual_manifold(dual_extract(v, axes, 0, "nets"))
    expect_true(m$closed && m$oriented && m$vertices, label = paste("nets", size, trial))
    d <- dual_extract(v, axes, 0, "dual")
    expect_true(dual_manifold(d)$oriented)
    expect_true(all(d$x >= 1 & d$x <= size & d$y >= 1 & d$y <= size & d$z >= 1 & d$z <= size))
  }
  a <- seq(-1, 1, length.out = 17)
  d <- expand.grid(x = a, y = a, z = a)
  torus <- with(d, (sqrt(x^2 + y^2) - 0.6)^2 + z^2)
  for (method in c("nets", "dual")) {
    m <- dual_manifold(dual_extract(torus, list(a, a, a), 0.06, method, FALSE))
    expect_true(m$closed && m$vertices); expect_equal(m$chi, 0)
  }
})

test_that("dual contouring with function gradients reproduces sharp corners", {
  fun <- function(x, y, z) pmax(abs(x), abs(y), abs(z))
  build <- function(method) ggplot2::ggplot_build(ggplot2::ggplot() +
    geom_isofunction_3d(fun = fun, n = 21, levels = 0.53, method = method, refine = 0) +
    ggplot2::xlim(-1, 1) + ggplot2::ylim(-1, 1) + scale_z_continuous(limits = c(-1, 1)) + coord_3d())$data[[1L]]
  dual <- build("dual"); nets <- build("nets")
  corner <- function(b) max(apply(abs(as.matrix(b[c("x", "y", "z")])), 1, min))
  expect_equal(corner(dual), 0.53, tolerance = 1e-9)
  expect_lt(corner(nets), 0.52)
  expect_lt(max(abs(fun(dual$x, dual$y, dual$z) - 0.53)), 1e-9)
  expect_gt(max(abs(fun(nets$x, nets$y, nets$z) - 0.53)), 0.01)
})

test_that("the QEF solve truncates rank and falls back to the mass point", {
  a <- seq(-1, 1, length.out = 9)
  d <- expand.grid(x = a, y = a, z = a)
  plane <- with(d, x + 0.3 * y)
  b <- dual_extract(plane, list(a, a, a), 0.1, "dual", FALSE)
  expect_equal(b$x + 0.3 * b$y, rep(0.1, nrow(b)), tolerance = 1e-12)
  s <- stats_scene(ggplot2::ggplot(transform(d, value = x^2 + y^2 + z^2), ggplot2::aes(x, y, z = z, value = value)) +
    geom_isosurface_3d(levels = 0.5, method = "dual", caps = TRUE) + coord_3d())
  expect_gt(length(s$packet$faces$primitive_id), 0L)
  expect_silent(validate_scene_packet(s$packet))
})
