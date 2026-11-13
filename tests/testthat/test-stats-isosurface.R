iso_topology <- function(b) {
  f <- matrix(b$.vertex, ncol = 3L, byrow = TRUE)
  e <- rbind(f[, 1:2], f[, 2:3], f[, c(3, 1)])
  counts <- table(paste(pmin(e[, 1], e[, 2]), pmax(e[, 1], e[, 2])))
  list(chi = length(unique(b$.vertex)) - length(counts) + nrow(f),
    closed = all(counts == 2), oriented = !anyDuplicated(paste(e[, 1], e[, 2])))
}

iso_extract <- function(v, axes, level, method, caps = TRUE)
  ggplot3d:::geometry_isosurface_cpp(axes[[1]], axes[[2]], axes[[3]], as.double(v), level, caps, method)

test_that("both methods give closed oriented surfaces on random fields", {
  set.seed(4)
  for (size in c(2L, 2L, 3L)) for (trial in 1:60) {
    axes <- lapply(rep(size, 3L), seq_len)
    v <- rnorm(size^3)
    for (method in c("cubes", "tetra")) {
      t <- iso_topology(iso_extract(v, axes, 0, method))
      expect_true(t$closed && t$oriented, label = paste(method, size, trial))
      expect_true(iso_topology(iso_extract(v, axes, 0, method, FALSE))$oriented)
    }
  }
})

test_that("marching cubes resolves face and interior ambiguities like the trilinear interpolant", {
  unit <- list(1:2, 1:2, 1:2)
  # Two diagonally opposite high corners joined through the cell centre: the
  # capped sublevel region is the cube minus a tunnel, a solid torus.
  expect_equal(iso_topology(iso_extract(c(1, rep(-0.2, 6), 1), unit, 0, "cubes"))$chi, 0)
  expect_equal(iso_topology(iso_extract(c(0.2, rep(-1, 6), 0.2), unit, 0, "cubes"))$chi, 2)
  # Face saddle above and below the level.
  expect_equal(iso_topology(iso_extract(c(1, -0.2, -0.2, 1, rep(-1, 4)), unit, 0, "cubes", FALSE))$chi, 1)
  expect_equal(iso_topology(iso_extract(c(0.2, -1, -1, 0.2, rep(-1, 4)), unit, 0, "cubes", FALSE))$chi, 2)
  # Two spheres apart and merged through a saddle, and a torus.
  a <- seq(-1, 1, length.out = 21)
  d <- expand.grid(x = a, y = a, z = a)
  two <- with(d, pmin(sqrt((x - 0.4)^2 + y^2 + z^2), sqrt((x + 0.4)^2 + y^2 + z^2)))
  for (method in c("cubes", "tetra")) {
    apart <- iso_topology(iso_extract(two, list(a, a, a), 0.35, method, FALSE))
    joined <- iso_topology(iso_extract(two, list(a, a, a), 0.45, method, FALSE))
    expect_equal(c(apart$chi, joined$chi), c(4, 2))
    expect_true(apart$closed && joined$closed)
    torus <- with(d, (sqrt(x^2 + y^2) - 0.6)^2 + z^2)
    t <- iso_topology(iso_extract(torus, list(a, a, a), 0.06, method, FALSE))
    expect_equal(t$chi, 0); expect_true(t$closed)
  }
})

test_that("random-field Euler characteristics match a refined trilinear reference", {
  set.seed(11)
  refine <- function(v, f) {
    arr <- array(v, c(2, 2, 2)); g <- seq(0, 1, length.out = f + 1L)
    p <- as.matrix(expand.grid(g, g, g)); out <- 0
    for (c in 0:7) {
      b <- c(c %% 2, (c %/% 2) %% 2, c %/% 4)
      w <- (if (b[1]) p[, 1] else 1 - p[, 1]) * (if (b[2]) p[, 2] else 1 - p[, 2]) * (if (b[3]) p[, 3] else 1 - p[, 3])
      out <- out + w * arr[b[1] + 1, b[2] + 1, b[3] + 1]
    }
    list(v = out, axes = list(g, g, g))
  }
  margin <- function(v) {
    a <- array(v, c(2, 2, 2)); m <- Inf
    faces <- list(a[1, , ], a[2, , ], a[, 1, ], a[, 2, ], a[, , 1], a[, , 2])
    for (q in faces) {
      s <- c(q[1, 1], q[2, 1], q[2, 2], q[1, 2])
      if ((s[1] >= 0) == (s[3] >= 0) && (s[2] >= 0) == (s[4] >= 0) && (s[1] >= 0) != (s[2] >= 0))
        m <- min(m, abs((s[1] * s[3] - s[2] * s[4]) / (s[1] + s[3] - s[2] - s[4])))
    }
    m
  }
  compared <- 0
  for (trial in 1:200) {
    v <- rnorm(8)
    if (margin(v) < 0.05) next
    r <- refine(v, 24L)
    expect_equal(iso_topology(iso_extract(v, list(1:2, 1:2, 1:2), 0, "cubes"))$chi,
      iso_topology(iso_extract(r$v, r$axes, 0, "tetra"))$chi, label = paste("trial", trial))
    compared <- compared + 1
  }
  expect_gt(compared, 150)
})

test_that("levels share one sweep and match separate extraction", {
  a <- seq(-1, 1, length.out = 9)
  d <- expand.grid(x = a, y = a, z = a); v <- with(d, x^2 + y^2 + z^2)
  for (method in c("cubes", "tetra")) {
    both <- iso_extract(v, list(a, a, a), c(0.3, 0.7), method, TRUE)
    for (level in c(0.3, 0.7)) {
      one <- iso_extract(v, list(a, a, a), level, method, TRUE)
      part <- both[both$level == level, ]
      expect_equal(nrow(part), nrow(one))
      expect_equal(as.matrix(part[c("x", "y", "z")]), as.matrix(one[c("x", "y", "z")]), ignore_attr = TRUE)
    }
  }
})

test_that("regularised tetrahedra remove sliver triangles and stay on the surface", {
  a <- seq(-1, 1, length.out = 25)
  d <- expand.grid(x = a, y = a, z = a); v <- with(d, x^2 + y^2 + z^2)
  b <- iso_extract(v, list(a, a, a), 0.5, "tetra", FALSE)
  cubes <- iso_extract(v, list(a, a, a), 0.5, "cubes", FALSE)
  expect_lt(nrow(b), nrow(cubes))
  xyz <- as.matrix(b[c("x", "y", "z")]); i <- seq(1L, nrow(xyz), 3L)
  angle <- function(o, u, w) { u <- u - o; w <- w - o; acos(rowSums(u * w) / sqrt(rowSums(u^2) * rowSums(w^2))) }
  smallest <- pmin(angle(xyz[i, ], xyz[i + 1, ], xyz[i + 2, ]), angle(xyz[i + 1, ], xyz[i + 2, ], xyz[i, ]),
    angle(xyz[i + 2, ], xyz[i, ], xyz[i + 1, ])) * 180 / pi
  expect_gt(min(smallest), 10)
  expect_lt(max(abs(sqrt(rowSums(xyz^2)) - sqrt(0.5))), 0.02)
})

test_that("isosurface layers accept a method and reject unknown ones", {
  d <- expand.grid(x = seq(-1, 1, length.out = 7), y = seq(-1, 1, length.out = 7), z = seq(-1, 1, length.out = 7))
  d$value <- with(d, x^2 + y^2 + z^2)
  for (method in c("cubes", "tetra")) {
    s <- stats_scene(ggplot2::ggplot(d, ggplot2::aes(x, y, z = z, value = value)) +
      geom_isosurface_3d(levels = 0.5, method = method) + coord_3d())
    expect_gt(length(s$packet$faces$primitive_id), 0L)
    expect_silent(validate_scene_packet(s$packet))
  }
  expect_error(geom_isosurface_3d(levels = 0.5, method = "voxels"), "should be one of")
  b <- ggplot2::ggplot_build(ggplot2::ggplot() + geom_isofunction_3d(fun = function(x, y, z) x + y + z,
    n = 5, levels = 0.2, method = "tetra") + coord_3d())$data[[1L]]
  expect_equal(b$x + b$y + b$z, rep(0.2, nrow(b)), tolerance = 1e-12)
})
