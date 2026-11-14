# Implicit surfaces fitted to point clouds

Fits a function s(x) whose zero set passes through the points and draws
that level surface. Surface triangles face increasing s, outwards.

## Usage

``` r
stat_implicit_fit_3d(
  mapping = NULL,
  data = NULL,
  geom = GeomStatsSurface3D,
  position = "identity",
  ...,
  method = "rbf",
  n = 32L,
  k = 10L,
  smooth = 0,
  depth = 8L,
  screening = 4,
  samples_per_node = 1.5,
  extractor = NULL,
  refine = 2L,
  na.rm = FALSE,
  show.legend = NA,
  inherit.aes = TRUE
)
```

## Arguments

- mapping, data, position, ..., na.rm, show.legend, inherit.aes:

  Standard layer arguments.

- geom:

  Geom prototype for keyed triangle corners.

- method:

  Fitting method, `"rbf"` or `"poisson"`.

- n:

  Grid resolution for the `"rbf"` extraction, scalar or three integers.

- k:

  Number of neighbours for normal estimation and, for `"poisson"`, for
  the area per point.

- smooth:

  Value added to the diagonal of the `"rbf"` interpolation matrix;
  positive values trade interpolation for smoothness.

- depth:

  Maximum octree depth of the `"poisson"` fit, from 1 to 10; the finest
  cells are at least 1.1 / 2^depth of the largest extent of the points.

- screening:

  Weight of the point values against the normal field in the `"poisson"`
  fit (PoissonRecon's point weight); 0 gives unscreened Poisson
  reconstruction.

- samples_per_node:

  Number of points an octree cell should meet at the depth where a point
  is splatted in the `"poisson"` fit.

- extractor:

  Isosurface extraction method, as `method` in
  [`stat_isosurface_3d()`](https://gillescolling.com/ggplot3d/reference/stat_isosurface_3d.md);
  `NULL` uses `"cubes"` for `"rbf"` and `"nets"` for `"poisson"`, which
  takes `"nets"` or `"dual"`.

- refine:

  Number of regula falsi steps per edge vertex.

## Details

`method = "rbf"` is the polyharmonic radial basis function fit of Carr
et al. (2001): s(x) = sum_i w_i \|x - c_i\| + p(x) with a linear
polynomial p, the biharmonic spline in three dimensions, interpolating s
= 0 at every point and s = +e and -e at two off-surface points moved by
e along the point's normal outwards and inwards. e starts at 1 % of the
bounding box diagonal and is halved for a point until that point is the
data point nearest to both of its off-surface points. The (3n +
4)-square system is solved densely, so at most 2000 points are taken. s
is sampled on an `n`-point grid over the bounding box of the points,
enlarged on each side by 10 % of its extent plus 5 % of its diagonal,
and its zero set is extracted as in stat_isofunction_3d with `extractor`
as the extraction method, `refine` regula falsi steps per edge vertex
and vertex normals from s.

`method = "poisson"` is screened Poisson surface reconstruction (Kazhdan
and Hoppe 2013), for large point sets. Each point's surface area is
estimated from the disc reaching its `k`-th nearest neighbour, and its
normal is splatted, as a kernel of that area, at the octree depth where
a cell meets about `samples_per_node` points (interpolated between the
two nearest depths). The octree, of maximum depth `depth`, spans a cube
1.1 times the largest extent of the points, enlarged where needed so
that it reaches two splat cells beyond every point. The octree is
complete to depth 5 and conforming, so every node sees the functions of
the coarser nodes around it. The indicator function chi is a combination
of quadratic B-splines, one per octree node at every depth, reflected at
the cube faces (Neumann boundary), and minimises the squared distance of
its gradient to the splatted normal field plus `screening` times the
mean squared value of chi at the points, scaled by the surface area; the
screening of depth d is weighted by 2^d. The system is solved by one
cascadic pass from the root down, each depth by conjugate gradients with
the coarser solution fixed (Kazhdan and Hoppe 2013, algorithm 2). The
surface is the level of chi at its mean over the points, extracted on
the leaves of the octree by surface nets (`extractor = "nets"`, the
default for this method, which placed vertices closer to smooth test
surfaces) or dual contouring (`"dual"`), with `refine` regula falsi
steps per crossing and normals from the gradient of chi. `screening = 0`
gives the unscreened reconstruction of Kazhdan, Bolitho and Hoppe
(2006). The parameters follow PoissonRecon: `depth`, `screening`
(`--pointWeight`) and `samples_per_node` (`--samplesPerNode`); larger
`samples_per_node` smooths noisy points.

Normals come from the `nx`, `ny` and `nz` aesthetics when all three are
mapped. Otherwise each point's normal is the least principal axis of its
`k` nearest neighbours, and normals are oriented consistently by
propagation along a minimum spanning tree of the neighbour graph
weighted by 1 - \|n_i . n_j\|, starting at the highest point with an
upward normal (Hoppe et al. 1992); a closed surface then gets outward
normals.

## References

Carr, Beatson, Cherrie, Mitchell, Fright, McCallum and Evans (2001).
Reconstruction and representation of 3D objects with radial basis
functions. SIGGRAPH 2001, 67-76.
[doi:10.1145/383259.383266](https://doi.org/10.1145/383259.383266)

Hoppe, DeRose, Duchamp, McDonald and Stuetzle (1992). Surface
reconstruction from unorganized points. SIGGRAPH 1992, 71-78.
[doi:10.1145/133994.134011](https://doi.org/10.1145/133994.134011)

Kazhdan, Bolitho and Hoppe (2006). Poisson surface reconstruction.
Symposium on Geometry Processing 2006, 61-70.

Kazhdan and Hoppe (2013). Screened Poisson surface reconstruction. ACM
Transactions on Graphics 32, 29.
[doi:10.1145/2487228.2487237](https://doi.org/10.1145/2487228.2487237)

## Examples

``` r
set.seed(2)
p <- matrix(rnorm(450), ncol = 3)
p <- p / sqrt(rowSums(p^2))
d <- data.frame(x = p[, 1], y = p[, 2], z = 0.6 * p[, 3])
ggplot2::ggplot(d, ggplot2::aes(x, y, z = z)) +
  stat_implicit_fit_3d(n = 20, fill = "steelblue") + coord_3d()

# Screened Poisson reconstruction of a noisy torus from 5000 points
u <- runif(5000, 0, 2 * pi); v <- runif(5000, 0, 2 * pi)
torus <- data.frame(x = (1 + 0.4 * cos(v)) * cos(u), y = (1 + 0.4 * cos(v)) * sin(u),
                    z = 0.4 * sin(v) + rnorm(5000, sd = 0.005))
ggplot2::ggplot(torus, ggplot2::aes(x, y, z = z)) +
  stat_implicit_fit_3d(method = "poisson", depth = 6, fill = "steelblue") + coord_3d()
```
