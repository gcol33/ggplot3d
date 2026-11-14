# Intrinsic geometry on meshes

Distances, shortest paths and polar charts measured along a triangulated
surface rather than through space. The verbs take a
[`mesh_3d()`](https://gillescolling.com/ggplot3d/reference/mesh_3d.md)
corner table or a compatible data frame, as the [mesh
processing](https://gillescolling.com/ggplot3d/reference/mesh_processing.md)
verbs do, and points in the units of its positions.

## Usage

``` r
mesh_geodesic_distance(
  m,
  source,
  time = 1,
  boundary = c("average", "neumann", "dirichlet")
)

mesh_logmap(m, source, direction = NULL, time = 1)

mesh_geodesic_path(m, from, to, max_iterations = 100000L)
```

## Arguments

- m:

  A mesh: a
  [`mesh_3d()`](https://gillescolling.com/ggplot3d/reference/mesh_3d.md)
  corner table or a compatible data frame.

- source:

  Source points, `c(x, y, z)` or a matrix or data frame of points;
  `mesh_logmap()` takes one point.

- time:

  Heat time as a multiple of the squared mean edge length. Larger values
  give a smoother distance.

- boundary:

  Heat at boundary vertices: `"average"`, `"neumann"` or `"dirichlet"`.

- direction:

  Tangent direction of the logarithmic map's angle zero, `c(x, y, z)`,
  or NULL for the first edge at the source.

- from, to:

  Path ends, as points; rows pair up.

- max_iterations:

  Largest number of FlipOut steps per path.

## Value

`mesh_geodesic_distance()` and `mesh_logmap()` return the mesh of the
class of `m` with the added columns. `mesh_geodesic_path()` returns a
data frame of path points with columns `x`, `y`, `z`, `path` (the row of
`from` and `to`) and `s`, the arc length from the start; the attribute
`length` holds the length of each path.

## Details

`mesh_geodesic_distance()` adds the column `distance`, the geodesic
distance from the nearest source, by the heat method of Crane,
Weischedel and Wardetzky (2013). Heat spreads from the sources for a
short time `t = time * h^2`, `h` the mean edge length, by one backward
Euler step of the heat equation with the cotangent Laplacian and mixed
Voronoi areas; the direction of the heat gradient, normalised on every
face, is then integrated back into a distance by a Poisson equation.
Both linear systems are factored by a native sparse Cholesky
decomposition with a nested dissection ordering. A source is a point;
its unit of heat starts on the corners of the closest face, by
barycentric weight, and the distance is shifted to vanish there. With
several sources the distance is to the nearest. On a surface with
boundary, `boundary` chooses how heat behaves there: absorbed
(`"dirichlet"`), reflected (`"neumann"`), or the mean of both
(`"average"`), which the paper recommends because it mimics a surface
without boundary. Parts of the mesh that hold no source get `NA`.

`mesh_logmap()` adds `logmap_u` and `logmap_v`, the logarithmic map of
the surface at the vertex closest to `source`: polar coordinates around
the source whose radius is the geodesic distance and whose angle is the
direction in which the shortest geodesic leaves the source, measured
from `direction` (projected into the tangent plane; by default the first
edge at the source). It is the vector heat method of Sharp, Soliman and
Crane (2019): the source's tangent vectors are carried over the surface
by heat flow of the connection Laplacian, a unit vector to fix the angle
zero and outward unit vectors on a small circle to give the radial
direction, and the radius comes from a Poisson equation on the radial
field.

`mesh_geodesic_path()` returns the shortest path along the surface
between the vertices closest to each row of `from` and `to`, as a
polyline that crosses the faces. A Dijkstra path along the mesh edges is
straightened by FlipOut (Sharp and Crane 2020): edges of an intrinsic
triangulation, which keeps the surface fixed and changes only which
straight segments on it are edges, are flipped around every bend of the
path that is sharper than a straight line, until no bend can be
shortened. The result is a locally shortest geodesic of the polyhedral
surface, exact up to rounding; it is the shortest path whenever the
Dijkstra path winds around the surface's holes the same way the shortest
path does. The intrinsic edges are traced back onto the input faces
through the polar angles of the edge directions at each vertex
(signposts, Sharp, Soliman and Crane 2019b).

## References

Crane, Weischedel and Wardetzky (2013). Geodesics in heat: a new
approach to computing distance based on heat flow. ACM Transactions on
Graphics 32(5), 152.
[doi:10.1145/2516971.2516977](https://doi.org/10.1145/2516971.2516977)

Sharp, Soliman and Crane (2019). The vector heat method. ACM
Transactions on Graphics 38(3), 24.
[doi:10.1145/3243651](https://doi.org/10.1145/3243651)

Sharp, Soliman and Crane (2019b). Navigating intrinsic triangulations.
ACM Transactions on Graphics 38(4), 55.
[doi:10.1145/3306346.3322979](https://doi.org/10.1145/3306346.3322979)

Sharp and Crane (2020). You can find geodesic paths in triangle meshes
by just flipping edges. ACM Transactions on Graphics 39(6), 249.
[doi:10.1145/3414685.3417839](https://doi.org/10.1145/3414685.3417839)

Davis (2006). Direct Methods for Sparse Linear Systems. SIAM.
[doi:10.1137/1.9780898718881](https://doi.org/10.1137/1.9780898718881)

## Examples

``` r
sphere <- function(u, v) list(x = cos(u) * sin(v), y = sin(u) * sin(v), z = cos(v))
m <- ggplot2::layer_data(ggplot2::ggplot() +
  stat_parametric_3d(fun = sphere, v = c(0, pi), n = c(32, 17), closed = c(TRUE, FALSE)) +
  coord_3d())
m <- m[c("x", "y", "z", ".face", ".corner", ".vertex")]
d <- mesh_geodesic_distance(m, source = c(0, 0, 1))
range(d$distance)
l <- mesh_logmap(m, source = c(0, 0, 1), direction = c(1, 0, 0))
head(l[c("logmap_u", "logmap_v")])
path <- mesh_geodesic_path(m, from = c(1, 0, 0), to = c(0, 1, 0))
attr(path, "length")
```
