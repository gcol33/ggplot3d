# Spherical geometry in stereographic and gnomonic charts

Maps the unit three-sphere in four dimensions to three display
coordinates. Paths and faces follow shorter great circles through the
shared sampler. Ambient inputs map x, y, z and w. Stereographic input is
the inverse chart (2p, \|p\|^2-1)/(1+\|p\|^2). The display map is
`X[1:3]/(1-X[4])`. Gnomonic coordinates are the homogeneous point X
itself, (X1:3, X4), read in oriented projective space (Stolfi 1991): X
and -X are distinct points, so the chart covers the whole sphere, and
the display `X[1:3]/X[4]` shows the hemisphere X4 \> 0. Great circles
are straight lines and totally geodesic faces are planar, so SO(4) acts
linearly. Stereographic coordinates are homogeneous, (X1:3, 1 - X4), so
the pole is the point at infinity and box clipping happens before
division. Paths are clipped analytically on their great circles against
the box planes, which are planes of R^4. Faces are sampled only outside
a cap around the pole whose image lies outside twice the box
circumradius, and the scene engine clips their triangles to the box in
homogeneous coordinates, so fibres and faces through the pole are drawn
up to the panel box.

## Usage

``` r
coord_spherical_geometry_3d(
  dim = 3,
  model = c("stereographic", "gnomonic"),
  input = model,
  extent = 3,
  inside = FALSE,
  observer = NULL,
  fog = NULL,
  ...
)
```

## Arguments

- dim:

  Spherical dimension, currently three.

- model, input:

  Display model and input chart.

- extent:

  Positive half-width of the finite display box. Gnomonic geometry is
  clipped to the finite box of the hemisphere in front of the observer.

- inside:

  Use a first-person perspective eye at the gnomonic origin.

- observer:

  Observer point in the input chart, an ambient unit four-vector, or an
  SO(4) matrix taking the observer to the origin.

- fog:

  Optional positive inverse-area brightness scale. At each primitive
  centre brightness is min(1, fog/sin(d)^2), in linear light.

- ...:

  Arguments to
  [`coord_3d()`](https://gillescolling.com/ggplot3d/reference/coord_3d.md),
  including SO(4) isometry matrices from
  [`rotate_4d()`](https://gillescolling.com/ggplot3d/reference/rotate_4d.md).
  These also work in animate_3d(isometry=).

## Value

A Coord3D object.

## Details

In the gnomonic model an observer or isometry is an SO(4) matrix acting
on the homogeneous coordinates (X1:3, X4), carried by the camera
pre-transform as in Weeks (2002). The geometry of the whole sphere is
emitted once, with packet vertices of either sign of w; the scene engine
applies the action and clips in homogeneous coordinates to w \>= 0 and
the panel box (Blinn and Newell 1978), so the hemisphere within a
quarter turn of the observer is drawn and a rotation brings the far
hemisphere into view. animate_3d(isometry = ) frames, such as Clifford
translations from
[`rotate_4d()`](https://gillescolling.com/ggplot3d/reference/rotate_4d.md)
with left or right quaternions, only recompile. Inside views restrict
geometry to the observer hemisphere and finite panel box. Near defaults
to sqrt(machine epsilon), and far to the farthest panel-box corner, in
normalised camera units. Azimuth, elevation and roll set facing. The
stereographic model applies the action before packing.

## References

Johnson (2011). A visualization of the Hopf fibration.
<https://nilesjohnson.net/hopf.html>. Obermeyer. Jenn3d.
<https://jenn3d.org/>. Weeks (2002). Real-time rendering in curved
spaces.
[doi:10.1109/MCG.2002.1046633](https://doi.org/10.1109/MCG.2002.1046633)
. Coulon et al. (2022), Section 7. <https://arxiv.org/abs/2010.15801>.
Stolfi (1991). Oriented projective geometry. Academic Press. Hart,
Francis and Kauffman (1994). Visualizing quaternion rotation.
[doi:10.1145/195784.197480](https://doi.org/10.1145/195784.197480) .

## Examples

``` r
d <- data.frame(x=c(-1,1),y=.4,z=0)
ggplot2::ggplot(d,ggplot2::aes(x,y,z=z))+geom_path_3d()+
  coord_spherical_geometry_3d(isometry=rotate_4d("xw",.2))
d <- data.frame(x=.5,y=c(-.2,.2,.2,-.2),z=c(-.2,-.2,.2,.2))
ggplot2::ggplot(d,ggplot2::aes(x,y,z=z))+geom_polygon_3d(fill="steelblue")+
  coord_spherical_geometry_3d(inside=TRUE,azimuth=180,elevation=0,fog=.1)
```
