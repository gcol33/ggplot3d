# Physical solids for 3D printing

Maps the panel box into a millimetre bounding box with a uniform scale,
keeping its aspect, then applies physical feature dimensions. Strokes
use the rotation-minimising tube sweep; markers use their solid
templates. Tube radii include the polygonal cross-section's inradius
correction. Marker solids contain an inscribed ball of diameter min_mm.
Open faces become normal-offset shells or solids closed down to the
base. Layer-specific surface methods are keyed by the names from
export_3d().

## Usage

``` r
print_3d(
  size_mm = c(100, 100, 60),
  min_mm = 0.8,
  shell_mm = 1.2,
  base_mm = 2,
  text = c("raised", "engraved", "none"),
  furniture = c("base", "axes"),
  surface = c("shell", "base"),
  layers = NULL,
  sides = 12L
)
```

## Arguments

- size_mm:

  Three positive maximum dimensions of the panel box.

- min_mm:

  Minimum tube or marker width and text extrusion depth.

- shell_mm:

  Normal-offset thickness, clamped to min_mm.

- base_mm:

  Base thickness, clamped to min_mm when positive.

- text:

  `"raised"`, `"engraved"` or `"none"`.

- furniture:

  Components to include: `"base"` and/or `"axes"`.

- surface:

  Default open-surface method: `"shell"` or `"base"`.

- layers:

  Named vector of surface methods, keyed by layer name such as
  `c(layer1_surface = "base", layer2_mesh = "shell")`.

- sides:

  Number of tube cross-section sides, at least six.

## Value

A printing specification for `export_3d(..., print = )`.

## Details

Raised and engraved character labels use font outlines on backing
plaques. Outlines expand by min_mm / 2 and the extrusion depth is
min_mm. A plaque is min_mm thick for raised text and 2 \* min_mm for
engraved text. Axis labels use the existing in-plane guide layout. Text
and axis furniture can extend beyond the panel box.

Meshes are checked for edge incidence, winding and self-intersections.
Manifold performs Boolean union, preserves cavity boundaries and carries
colour properties through the bundled native Manifold library, using
double-precision meshes and a single-threaded Boolean engine.
Disconnected solid components are stored in one model object.

## Examples

``` r
settings <- print_3d(size_mm = c(100, 100, 60), text = "none")
settings$min_mm
```
