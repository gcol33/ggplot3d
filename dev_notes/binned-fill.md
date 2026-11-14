# Binned face validation

Faces split in `packet_from_primitives_3d()` using per-vertex scalar samples
retained before scale mapping. Scale breaks are converted back to the scalar's
original units. A clipped triangle becomes convex band polygons; positions,
normals and UVs share barycentric interpolation weights. Original unit vertex
normals are interpolated without renormalising the new corners. Flat styles
retain the parent normal. Attribute interpolation and palette mapping are
batched across the emitted pieces. Surface edge colours use the same cuts.

Binned scales train on vertex samples under both shading modes, so implicit
limits retain scalar extrema. Continuous ramps keep their face-average training
and bypass splitting. Custom stepped palettes share jump detection between
training and mapping, including scales::new_continuous_palette() wrappers.

Clipping follows linear edge interpolation as used by VTK's
vtkBandedPolyDataContourFilter:
https://vtk.org/doc/nightly/html/classvtkBandedPolyDataContourFilter.html
Break and palette semantics follow ggplot2's ScaleBinned:
https://ggplot2.tidyverse.org/reference/binned_scale.html

The tone-1 measurements use the lighting vignette's 36 by 36 hill, its exact
minimal theme with base size 17 and transparent backgrounds, and a 6.5 by 4.5
inch svglite device with point size 14. Three paired device renders include
build, exact visibility and paint. The original path is reproduced by bypassing
band detection; its packet is identical to the packet saved from the original
checkout before implementation.

| Measure | Original | Split bands |
| --- | ---: | ---: |
| Emitted faces | 2450 | 2850 |
| SVG bytes | 115340 | 101828 |
| Median elapsed seconds | 2.55 | 2.55 |

Paired elapsed seconds were 3.19/3.11, 2.33/2.45 and 2.55/2.55. These measurements
support keeping clipping in R with batched attribute interpolation. The final
PNG was rendered at the vignette's 120 dpi and inspected. Local images and
baseline packets remain in `dev_notes/bands/`.

The analytic regression compares a top-down vector render with the same
Gaussian hill field. Interior samples farther than 0.01 scalar units from a band
boundary must have less than 0.5% incorrect colours. The original path measured
13.5%. Geometry tests independently require every sub-face to remain between
successive breaks. Tests cover flat and smooth surfaces, transformed mesh fills
and colours, custom stepped palettes, censored limits, area, interpolated normals
and UVs, viewer face/corner arrays, and lit and unlit vector/raster agreement.
Orthographic and perspective visibility are checked against the independent
referee. Logarithmic scales interpolate the source scalar: a break at 10 on an
edge from 1 to 100 lies at 9/99 of that edge.

The saved original continuous-scale smooth hill packet is identical to the new
packet. Persistent tests also require byte-identical packets and compiled scenes
for ordinary and low-contrast continuous gradients with band detection bypassed.

Banded edge colours preserve the parent face fill's alpha while interpolating its RGB channels.
