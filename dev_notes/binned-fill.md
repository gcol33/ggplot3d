# Binned face validation

The 36 by 36 lighting hill uses 2,450 source triangles. Faces now split in
`packet_from_primitives_3d()` using transformed scalar samples retained before
scale mapping. A clipped triangle becomes convex band polygons; numeric corner
attributes use the same barycentric interpolation. Original unit vertex normals
are interpolated without renormalising the new corners. Flat styles retain the
parent face normal. Continuous palettes bypass the splitter.

The clipping follows linear edge interpolation as used by VTK's
vtkBandedPolyDataContourFilter:
https://vtk.org/doc/nightly/html/classvtkBandedPolyDataContourFilter.html
Binned breaks and palette evaluation follow ggplot2's ScaleBinned:
https://ggplot2.tidyverse.org/reference/binned_scale.html

Measurements on the lighting vignette's tone plot, 6.5 by 4.5 inch svglite output,
from this Windows checkout. Each timing is one complete device render; elapsed
times include build, visibility and paint and are subject to run-to-run variation.

| Measure | Original | Split bands |
| --- | ---: | ---: |
| Emitted faces | 2450 | 2850 |
| SVG bytes | 126220 | 110956 |
| Render elapsed seconds | 7.25 | 6.42 |

The split plot's packet build took 2.99 seconds. The measured total render cost
supports keeping this geometry operation in R. The rendered PNG was inspected.
Generated images and baseline packets remain local in `dev_notes/bands/`.

The analytic regression compares a top-down vector render against the same
Gaussian hill field. Interior samples farther than 0.01 scalar units from a band
boundary must have less than 0.5% incorrect colours. The original path measured
13.5%. Geometry tests separately require every sub-face to stay entirely between
successive scale breaks, including flat and smooth surfaces, log-transformed
mesh fills and colours, custom stepped palettes and censored scale limits.
Additional checks cover area, interpolated normals and UVs, viewer face/corner
arrays, lit and unlit vector/raster agreement, and exact orthographic and
perspective visibility against the independent referee.

The saved original continuous-scale smooth hill packet is identical to the new
packet. Persistent tests also require byte-identical packets and compiled scenes
for ordinary and low-contrast continuous gradients with band detection bypassed.
