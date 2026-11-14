# Plotter and retained outlines

I reproduced the lighting vignette's hatch chunk with its 36 by 36 surface,
spacing 0.02, outline linewidth 0.4, svglite at 6.5 by 4.5 inches and
pointsize 14. I used the visibility vignette's halo cube with azimuth -50,
elevation 25, outline linewidth 0.8 and halo 1.2 mm at the same device size.
`plotter-repro.R before` and `plotter-repro.R final` write the SVG and ragg
PNG files under `dev_notes/plotter-before` and `dev_notes/plotter-final`.

| SVG | Before, bytes | After, bytes |
| --- | ---: | ---: |
| Lighting hatch, plotter | 1,036,059 | 22,768 |
| Halo cube, vector | 102,317 | 30,165 |
| Halo cube, hybrid | 24,672 | 22,264 |

The hatch has 69 polylines and zero filled scene paths. The vector cube has
10 compound paths, including its face fills, compared with 85 paths before.
The small triangles and long sampled arcs originate in filled visible stroke
outlines, including dashed round caps and pieces of halo footprints. Opaque
outlines of the same paint now share compound paths, with device-scale
simplification and consecutive-point removal.

The cube packet contains 12 unique polylines. The raster renderer classified
each pixel across a retained stroke against the opaque depth at that pixel.
A sloping adjacent face could put the inward half of a silhouette stroke
behind its own surface. This produced dashed paint on visible edges and
reduced the apparent width. Retained stroke visibility now queries opaque
faces at the nearest centreline point through a bounds index, then places the
stroke over the surface at the painted sample. This uses the same decisive
point as the vector-versus-image occlusion test. The slope-scaled buffer
offset from 794ea1b belongs to that separate occlusion test.

Plotter compilation requests explicit hidden-stroke classification, including
strokes whose ordinary vector rendering relies on later face fills to cover
them. It removes the hidden intervals required by each source's hidden
policy. Grid and plotter share the native centreline interval clipper and
the R stroke-run preparation. Plotter always selects these intervals,
unions footprint pieces and joins continued intervals before removing
points closer than the smaller of 0.01 mm and one tenth of a device pixel.
It sorts paths by nearest endpoint within each layer and colour and permits
reversal, following [vpype linesort](https://vpype.readthedocs.io/en/stable/reference.html#linesort)
and [AxiDraw reordering](https://www.axidraw.com/doc/cli_api/#reordering).
Text closes the scene.

The lighting pixel test compares dark interior stroke pixels from ragg at
300 dpi, with a one-pixel neighbourhood and fewer than 0.1% unmatched canvas
pixels in each direction. The centreline representation can differ from a
partly visible stroke ribbon at occlusion boundaries. Cube tests sample all
12 edge interiors, require nine solid edges in both renderers, and compare
isolated stroke coverage within 2%. SVG tests inspect parsed XML for filled
paths, masks, repeated consecutive coordinates and size limits.

I inspected the hatch, vector cube and hybrid cube PNGs before integration.
