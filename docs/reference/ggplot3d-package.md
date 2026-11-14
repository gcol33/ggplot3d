# 3D Plots in the Grammar of Graphics

Three-dimensional layers, z position scales and camera controls for
ggplot2. Panel scenes resolve visibility across layers for vector and
raster output.

## Options

- `ggplot3d.threads`:

  Number of threads the scene compiler uses to find which pieces of a
  panel occlude each other. Defaults to every logical core, or 2 when
  `_R_CHECK_LIMIT_CORES_` is set. The compiled scene is the same for
  every thread count.

- `ggplot3d.cache_bytes`:

  Memory, in bytes, kept for recently compiled scenes, so that redrawing
  a plot at the same size and view skips the compiler. Defaults to 256
  MiB; 0 turns the cache off.

- `ggplot3d.max.overlaps`:

  Default `max.overlaps` of
  [`geom_text_repel_3d()`](https://gillescolling.com/ggplot3d/reference/geom_label_3d.md)
  and
  [`geom_label_repel_3d()`](https://gillescolling.com/ggplot3d/reference/geom_label_3d.md):
  a label that overlaps more labels and points than this where it starts
  is not drawn. Defaults to 10.

## See also

Useful links:

- <https://github.com/gcol33/ggplot3d>

- Report bugs at <https://github.com/gcol33/ggplot3d/issues>

## Author

**Maintainer**: Gilles Colling <gilles.colling051@gmail.com>
([ORCID](https://orcid.org/0000-0003-3070-6066)) \[copyright holder\]

Authors:

- Gilles Colling <gilles.colling051@gmail.com>
  ([ORCID](https://orcid.org/0000-0003-3070-6066)) \[copyright holder\]

Other contributors:

- The Manifold Authors (Bundled Manifold library) \[copyright holder\]

- Angus Johnson (Bundled Clipper2 library) \[copyright holder\]

- Jared Hoberock (Manifold utilities) \[copyright holder\]

- Nathan Bell (Manifold utilities) \[copyright holder\]

- Wenzel Jakob (Disjoint-set implementation) \[copyright holder\]

- wi-re (Singular value decomposition) \[copyright holder\]

- Sun Microsystems, Inc. (Mathematical functions) \[copyright holder\]
