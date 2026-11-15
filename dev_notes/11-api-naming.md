# API naming rules

One name per concept, one value set per name. Checked against the exported
signatures (`getNamespaceExports()` + `formals()`).

## Function names

- Grammar functions end in `_3d` (or `_4d`): `geom_`, `stat_`, `coord_`,
  `scale_z_`, `guide_`, `position_`, `facet_`, `annotation_`, and the setting
  constructors passed to them (`camera_3d`, `material_3d`, `light_3d`,
  `backend_3d`, ...), and the motion sources for `animate_3d()`.
- Families named after one of the package's own object types drop the suffix
  and use the type as prefix: `mesh_*()` operate on `mesh_3d()` objects,
  `sdf_*()` build signed distance fields, `light_*()` build light sources for
  `light_3d()`.
- ggplot2 mirrors keep ggplot2's name: `zlim()`, re-exported `fortify()`.
- A concept owned by another package is a method of its generic, never a
  second name (`ggplot2::get_alt_text()`).
- A function returning a list of layers is not a `stat_`/`geom_`; a function
  without data is an `annotation_`.
- Every `geom_x_3d()` paired with a `stat_x_3d()` has the full signature
  (generated from the stat's formals), never `function(...)`.

## Argument names

- Spelling is British: `colour`, `centre`, `normalise`, `fibre`.
- snake_case, except arguments copied from current ggplot2 or ggrepel, which
  keep their spelling (`na.rm`, `show.legend`, `fun.args`, `method.args`,
  `label.padding`, `max.iter`, `inner.radius`, `size.unit` and its
  `radius.unit`/`width.unit` siblings, `middle.colour`, ...).
- `tolerance`, never `tol`. `dimension`, never `dim`/`d`.
- `backend`: vector/raster/... choice, values as `backend_3d(type =)`.
- `onto`: where something is projected or dropped; values are pane names
  (`"floor"`, `"back"`, `"xmin"` ... `"zmax"`), plus `list(axis =, at =)`
  planes where supported. `panes` (coord) names the panes that are drawn.
- `plane` + `at`: an orientation (`"xy"`, `"xz"`, `"yz"`, or a pane name) and
  its position.
- `space`: the coordinate frame a value is given or computed in (`"data"`,
  `"camera"`, `"model"`, `"screen"`, `"ilr"`). `geometry`: the geometry of
  space itself (`coord_3d()`, `backend_3d()`, `geodesic_3d()`).
- `levels`: iso values / contour levels; `probs`: probability content
  levels; `level`: a confidence level.
- `*_colour` for colour-valued arguments of an object that also has a
  same-named concept (`shadow_colour` in `light_3d()` vs `coord_3d(shadow =)`).
