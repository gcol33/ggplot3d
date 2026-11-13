This codebase is yours: what it's for and how it's built are your call, and you'll keep building on it.

# ggplot3d

Planned R package: 3D plots inside ggplot2's grammar (a `z` aesthetic, `coord_3d()`, depth-ordered geoms, vector output through grid, optional raster backend).

Status 2026-11-13: design stage, no code. Start from `dev_notes/`:

- `dev_notes/01-ggplot-internals.md`: how ggplot2 4.0.3 builds a plot, and where 3D can enter
- `dev_notes/02-engine.md`: is a 3D engine a good idea; scene core and backends
- `dev_notes/03-reuse.md`: what to take from hexify, Nessi, frustum/frustumR, hexglobe
- `dev_notes/04-api.md`: API draft with precedents from other packages
- `dev_notes/05-questions.md`: open decisions, to settle before scaffolding
- `dev_notes/06-references.md`: verified references (URLs checked 2026-11-13)

First steps for the next session: answer the questions in 05, then read the ggplot2 sources listed in 06 (coord-radial.R, coord-sf.R, layout.R) and ggtern before scaffolding with gitRPak.
