# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

This codebase is yours: what it's for and how it's built are your call, and you'll keep building on it.

# ggplot3d

R package: 3D plots inside ggplot2's grammar. A `z` position scale, `coord_3d()`, `_3d` geoms/stats, one shared scene per panel with exact visibility, drawn as grid vector output or through a raster backend. Rcpp/C++17 engine. pkgdown site at https://gillescolling.com/ggplot3d/ (built into `docs/`, tracked for GitHub Pages).

## Commands

Windows R lives at `C:/Program Files/R/R-4.6.1/bin/Rscript.exe`. Run from the repo root.

```r
devtools::load_all()                         # compiles src/ when needed
devtools::test()                             # whole suite
devtools::test(filter = "engine-bsp")        # one file: tests/testthat/test-engine-bsp.R
testthat::test_file("tests/testthat/test-grid-backend.R")
Rcpp::compileAttributes(); devtools::document()   # after changing // [[Rcpp::export]] or roxygen
rcmdcheck::rcmdcheck(args = c("--as-cran", "--no-manual"))   # what CI runs
source("build_site.R")                       # pkgdown site via ~/.R/build_pkgdown.R
```

- Native code is mostly header-only (`src/scene/*.h`, `src/raster/*.h`, `src/geometry/*.h`) included by `engine.cpp`, `raster.cpp`, `geometry.cpp`. Editing a header alone does not trigger a rebuild: delete `src/*.o` and `src/*.dll`/`*.so` first.
- `R/RcppExports.R` and `src/RcppExports.cpp` are generated; never hand-edit.
- Benchmarks and the visibility referee: `bench/README.md` (`bench/run-referee.R`, `bench/run-benchmarks.ps1`). Comparisons are against ggcube.

## Architecture

Pipeline: ggplot2 build → per-layer emitters produce **scene packets** → packets merged per panel → C++ engine compiles a draw order with exact visibility → a backend draws it. Draw-time compilation: the scene is compiled in `makeContent()`, when the device size is known.

**Entry into ggplot2** (`R/build-adapter.R`, `R/layout-3d.R`, `R/scale-z.R`):
- Adding `coord_3d()` (S7 `update_ggplot` method) tags the plot with class `ggplot3d`. `ggplot_build.ggplot3d` then wraps the plot: every layer goes through `adapt_layer_3d()`, scales through `scales_3d()` (adds/infers the z scale, keeps z out of non-position scales), layout through `Layout3D`.
- Adapted layers add internal `.source` (input row) and `.layer` columns; these are reserved names. Contour stats get `z <- level`.
- `Coord3D` (`R/coord-3d.R`) suppresses ggplot2's own axes/bg/fg; `draw_panel` calls `collect_scene_layers()` then `scene_panel_grob()`. `z_scales = "free"` adds a `SCALE_Z` layout column; facets never own z.

**Emitter registry** (`R/emitters.R`): `register_geom_3d(geom, emit, ...)` maps a Geom class to an emitter that returns a packet. ggplot2's own GeomPoint/Path/Line/Segment/Text/Polygon/Blank and contour geoms are registered, so plain ggplot2 layers work in `coord_3d()`. New geoms are added by registering an emitter in their `R/layers-*.R` file, never by special-casing the build. Positions other than identity are only allowed for a fixed set of geoms or via the entry's `position_check`.

**Scene contract** (`R/scene-contract.R` ↔ `src/scene/contract.h`, spec in `dev_notes/08-scene-contract.md`): `new_scene_packet()` validates typed columnar tables (vertices, corners, faces, polylines, markers, text, footprints, styles), 1-based indices, positions normalised to the panel box. `merge_scene_packets()` remaps IDs across layers. The R schema, C++ reader and the spec must change together; schema is versioned (`schema_version`).

**Engine** (`src/engine.cpp` → `scene_compile_cpp`): projection, clipping, BSP partition/ordering, coverage, stroke and footprint handling under `src/scene/`; memory capped by a `ResourceBudget` (viewport attribute `engine_budget_bytes`). Returns draw order, masks and a `capabilities` list (`exact_visibility`, `footprint_clipping`) that the referee tests gate on.

**Backends**: `R/backend-grid.R` (vector; seam-free surfaces via compound-path underlays, see comment block there), `R/backend-raster.R` + `src/raster/` (supersampled raster), `R/backend-light.R` (Lambert shading from `light_3d()`). Chosen with `coord_3d(backend = backend_3d(...))`.

**Panel furniture** (`R/panel-furniture.R`, `R/guides-3d.R`, `R/theme-3d.R`): panes, grid lines and back box edges are emitted into the scene packet so they depth-sort with data; axes and front frame go to the overlay.

**Other**: `R/view-3d.R` camera (`new_camera_3d`), `R/stats-*.R` + `src/geometry/` (KDE, hull, isosurface), `R/animate-3d.R` (`animate_3d()` frames via ragg; gifski/av), `inst/htmlwidgets/` interactive viewer.

## Tests

Test files are prefixed by area: `test-engine-*` (C++ via `scene_compile_cpp`), `test-integration-*` (build adapter, scales), `test-grid-*` / `test-raster-*` (backends), `test-referee-*` (oracle comparison against fixtures in `tests/fixtures/oracle` and `tests/fixtures/scenes`), `test-layers-*`, `test-stats-*`, `test-motion-*`. `helper-stats-scene.R::stats_scene()` builds a plot and returns its merged packet, the usual way to assert on emitted geometry.

## Design notes

`dev_notes/` (Rbuildignored): 01–06 original design research, `07-api-v2.md` the full API spec (large; grep it), `08-scene-contract.md` the frozen packet schema, `09-work-plan.md` the phase plan with the file-ownership split used for parallel agents, `10-roadmap-references.md` the references behind the roadmap issues (GitHub tracking issue #76). Engine validation, doc-quality audits and ggcube comparisons sit in its subfolders.
