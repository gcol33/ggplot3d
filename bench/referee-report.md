# Phase 1 referee report

Benchmarking remains in progress. Tables contain completed measurements and earlier attempt diagnostics; unfinished cases are being run sequentially under Task Scheduler.

## Files

- `tests/fixtures/oracle/reference.R` and its README: independent camera, rays, coverage, rasteriser and scorer.
- `tests/fixtures/scenes/builders.R`: fourteen packet builders and three camera specifications.
- `tests/testthat/test-referee-oracle.R`, `test-referee-scenes.R`, `test-referee-scales.R`: analytical validation, capability-gated fixture scoring and public scale checks.
- `bench/`: sequential memory-limited benchmark launcher/worker, raw logs, device reproductions, profiles, matrix, previews and dated results.

No commits were created. Changes are confined to the assigned test and benchmark paths.

## Oracle and validation

The oracle derives its camera from the contract, including sphere framing, perspective distance, axial depth, roll and pole orientation. It clips source polygons independently, casts pixel-centre rays, uses barycentric triangle intersections and sorts hits front to back. Physical strokes interpolate reciprocal depth under perspective; markers use constant anchor depth and measured footprints. Alpha lists stop at the first opaque hit. Source primitive identities remain distinct across surfaces and layers.

The compiled rasteriser paints projected fragments in draw_order using parent_primitive_id and masks. Face coverage uses a projected even-odd test. The scorer compares visible IDs and, for translucent scenes, full ordered ID/alpha lists. It excludes a one-pixel Euclidean band around source and clipping edges. Fixture circles and butt strokes use exact screen-distance edge bands. BSP cuts remain scored. Acceptance is zero mismatched interior pixels.

Analytical tests cover camera axes and framing in both projections; pole/roll orientation; constant and tilted plane depths; both halves of crossing quads; all three fronts in a cyclic strip scene; box/near clipping; nearby noncoplanar faces at the contract tolerance; circle, polygon-hole and stroke footprints; opacity-zero and alpha weights. A manually constructed compiled scene proves that reversing paint order produces mismatches without a production render. The dense 40 x 40 surface fixtures use zoom 2.5 to retain sampleable triangle interiors at 160 x 160.

The oracle explicitly rejects text, overlay partitions, retained hidden-stroke policies and bent paths. These semantics need additional independent reference coverage before accepting corresponding engine claims.

## Fixture x camera matrix

Cells show diagnostic interior mismatches / scored foreground pixels. PASS means zero mismatches. Capability-gated acceptance tests retain their separate SKIP status until the engine enables the returned capability flags.

| Fixture | Orthographic oblique | Perspective | Orthographic pole |
|---|---:|---:|---:|
| A_separate | PASS 0 / 801 | PASS 0 / 766 | PASS 0 / 4556 |
| A_reversed | PASS 0 / 801 | PASS 0 / 766 | PASS 0 / 4556 |
| A_grouped | PASS 0 / 801 | PASS 0 / 766 | PASS 0 / 4556 |
| B_surface_first | PASS 0 / 3588 | PASS 0 / 2931 | PASS 0 / 7682 |
| B_surface_last | PASS 0 / 3588 | PASS 0 / 2931 | PASS 0 / 7682 |
| C_columns | PASS 0 / 4500 | PASS 0 / 4194 | PASS 0 / 6776 |
| G1_crossing | PASS 0 / 4630 | PASS 0 / 5587 | PASS 0 / 5184 |
| G2_cycle | PASS 0 / 924 | PASS 0 / 649 | PASS 0 / 1316 |
| near_crossing | PASS 0 / 19 | PASS 0 / 451 | PASS 0 / 2520 |
| coplanar_layers | PASS 0 / 2477 | PASS 0 / 1896 | PASS 0 / 5256 |
| translucent_points | PASS 0 / 2478 | PASS 0 / 1785 | PASS 0 / 5092 |
| thick_line | PASS 0 / 2320 | PASS 0 / 1788 | PASS 0 / 4880 |
| marker_silhouette | PASS 0 / 2424 | PASS 0 / 1833 | PASS 0 / 5224 |
| pole | PASS 0 / 4222 | PASS 0 / 4491 | PASS 0 / 5184 |

Diagnostic combinations: 42 ; zero-mismatch combinations: 42

The full CSV includes scored background and foreground totals, skipped capability names and mismatch counts. Preview PNGs place the oracle on the left and compiled painting on the right.

Final testthat run: passed assertions 40 ; failed assertions 0 ; error tests 0 ; skipped tests 43 ; warnings 0

Returned engine capabilities:

```
list(visibility = "bsp", exact_visibility = FALSE, footprint_clipping = FALSE)
```

Referee DLL MD5: 90dd3e32e3c8c549a8cfdfcf89446472
Measured: 2026-11-13 05:18:19.158455

# Surface benchmarks

Date: 2026-11-13

Median of three complete repetitions per package and cell grid. Each package/grid runs in a fresh R process, sequentially, with a Windows job memory limit below 4 GiB. Both render deterministic z = sin(pi*x) cos(pi*y), without lighting, at 7 inches square and 150 DPI. ggplot3d emits two triangles per cell; ggcube emits one quad. Default sorting is used for each package.

Build and gtable timings exclude drawing. Grob device timings include deferred scene compilation. Direct ggsave timings include building, gtable assembly and device drawing. SVG bytes refer to grob output. Incomplete repetitions have no reported median.

| Package | Cells | Faces | Build s | Gtable s | PNG grob s | SVG grob s | ggsave PNG s | ggsave SVG s | SVG bytes | Status |
|---|---:|---:|---:|---:|---:|---:|---:|---:|---:|---|
| ggplot3d | 50 x 50 | 5000 | 0.390 | 0.610 | 76.400 | 76.500 | 81.820 | 102.010 | 20810611 | 3 complete repeats; worker exit 1 (see stderr) |
| ggplot3d | 100 x 100 | 20000 | NA | NA | NA | NA | NA | NA | NA | process terminated; see stderr and exit code |
| ggplot3d | 200 x 200 | 80000 | NA | NA | NA | NA | NA | NA | NA | Error in `load_all()`: ! At 'R/layers-surface.R:47:114': unexpected ')' |
| ggplot3d | 316 x 316 | 199712 | NA | NA | NA | NA | NA | NA | NA | process terminated; see stderr and exit code |
| ggcube | 50 x 50 | 2500 | 0.210 | 0.410 | 0.200 | 0.180 | 0.910 | 0.810 | 350448 | ok |
| ggcube | 100 x 100 | 10000 | 0.470 | 1.720 | 0.220 | 0.210 | 2.460 | 2.490 | 1369418 | ok |
| ggcube | 200 x 200 | 40000 | 1.700 | 7.440 | 0.290 | 0.420 | 9.480 | 10.020 | 5446708 | ok |
| ggcube | 316 x 316 | 99856 | 4.200 | 17.110 | 0.390 | 0.950 | 21.800 | 21.970 | 13581989 | ok |

Engine DLL MD5: 90dd3e32e3c8c549a8cfdfcf89446472

## Rprof

Sampling interval: 0.01 s; 100 x 100 direct PNG ggsave. Self time includes R-visible native entry points.

The direct PNG profile is partial:
Rprof: native-phase sampling stopped before PNG completion; a complete profile remains queued.
These self times describe that captured phase.

| Function | Self seconds | Self percent |
|---|---:|---:|
| `.Call` | 657.39 | 99.91 |
| `grDevices::col2rgb` | 0.28 | 0.04 |
| `match` | 0.05 | 0.01 |
| `grid.Call` | 0.03 | 0 |
| `findCenvVar` | 0.02 | 0 |

Completed build/gtable profile, measured separately:

| Function | Self seconds | Self percent |
|---|---:|---:|
| `grDevices::col2rgb` | 0.26 | 76.47 |
| `FUN` | 0.01 | 2.94 |
| `emit_scene_3d` | 0.01 | 2.94 |
| `isTRUE` | 0.01 | 2.94 |
| `fetch_ggproto` | 0.01 | 2.94 |

## Requested shared edits and dependencies

The shared R panel assembly should render panel borders with transparent fill. `check-devices.R` reproduces the white rectangle covering the scene under theme_bw(); `visual-defect.md` records the visible failure and the explicit benchmark setting. The referee task does not edit R/.

The orchestrator can review the diagnostic matrix before enabling exact_visibility and footprint_clipping. The suite gates acceptance on those returned flags and names a missing flag in each skip.

testthat, ragg and svglite are already in Suggests. The oracle adds no dependency. ggcube 0.3.0 is required only for the benchmark comparison and is a proposed benchmark Suggests entry. devtools is a development-runner requirement. No DESCRIPTION or NAMESPACE edits were made.
