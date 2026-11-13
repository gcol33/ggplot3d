Sys.setlocale("LC_ALL", "C")
matrix <- read.csv("bench/referee-matrix.csv")
run_date <- readLines("bench/run-date.txt",warn=FALSE)[1]
directory <- file.path("bench",paste0("raw-",run_date))
exits <- as.vector(outer(c("ggplot3d","ggcube"),c(50,100,200,316),function(package,cells)
  file.path(directory,paste0(package,"-",cells,"-exit.txt"))))
complete <- all(file.exists(c(exits,file.path(directory,"ggplot3d-100-profile-run-exit.txt"))))
benchmark_status <- if(complete) "All planned benchmark and profile attempts have finished. Missing medians retain their recorded diagnostics." else
  "Benchmarking remains in progress. Tables contain completed measurements and earlier attempt diagnostics; unfinished cases are being run sequentially under Task Scheduler."
lines <- c("# Phase 1 referee report","",
  benchmark_status,"",
  "## Files","",
  "- `tests/fixtures/oracle/reference.R` and its README: independent camera, rays, coverage, rasteriser and scorer.",
  "- `tests/fixtures/scenes/builders.R`: fourteen packet builders and three camera specifications.",
  "- `tests/testthat/test-referee-oracle.R`, `test-referee-scenes.R`, `test-referee-scales.R`: analytical validation, capability-gated fixture scoring and public scale checks.",
  "- `bench/`: sequential memory-limited benchmark launcher/worker, raw logs, device reproductions, profiles, matrix, previews and dated results.","",
  "No commits were created. Changes are confined to the assigned test and benchmark paths.","",
  "## Oracle and validation","",
  "The oracle derives its camera from the contract, including sphere framing, perspective distance, axial depth, roll and pole orientation. It clips source polygons independently, casts pixel-centre rays, uses barycentric triangle intersections and sorts hits front to back. Physical strokes interpolate reciprocal depth under perspective; markers use constant anchor depth and measured footprints. Alpha lists stop at the first opaque hit. Source primitive identities remain distinct across surfaces and layers.","",
  "The compiled rasteriser paints projected fragments in draw_order using parent_primitive_id and masks. Face coverage uses a projected even-odd test. The scorer compares visible IDs and, for translucent scenes, full ordered ID/alpha lists. It excludes a one-pixel Euclidean band around source and clipping edges. Fixture circles and butt strokes use exact screen-distance edge bands. BSP cuts remain scored. Acceptance is zero mismatched interior pixels.","",
  "Analytical tests cover camera axes and framing in both projections; pole/roll orientation; constant and tilted plane depths; both halves of crossing quads; all three fronts in a cyclic strip scene; box/near clipping; nearby noncoplanar faces at the contract tolerance; circle, polygon-hole and stroke footprints; opacity-zero and alpha weights. A manually constructed compiled scene proves that reversing paint order produces mismatches without a production render. The dense 40 x 40 surface fixtures use zoom 2.5 to retain sampleable triangle interiors at 160 x 160.","",
  "The oracle explicitly rejects text, overlay partitions, retained hidden-stroke policies and bent paths. These semantics need additional independent reference coverage before accepting corresponding engine claims.","",
  "## Fixture x camera matrix","",
  "Cells show diagnostic interior mismatches / scored foreground pixels. PASS means zero mismatches. Capability-gated acceptance tests retain their separate SKIP status until the engine enables the returned capability flags.","",
  "| Fixture | Orthographic oblique | Perspective | Orthographic pole |",
  "|---|---:|---:|---:|")
for(name in unique(matrix$fixture)) {
  values <- vapply(c("oblique","perspective","pole"),function(view) {
    row <- matrix[matrix$fixture==name & matrix$camera==view,,drop=FALSE]
    if(!nrow(row)) return("missing")
    paste0(row$diagnostic," ",row$wrong," / ",row$covered)
  },character(1))
  lines <- c(lines,paste0("| ",paste(c(name,values),collapse=" | ")," |"))
}
lines <- c(lines,"",paste("Diagnostic combinations:",nrow(matrix),"; zero-mismatch combinations:",sum(matrix$diagnostic=="PASS")),"",
  "The full CSV includes scored background and foreground totals, skipped capability names and mismatch counts. Preview PNGs place the oracle on the left and compiled painting on the right.")
if(file.exists("bench/referee-tests.csv")) {
  tests <- read.csv("bench/referee-tests.csv")
  lines <- c(lines,"",paste("Final testthat run: passed assertions",sum(tests$passed),
    "; failed assertions",sum(tests$failed),"; error tests",sum(tests$error),
    "; skipped tests",sum(tests$skipped),"; warnings",sum(tests$warning)))
}
if(file.exists("bench/referee-engine.rds")) {
  metadata <- readRDS("bench/referee-engine.rds")
  lines <- c(lines,"","Returned engine capabilities:","","```",capture.output(dput(metadata$capabilities)),"```","",
    paste("Referee DLL MD5:",unname(metadata$dll_md5)),paste("Measured:",metadata$measured_at))
}
summary <- file.path("bench",paste0("summary-",run_date,".md"))
if(file.exists(summary)) lines <- c(lines,"",readLines(summary))
lines <- c(lines,"","## Requested shared edits and dependencies","",
  "The shared R panel assembly should render panel borders with transparent fill. `check-devices.R` reproduces the white rectangle covering the scene under theme_bw(); `visual-defect.md` records the visible failure and the explicit benchmark setting. The referee task does not edit R/.","",
  "The orchestrator can review the diagnostic matrix before enabling exact_visibility and footprint_clipping. The suite gates acceptance on those returned flags and names a missing flag in each skip.","",
  "testthat, ragg and svglite are already in Suggests. The oracle adds no dependency. ggcube 0.3.0 is required only for the benchmark comparison and is a proposed benchmark Suggests entry. devtools is a development-runner requirement. No DESCRIPTION or NAMESPACE edits were made.")
writeLines(lines,"bench/referee-report.md")
