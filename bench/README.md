# Referee and surface benchmarks

Run all commands from the repository root, using Windows R 4.6.1.

```powershell
& 'C:/Program Files/R/R-4.6.1/bin/Rscript.exe' bench/run-referee.R
& 'C:/Program Files/R/R-4.6.1/bin/Rscript.exe' bench/preview-referee.R
powershell.exe -NoProfile -ExecutionPolicy Bypass -File bench/run-benchmarks.ps1
powershell.exe -NoProfile -ExecutionPolicy Bypass -File bench/run-benchmarks.ps1 -Packages ggplot3d -CellSizes 100 -Mode profile
```

The matrix runner scores every fixture diagnostically. Testthat additionally
gates acceptance on the returned `exact_visibility` and `footprint_clipping`
capabilities. `referee-matrix.csv` preserves both results. Each failure saves an
RDS reproduction with input packet, camera and incorrect pixel locations. The
preview script compares independent rays on the left with painted compiled
fragments on the right, including alpha compositing.

The PowerShell benchmark launcher starts one package/grid worker at a time.
Each worker runs an immutable copy of the script; R source checksums are retained.
Windows job objects constrain each worker and its descendants to less than
4 GiB of committed memory. R allocation failures are recorded, and a hard process
termination retains its exit code, stderr and the last complete timing stage.
Peak committed memory is queried from the job object. Each grid has exactly
50 x 50, 100 x 100, 200 x 200 or 316 x 316 cells, with one additional vertex on
each axis. ggplot3d triangulates each cell; ggcube 0.3.0 retains quads. Both receive
the same deterministic input coordinates and the same explicit transparent
panel border. See `visual-defect.md` for the border reproduction.

Three repetitions measure build, gtable, PNG/SVG `ggsave(gtable)` and PNG/SVG
`ggsave(plot)` separately. The latter two include the complete public plotting
pipeline. Surface compilation is deferred until device drawing. Medians require
three complete repetitions; partial runs retain their raw stage timings. SVG
bytes describe the gtable output. Profile mode uses a separate process for the
100 x 100 ggplot3d build/gtable and direct PNG save, with a 0.01 s sampling interval.
The profile is retained even if that attempt raises an R error.

Raw timings, profiles, device outputs, session information, engine DLL checksum,
hardware and memory logs live under `raw-<date>/`. `summarise.R` produces dated
CSV and Markdown summaries. `run-date.txt` fixes the experiment date across
overnight execution; set it before starting a new experiment. Background workloads belonging to other sessions are
left running. Benchmark numbers therefore describe the host's ambient load.

The test suite uses `testthat`; preview and benchmark scripts use `ragg` and
`svglite`, already in Suggests. `ggcube` 0.3.0 is a benchmark-only requirement.
`devtools` is required by the explicitly requested development runners. The
oracle itself uses base R and needs no additional package.
