# Upstream bugs

Bugs found in other projects while working on ggplot3d, and where they were reported.

## R grDevices: pdf() device (`src/library/grDevices/src/devPS.c`)

Account: R Bugzilla, gilles.colling051@gmail.com, created by Martin Maechler on 2026-11-14 after a request to bug-report-request@r-project.org.

| Bug | Filed | Summary | Status | Notes |
|---|---|---|---|---|
| [19323](https://bugs.r-project.org/show_bug.cgi?id=19323) | 2026-11-15 | Group content longer than about 16 kB is truncated while `/Length` keeps the full length (`catDefn()` grows by one `DEFBUFSIZE` step) | UNCONFIRMED | Report: `check-pdf-crash/bugzilla-report-group-truncation.md`; repro: `check-pdf-crash/repro-group-truncation.R`; patch: `check-pdf-crash/r-pdf-catdefn-growth.patch` |
| [19324](https://bugs.r-project.org/show_bug.cgi?id=19324) | 2026-11-15 | Writes past `pd->pos` in `PDF_endfile()` when a file holds many definitions such as clipping paths; crashes at `dev.off()` | UNCONFIRMED | Report: `check-pdf-crash/bugzilla-report.md`; repro: `check-pdf-crash/repro-grid-clips.R`; patch: `check-pdf-crash/r-pdf-definition-capacity.patch`; diagnosis: `check-pdf-crash/diagnosis.md` |

Both reproduced on R 4.6.1 (Windows 11); the code is unchanged in trunk as of devPS.c's 2026-08-31 revision. Neither patch has been tested in a rebuilt R.

Effect on ggplot3d: 19324 crashed the `R CMD check` examples, which all go to one standard PDF device. 19323 breaks any group with more than about 16 kB of content drawn on `pdf()`.

Handled in ggplot3d (81f927c): quadric runs are painted as exact clipped geometry, without one clipping path per run; `grid_capabilities_3d()$large_definitions` is FALSE on `pdf()`, and there clipping paths replace masks and group compositing is off. Check: every help example rendered to `pdf(compress = FALSE)` with each stream's `/Length` compared to its content, 0 of 142 topics truncated, no crash. Once R fixes both, `large_definitions` can be gated on the R version that carries the fix.
