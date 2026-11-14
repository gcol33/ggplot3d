# R PDF device crash during package checks

R 4.6.1 on Windows exits with -1073741819 (0xC0000005) while closing the PDF device used for examples. The full examples file and devtools::run_examples(run_donttest = TRUE) both reproduce it. Windows Application Error records grDevices.dll, offset 0x1792e.

The standalone repro-grid-clips.R uses only the base grid package. It creates 12,000 clipping paths and crashes at dev.off() with the same exception and instruction.

The debugger stops at the assignment pd->pos[++pd->nobjs] = ftell(pd->pdffp) in PDFwriteDefinitions(). PDF_endfile() reserves offset storage using raster, mask and pattern counts and the resource-dictionary count. PDFwriteDefinitions() subsequently writes every definition, including placeholders for clipping paths. The resulting writes exceed the pos allocation because the reservation omits numDefns. The debugger labels the stripped function by its nearest exported symbol, PicTeX; the fault maps to the PDF definition-writing loop.

Source: https://svn.r-project.org/R/trunk/src/library/grDevices/src/devPS.c
Mirror used to retrieve the source: https://github.com/wch/r-source/blob/trunk/src/library/grDevices/src/devPS.c

r-pdf-definition-capacity.patch adds numDefns to the final object-offset allocation. The zero-context patch applies cleanly to the downloaded source with git apply --unidiff-zero. Rebuilding R with this proposed patch remains required for source-level validation. The installed R files remain unchanged.

Run from the package root, with R_MAX_VSIZE=4Gb and Windows-compatible locale variables:

    Rscript --vanilla dev_notes/check-pdf-crash/repro-grid-clips.R

The initial tarball check reports 2 errors, 2 warnings and 1 note. One error is the native PDF crash. The other is the introductory vignette's missing-z demonstration, whose plot is drawn after knitr's error handler returns. Both warnings report absent built vignettes after --no-build-vignettes. The note treats the Wavefront OBJ fixture as a compiled object. Package tests pass with 8,109 assertions, zero failures, zero warnings and 15 skips.

The package fixes store the OBJ fixture as text and copy it to a temporary .obj path inside a shared test helper. The vignette explicitly prints the deliberately invalid plot inside its error handler. The native crash requires the upstream R PDF allocation fix.

The parent commit a7cd80d (849eaf1^) also exits at the saved examples' final dev.off() with -1073741819. Its own tarball check stops earlier at the nonplanar OBJ example, so the same corrected main examples were run against the installed parent package. This comparison retains the full drawing workload while removing the vendored Manifold implementation.

A controlled debugger run validates the allocation diagnosis: the base-grid reproducer reaches PDFwriteDefinitions() with 9 existing objects, 12,000 definitions and offset capacity 2,000. Increasing that process's offset storage to 12,509 entries before the loop makes the same reproducer close successfully and exit 0. This changes only the debugged process's allocation and leaves the system R installation unchanged. The proposed source patch performs the equivalent reservation from tempnobj plus numDefns.

Example bisection uses the generated 114-topic examples file. Topics 1-57 and 58-114 each exit with -1073741819. Topics 1-29 and 30-57 each exit 0. The failure accumulates across individually completing subsets; it does not identify one invalid example. The package-independent grid reproducer and the controlled offset-table reservation identify the native cause.
