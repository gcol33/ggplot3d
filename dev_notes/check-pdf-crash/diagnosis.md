# R PDF device crash during package checks

R 4.6.1 on Windows exits with -1073741819 (0xC0000005) while closing the PDF device used for examples. The full examples file and devtools::run_examples(run_donttest = TRUE) both reproduce it. Windows Application Error records grDevices.dll, offset 0x1792e.

The standalone repro-grid-clips.R uses only the base grid package. It creates 12,000 clipping paths and crashes at dev.off() with the same exception and instruction. ggplot3d and Manifold are absent from that process.

The debugger stops at the assignment pd->pos[++pd->nobjs] = ftell(pd->pdffp) in PDFwriteDefinitions(). PDF_endfile() reserves offset storage using raster, mask and pattern counts and the resource-dictionary count. It omits numDefns, although PDFwriteDefinitions() subsequently writes every definition, including placeholders for clipping paths. The resulting writes exceed the pos allocation. The debugger labels the stripped function by its nearest exported symbol, PicTeX; the fault is in the PDF definition-writing loop, not the PicTeX device.

Source: https://svn.r-project.org/R/trunk/src/library/grDevices/src/devPS.c
Mirror used to retrieve the source: https://github.com/wch/r-source/blob/trunk/src/library/grDevices/src/devPS.c

r-pdf-definition-capacity.patch adds numDefns to the final object-offset allocation. The patch applies cleanly to the downloaded source. It is a proposed upstream fix, with no rebuilt-R validation yet. The installed R files have not been changed.

Run from the package root, with R_MAX_VSIZE=4Gb and Windows-compatible locale variables:

    Rscript --vanilla dev_notes/check-pdf-crash/repro-grid-clips.R

The initial tarball check reports 2 errors, 2 warnings and 1 note. One error is the native PDF crash. The other is the introductory vignette's missing-z demonstration, whose plot is drawn after knitr's error handler returns. Both warnings report absent built vignettes after --no-build-vignettes. The note treats the Wavefront OBJ fixture as a compiled object. Package tests pass with 8,109 assertions, zero failures, zero warnings and 15 skips.

The package fixes store the OBJ fixture as text and copy it to a temporary .obj path inside a shared test helper. The vignette explicitly prints the deliberately invalid plot inside its error handler. The native crash requires the upstream R fix; changing the Manifold implementation or disabling examples cannot repair the PDF allocator.

The parent commit a7cd80d (849eaf1^) also exits at the saved examples' final dev.off() with -1073741819. Its own tarball check stops earlier at the nonplanar OBJ example, so the same corrected main examples were run against the installed parent package. This comparison retains the full drawing workload while removing the vendored Manifold implementation.

A controlled debugger run validates the allocation diagnosis: the base-grid reproducer reaches PDFwriteDefinitions() with 9 existing objects, 12,000 definitions and offset capacity 2,000. Increasing that process's offset storage to 12,509 entries before the loop makes the same reproducer close successfully and exit 0. This changes only the debugged process's allocation and leaves the system R installation unchanged. The proposed source patch performs the equivalent reservation from tempnobj plus numDefns.
