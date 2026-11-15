Product: R
Component: Graphics
Version: R 4.6.1 (code unchanged in R-devel trunk, devPS.c last changed 2026-08-31)
Hardware/OS: x86_64 Windows 11

Summary: pdf() device writes past pd->pos in PDF_endfile() when a file holds many definitions (e.g. clipping paths)

Description:

When a pdf() file holds many definitions, closing the device crashes R. On
Windows the process exits with 0xC0000005 (access violation) inside
grDevices.dll. A file with a few thousand clipping paths is enough:

    library(grid)
    path <- tempfile(fileext = ".pdf")
    pdf(path)
    grid.newpage()
    for (i in seq_len(12000)) {
      pushViewport(viewport(clip = circleGrob()))
      grid.rect()
      popViewport()
    }
    dev.off()   # R exits with -1073741819 (0xC0000005) here

Run with Rscript --vanilla on R 4.6.1, Windows 11. Only base grid is used.

PDF_endfile() grows pd->pos twice before writing the trailing objects. The
first growth reserves room for rasters, masks and patterns
(pd->nobjs + nraster + nmask + npattern + 500). The second reserves
tempnobj + 500, where tempnobj is the object count returned by
PDFwriteResourceDictionary(). Neither reservation counts pd->numDefns.
PDFwriteDefinitions(), called near the end of PDF_endfile(), then writes one
object per definition:

    pd->pos[++pd->nobjs] = (int) ftell(pd->pdffp);

including the empty placeholder objects for clipping paths, which are written
inline where they are used. With 12,000 clipping paths the loop starts with
9 objects already written, 12,000 definitions to write and room for 2,000
offsets, so it writes past the end of pd->pos.

I checked this in a debugger: the fault is at the line above in
PDFwriteDefinitions(). Enlarging pd->pos in the debugged process to 12,509
entries before the loop lets the same script close the device and exit 0.

The crash depends on the total number of definitions in one file, so it also
appears when many small, individually harmless plots go to one pdf() device.
I hit it through R CMD check, where all examples of a package go to one PDF:
two halves of the examples each closed cleanly, the full set crashed.

A possible fix is to count the definitions in the second reservation
(against trunk; I have not rebuilt R with it):

--- a/src/library/grDevices/src/devPS.c
+++ b/src/library/grDevices/src/devPS.c
@@ -7945,8 +7945,8 @@
 	fprintf(pd->pdffp, "endobj\n");
     }
 
-    if(tempnobj >= pd->max_nobjs) {
-	int new = tempnobj + 500;
+    if(tempnobj + pd->numDefns >= pd->max_nobjs) {
+	int new = tempnobj + pd->numDefns + 500;
 	void *tmp = realloc(pd->pos, new * sizeof(int));
 	if(!tmp)
 	    error("unable to increase object limit: please shutdown the pdf device");

Could you check whether this reservation is the right place, or whether the
definitions should be counted together with the rasters and masks in the
first one?
