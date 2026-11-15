Product: R
Component: Graphics
Version: R 4.6.1 (code unchanged in R-devel trunk, devPS.c last changed 2026-08-31)
Hardware/OS: x86_64 Windows 11

Summary: pdf(): content of a group longer than about 16 kB is truncated while /Length keeps the full length

Description:

When the content drawn inside a group on a pdf() device is long, the group's
stream in the file is cut off, but its /Length entry gives the full length.
The resulting PDF is broken: the stream ends in the middle of an operator and
/Length points past "endstream".

    library(grid)
    path <- tempfile(fileext = ".pdf")
    pdf(path, compress = FALSE)
    grid.group(gTree(children = do.call(gList, lapply(seq_len(1200),
      function(i) rectGrob(width = i / 1200, height = i / 1200)))))
    dev.off()

With R 4.6.1 (Rscript --vanilla, Windows 11) the group's form XObject says
/Length 39313, but only 16,226 bytes are written before "endstream". 490 of
the 1,200 rectangles are in the file and the last line reads
"148.89 14endstream". With 400 rectangles (13,163 bytes of content) the
stream is complete.

The content is copied into the XObject definition with copyDefn(), which calls
catDefn() once with the whole content string. catDefn() grows the destination
string by a single DEFBUFSIZE (8192) step:

    if (len + buflen + 1 >= pd->definitions[i].nchar) {
        pd->definitions[i].nchar = pd->definitions[i].nchar + DEFBUFSIZE;
        ...
    }
    strncat(pd->definitions[i].str, buf,
            pd->definitions[i].nchar - strlen(pd->definitions[i].str) - 1);

When buf is longer than one step, strncat() stops at the capacity and the rest
of the content is dropped. The /Length was written earlier from
strlen() of the full content string, so the two disagree. The same
copyDefn() path is used for masks, tiling patterns and compositing groups, so I
expect those to truncate in the same way, though I only tested a plain group.

A possible fix is to grow until the string fits (against trunk; I have not
rebuilt R with it):

--- a/src/library/grDevices/src/devPS.c
+++ b/src/library/grDevices/src/devPS.c
@@ -5158,7 +5158,8 @@
     /* Grow definition string if necessary) */
     if (len + buflen + 1 >= pd->definitions[i].nchar) {
 	void *tmp;
-        pd->definitions[i].nchar = pd->definitions[i].nchar + DEFBUFSIZE;
+        while (len + buflen + 1 >= pd->definitions[i].nchar)
+            pd->definitions[i].nchar = pd->definitions[i].nchar + DEFBUFSIZE;
 	tmp = realloc(pd->definitions[i].str, 
                       (pd->definitions[i].nchar)*sizeof(char));
 	if (!tmp) error(_("failed to increase definition string (shut down PDF device)"));
