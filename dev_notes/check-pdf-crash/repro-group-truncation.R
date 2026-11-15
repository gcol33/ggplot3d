library(grid)
path <- tempfile(fileext = ".pdf")
pdf(path, compress = FALSE)
grid.group(gTree(children = do.call(gList, lapply(seq_len(1200), function(i) rectGrob(width = i / 1200, height = i / 1200)))))
dev.off()

raw <- readBin(path, "raw", file.size(path))
txt <- rawToChar(raw[raw != as.raw(0)]); Encoding(txt) <- "bytes"
m <- gregexpr("/Length (\\d+)\\s*>>\\s*stream\\r?\\n", txt, perl = TRUE, useBytes = TRUE)[[1]]
for (k in seq_along(m)) {
  start <- m[k] + attr(m, "match.length")[k]
  len <- as.integer(sub("/Length (\\d+).*", "\\1", regmatches(txt, m)[k]))
  end <- regexpr("endstream", substring(txt, start), fixed = TRUE, useBytes = TRUE)
  cat(sprintf("stream %d: /Length %d, bytes before endstream %d\n", k, len, end - 1L))
}
cat(R.version.string, "\n")
unlink(path)
