library(grid)
path <- tempfile(fileext = '.pdf')
pdf(path)
grid.newpage()
for (i in seq_len(12000)) {
  pushViewport(viewport(clip = circleGrob()))
  grid.rect()
  popViewport()
}
cat('Closing PDF with 12000 clipping paths:', path, '\n')
flush.console()
dev.off()
unlink(path)
cat('PDF closed\n')
