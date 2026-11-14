unpack_rgba <- function(image) {
  n <- dim(image)
  pixels <- as.vector(t(matrix(as.integer(image), n[1L], n[2L], byrow = TRUE)[n[1L]:1L, , drop = FALSE]))
  vapply(c(0L, 8L, 16L, 24L), function(shift) bitwAnd(bitwShiftR(pixels, shift), 255L) / 255,
         numeric(length(pixels)))
}
