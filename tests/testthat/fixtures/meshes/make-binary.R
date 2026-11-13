# Writes the binary mesh fixtures from the ASCII ones. Run from this folder.
tetra <- rbind(c(0, 0, 0), c(0, 1, 0), c(1, 0, 0), c(0, 0, 0), c(1, 0, 0), c(0, 0, 1),
  c(0, 0, 0), c(0, 0, 1), c(0, 1, 0), c(1, 0, 0), c(0, 1, 0), c(0, 0, 1))
con <- file("tetra-binary.stl", "wb")
# A header starting with "solid" must not make the file read as ASCII.
writeBin(charToRaw(formatC("solid binary tetra", width = -80)), con)
writeBin(4L, con, size = 4L, endian = "little")
for (f in 1:4) {
  writeBin(c(0, 0, 0, as.vector(t(tetra[3 * (f - 1) + 1:3, ]))), con, size = 4L, endian = "little")
  writeBin(0L, con, size = 2L, endian = "little")
}
close(con)

xyz <- as.matrix(expand.grid(x = 0:1, y = 0:1, z = 0:1))[c(1, 2, 4, 3, 5, 6, 8, 7), ]
faces <- list(c(0, 3, 2, 1), c(4, 5, 6, 7), c(0, 1, 5, 4), c(1, 2, 6, 5), c(2, 3, 7, 6), c(3, 0, 4, 7))
con <- file("cube-binary.ply", "wb")
writeChar(paste0(paste("ply", "format binary_little_endian 1.0", "element vertex 8", "property double x",
  "property double y", "property double z", "property uchar red", "property uchar green", "property uchar blue",
  "element face 6", "property list uchar uint vertex_indices", "property uchar red", "property uchar green",
  "property uchar blue", "end_header", sep = "\n"), "\n"), con, eos = NULL)
for (i in 1:8) {
  writeBin(as.double(xyz[i, ]), con, size = 8L, endian = "little")
  writeBin(as.integer(c(i * 30, 255 - i * 30, 7)), con, size = 1L)
}
for (f in faces) {
  writeBin(4L, con, size = 1L)
  writeBin(as.integer(f), con, size = 4L, endian = "little")
  writeBin(c(200L, 100L, 50L), con, size = 1L)
}
close(con)
