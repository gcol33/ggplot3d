usage <- tools::checkDocFiles(dir = ".")
if (length(usage) || length(attr(usage, "bad_lines"))) {
  print(usage)
  stop("Rd usage preflight failed.")
}
width <- tools:::.check_Rd_line_widths(dir = ".")
if (length(width)) {
  print(width)
  stop("Rd width preflight failed.")
}
cat("RD_PREFLIGHT_OK\n")
