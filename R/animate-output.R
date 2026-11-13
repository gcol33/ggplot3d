# Animation outputs keyed by file extension. `device` names the graphics device
# each frame is drawn on; `kind` is "encode" (frames drawn to temporary PNG
# files and encoded into `file`), "sequence" (one file per frame, named from
# `file`) or "pages" (every frame a page of `file`, drawn in one process).
# Without a file, frames stay as temporary PNG files ("frames").
motion_outputs_3d <- list(
  gif = list(device = "png", kind = "encode", packages = "gifski",
    encode = function(paths, file, width, height, fps, progress) {
      gifski::gifski(paths, gif_file = file, width = width, height = height,
        delay = 1 / fps, progress = progress)
    }),
  mp4 = list(device = "png", kind = "encode", packages = "av",
    encode = function(paths, file, width, height, fps, progress) {
      av::av_encode_video(paths, output = file, framerate = fps, verbose = progress)
    }),
  apng = list(device = "png", kind = "encode", packages = character(),
    encode = function(paths, file, width, height, fps, progress) write_apng_3d(paths, file, fps)),
  png = list(device = "png", kind = "sequence", packages = character()),
  svg = list(device = "svg", kind = "sequence", packages = "svglite"),
  pdf = list(device = "pdf", kind = "pages", packages = character())
)
motion_outputs_3d$webm <- motion_outputs_3d$mp4

motion_devices_3d <- list(
  png = function(file, width, height, dpi) {
    ragg::agg_png(file, width = width, height = height, res = dpi, units = "px")
  },
  svg = function(file, width, height, dpi) {
    svglite::svglite(file, width = width / dpi, height = height / dpi)
  },
  pdf = function(file, width, height, dpi) {
    grDevices::pdf(file, width = width / dpi, height = height / dpi, onefile = TRUE)
  }
)

motion_output_3d <- function(file) {
  if (is.null(file)) {
    motion_require_3d("ragg")
    return(list(extension = NULL, device = "png", kind = "frames", packages = character()))
  }
  scene_assert(is.character(file) && length(file) == 1L && !is.na(file) && nzchar(file),
    "file must be a single output path")
  extension <- tolower(tools::file_ext(file))
  scene_assert(extension %in% names(motion_outputs_3d),
    paste0("file must end in ", paste0(".", sort(names(motion_outputs_3d)), collapse = ", ")))
  output <- motion_outputs_3d[[extension]]
  for (package in c(output$packages, if (output$device == "png") "ragg")) motion_require_3d(package)
  scene_assert(dir.exists(dirname(file)), "The output directory does not exist")
  c(list(extension = extension), output)
}

motion_open_device_3d <- function(device, file, width, height, dpi) {
  motion_devices_3d[[device]](file, width, height, dpi)
  current <- grDevices::dev.cur()
  scene_assert(current > 1L, "could not open a graphics device for animation frames")
  current
}

# One file per frame: a sprintf() pattern such as "frame-%03d.png" is filled
# with the frame number, otherwise the number is appended to the file name.
motion_sequence_paths_3d <- function(file, frames) {
  if (grepl("%", basename(file), fixed = TRUE)) {
    paths <- sprintf(file, seq_len(frames))
  } else {
    digits <- max(4L, nchar(frames))
    paths <- paste0(tools::file_path_sans_ext(file), "-", formatC(seq_len(frames), width = digits, flag = "0"),
      ".", tools::file_ext(file))
  }
  scene_assert(!anyDuplicated(paths), "the file pattern must give every frame its own name")
  paths
}

png_uint32_3d <- function(x) {
  x <- as.double(x)
  as.raw(c(x %/% 2^24, (x %/% 2^16) %% 256, (x %/% 2^8) %% 256, x %% 256))
}

png_uint16_3d <- function(x) as.raw(c(x %/% 256, x %% 256))

png_chunk_3d <- function(type, data = raw()) {
  body <- c(charToRaw(type), data)
  c(png_uint32_3d(length(data)), body, png_uint32_3d(png_crc32_cpp(body)))
}

png_chunks_3d <- function(path) {
  bytes <- readBin(path, "raw", n = file.info(path)$size)
  signature <- as.raw(c(137, 80, 78, 71, 13, 10, 26, 10))
  scene_assert(length(bytes) > 8L && identical(bytes[1:8], signature), "animation frame is not a PNG file")
  chunks <- list()
  position <- 9L
  while (position + 7L <= length(bytes)) {
    size <- sum(as.double(as.integer(bytes[position + 0:3])) * 256^(3:0))
    type <- rawToChar(bytes[position + 4:7])
    data <- if (size > 0) bytes[position + 8L + seq_len(size) - 1L] else raw()
    chunks[[length(chunks) + 1L]] <- list(type = type, data = data)
    position <- position + 12L + size
    if (type == "IEND") break
  }
  chunks
}

# Animated PNG (APNG 1.0): the first frame's IDAT data is the default image,
# later frames carry the same compressed streams as fdAT chunks.
write_apng_3d <- function(paths, file, fps) {
  frames <- lapply(paths, png_chunks_3d)
  types <- function(chunks) vapply(chunks, `[[`, character(1), "type")
  header <- frames[[1L]][[1L]]
  scene_assert(identical(header$type, "IHDR"), "animation frame lacks a PNG header")
  for (chunks in frames) {
    scene_assert(identical(chunks[[1L]], header), "animation frames must share one size and pixel format")
  }
  first <- frames[[1L]]
  before <- seq_len(match("IDAT", types(first)) - 1L)[-1L]
  ancillary <- Filter(function(chunk) !chunk$type %in% c("IHDR", "IDAT", "IEND", "acTL", "fcTL", "fdAT"), first[before])
  delay <- if (abs(fps - round(fps)) < 1e-9 && fps <= 65535) c(1, round(fps)) else
    c(min(65535, max(1, round(1000 / fps))), 1000)
  sequence <- 0
  out <- list(as.raw(c(137, 80, 78, 71, 13, 10, 26, 10)), png_chunk_3d("IHDR", header$data),
    png_chunk_3d("acTL", c(png_uint32_3d(length(frames)), png_uint32_3d(0))))
  out <- c(out, lapply(ancillary, function(chunk) png_chunk_3d(chunk$type, chunk$data)))
  width <- header$data[1:4]; height <- header$data[5:8]
  for (k in seq_along(frames)) {
    out[[length(out) + 1L]] <- png_chunk_3d("fcTL", c(png_uint32_3d(sequence), width, height,
      png_uint32_3d(0), png_uint32_3d(0), png_uint16_3d(delay[1L]), png_uint16_3d(delay[2L]),
      as.raw(0), as.raw(0)))
    sequence <- sequence + 1
    for (chunk in Filter(function(chunk) chunk$type == "IDAT", frames[[k]])) {
      out[[length(out) + 1L]] <- if (k == 1L) png_chunk_3d("IDAT", chunk$data) else {
        sequence <- sequence + 1
        png_chunk_3d("fdAT", c(png_uint32_3d(sequence - 1), chunk$data))
      }
    }
  }
  out[[length(out) + 1L]] <- png_chunk_3d("IEND")
  writeBin(unlist(out, use.names = FALSE), file)
  invisible(file)
}
