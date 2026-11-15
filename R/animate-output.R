# Animation outputs keyed by file extension. `device` names the graphics device
# each frame is drawn on; `kind` is "encode" (frames drawn to temporary PNG
# files and encoded into `file`), "sequence" (one file per frame, named from
# `file`), "pages" (every frame a page of `file`, drawn in one process), or
# "scene" (captured geometry and camera tracks written to one model file).
# Without a file or format, frames stay as temporary PNG files ("frames").
motion_outputs_3d <- list2env(list(
  gif = list(device = "png", kind = "encode", packages = character(),
    encode = function(paths, file, width, height, fps, progress) write_gif_3d(paths, file, fps)),
  mp4 = list(device = "png", kind = "encode", packages = "av",
    encode = function(paths, file, width, height, fps, progress) {
      av::av_encode_video(paths, output = file, framerate = fps, verbose = progress)
    }),
  apng = list(device = "png", kind = "encode", packages = character(),
    encode = function(paths, file, width, height, fps, progress) write_apng_3d(paths, file, fps)),
  png = list(device = "png", kind = "sequence", packages = character()),
  svg = list(device = "svg", kind = "sequence", packages = "svglite"),
  pdf = list(device = "pdf", kind = "pages", packages = character())
), parent = emptyenv())
motion_outputs_3d$webm <- motion_outputs_3d$mp4

motion_devices_3d <- list(
  png = function(file, width, height, dpi, bg) {
    ragg::agg_png(file, width = width, height = height, res = dpi, units = "px", background = bg)
  },
  svg = function(file, width, height, dpi, bg) {
    svglite::svglite(file, width = width / dpi, height = height / dpi, bg = bg)
  },
  pdf = function(file, width, height, dpi, bg) {
    grDevices::pdf(file, width = width / dpi, height = height / dpi, onefile = TRUE, bg = bg)
  }
)

motion_output_3d <- function(file, format = NULL) {
  known <- ls(motion_outputs_3d)
  if (!is.null(format)) {
    scene_assert(is.character(format) && length(format) == 1L && !is.na(format) && nzchar(format),
      "format must be a single output format or NULL")
    format <- check_choice_3d(tolower(format), known, "format")
  }
  if (is.null(file) && !is.null(format)) file <- tempfile("ggplot3d-animation-", fileext = paste0(".", format))
  if (is.null(file)) {
    motion_require_3d("ragg")
    return(list(file = NULL, extension = NULL, device = "png", kind = "frames", packages = character()))
  }
  scene_assert(is.character(file) && length(file) == 1L && !is.na(file) && nzchar(file),
    "file must be a single output path")
  extension <- tolower(tools::file_ext(file))
  scene_assert(extension %in% known,
    paste0("file must end in ", paste0(".", known, collapse = ", ")))
  scene_assert(is.null(format) || identical(extension, format), "file extension must agree with format")
  output <- motion_outputs_3d[[extension]]
  for (package in c(output$packages, if (output$device == "png") "ragg")) motion_require_3d(package)
  scene_assert(dir.exists(dirname(file)), "The output directory does not exist")
  c(list(file = file, extension = extension), output)
}

# The page behind each frame takes the fill of the plot's background element,
# as ggplot2::ggsave() does, so a plot drawn at a fixed aspect ratio has no
# margins of another colour around it. GIF and video frames carry no partial
# transparency, so a transparent background becomes white there.
motion_background_3d <- function(plot, extension = NULL) {
  fill <- ggplot2::calc_element("plot.background", ggplot2::complete_theme(plot$theme))$fill
  transparent <- is.null(fill) || is.na(fill) || identical(fill, "transparent") ||
    grDevices::col2rgb(fill, alpha = TRUE)[4L] == 0L
  if (!transparent) fill else if (isTRUE(extension %in% c("gif", "mp4", "webm"))) "white" else "transparent"
}

motion_open_device_3d <- function(device, file, width, height, dpi, bg = "white") {
  motion_devices_3d[[device]](file, width, height, dpi, bg)
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

# Pixels of a PNG frame as RGB bytes: the IDAT streams are concatenated,
# inflated and unfiltered (8-bit, non-interlaced images, as ragg writes them).
png_rgb_3d <- function(path) {
  chunks <- png_chunks_3d(path)
  header <- chunks[[1L]]$data
  scene_assert(identical(chunks[[1L]]$type, "IHDR"), "animation frame lacks a PNG header")
  number <- function(bytes) sum(as.double(as.integer(bytes)) * 256^(3:0))
  depth <- as.integer(header[9L]); type <- as.integer(header[10L]); interlace <- as.integer(header[13L])
  channels <- c(`0` = 1L, `2` = 3L, `4` = 2L, `6` = 4L)[as.character(type)]
  scene_assert(depth == 8L && !is.na(channels) && interlace == 0L,
    "animation frames must be 8-bit, non-interlaced grey or RGB PNG files")
  data <- unlist(lapply(Filter(function(chunk) chunk$type == "IDAT", chunks), `[[`, "data"), use.names = FALSE)
  width <- number(header[1:4]); height <- number(header[5:8])
  list(width = width, height = height,
    rgb = png_unfilter_cpp(memDecompress(data, type = "gzip"), width, height, channels))
}

# GIF89a with one palette for all frames and frame differences coded as
# transparent pixels (gif_encode_cpp). Delays are whole hundredths of a
# second, rounded on the running clock so the average rate stays `fps`.
write_gif_3d <- function(paths, file, fps, colours = 255L) {
  frames <- lapply(paths, png_rgb_3d)
  width <- frames[[1L]]$width; height <- frames[[1L]]$height
  clock <- round(100 * seq(0, length(frames)) / fps)
  bytes <- gif_encode_cpp(lapply(frames, `[[`, "rgb"), width, height, as.integer(diff(clock)), as.integer(colours))
  writeBin(bytes, file)
  invisible(file)
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
