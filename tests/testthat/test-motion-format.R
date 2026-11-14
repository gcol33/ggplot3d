test_that("output formats resolve from the registry with matching temporary paths", {
  local_mocked_bindings(motion_require_3d = function(package) NULL)
  motion_outputs_3d$testformat <- motion_outputs_3d$pdf
  on.exit(rm("testformat", envir = motion_outputs_3d), add = TRUE)
  for (format in ls(motion_outputs_3d)) {
    output <- motion_output_3d(NULL, format)
    expect_identical(tools::file_ext(output$file), format)
    expect_identical(normalizePath(dirname(output$file)), normalizePath(tempdir()))
    expect_identical(output$kind, motion_outputs_3d[[format]]$kind)
    expect_identical(output$device, motion_outputs_3d[[format]]$device)
    expect_identical(motion_output_3d(output$file, toupper(format)), output)
    expect_false(file.exists(output$file))
  }
})

test_that("invalid formats and conflicting extensions fail before encoding", {
  local_mocked_bindings(motion_require_3d = function(package) stop("encoder checked"))
  for (format in list(character(), NA_character_, c("gif", "mp4"), 1, TRUE, ""))
    expect_error(motion_output_3d(NULL, format), "format must be a single output format or NULL", fixed = TRUE)
  expect_error(motion_output_3d(NULL, "unknown"), "format must be one of", fixed = TRUE)
  expect_error(motion_output_3d("movie.mp4", "gif"), "file extension must agree with format", fixed = TRUE)
  expect_error(motion_output_3d("frames.png", "apng"), "file extension must agree with format", fixed = TRUE)
  p <- ggplot2::ggplot(mtcars, ggplot2::aes(wt, mpg, z = hp)) + geom_point_3d() + coord_3d()
  expect_error(animate_3d(p, file = "movie.mp4", format = "gif"),
    "file extension must agree with format", fixed = TRUE)
})

for (format in ls(motion_outputs_3d)) {
  test_that(paste("format alone writes", format, "output"), {
    output <- motion_outputs_3d[[format]]
    for (package in c(output$packages, "ragg")) skip_if_not_installed(package)
    p <- ggplot2::ggplot(mtcars[1:3, ], ggplot2::aes(wt, mpg, z = hp)) + geom_point_3d() + coord_3d()
    movie <- animate_3d(p, frames = 2, format = format,
      width = 160, height = 160, progress = FALSE)
    on.exit(unlink(c(movie$file, movie$frames)), add = TRUE)
    expect_s3_class(movie, "ggplot3d_animation")
    expect_identical(tools::file_ext(movie$file), format)
    expect_identical(normalizePath(dirname(movie$file)), normalizePath(tempdir()))
    expect_identical(movie$frame_count, 2L)
    paths <- if (output$kind == "sequence") movie$frames else movie$file
    expect_length(movie$frames, if (output$kind == "sequence") 2L else 0L)
    expect_true(all(file.exists(paths)))
    expect_true(all(file.size(paths) > 0))
    switch(format,
      gif = expect_identical(readChar(movie$file, 6L), "GIF89a"),
      apng = {
        chunks <- png_chunks_3d(movie$file)
        types <- vapply(chunks, `[[`, character(1), "type")
        expect_identical(sum(types == "fcTL"), 2L)
      },
      png = {
        expect_identical(readBin(paths[1L], "raw", 8L), as.raw(c(137, 80, 78, 71, 13, 10, 26, 10)))
        expect_false(file.exists(movie$file))
      },
      mp4 = , webm = {
        info <- av::av_media_info(movie$file)
        expect_equal(info$video$width, 160)
        expect_equal(info$video$height, 160)
      },
      pdf = expect_identical(readChar(movie$file, 4L), "%PDF"),
      svg = expect_match(readLines(paths[1L], warn = FALSE)[1L], "<?xml", fixed = TRUE),
      glb = expect_identical(readChar(movie$file, 4L), "glTF"),
      gltf = expect_match(paste(readLines(movie$file, warn = FALSE), collapse = ""), '"animations"', fixed = TRUE)
    )
  })
}

test_that("NULL file and format retain the PNG frames", {
  p <- ggplot2::ggplot(mtcars[1:3, ], ggplot2::aes(wt, mpg, z = hp)) + geom_point_3d() + coord_3d()
  implicit <- animate_3d(p, frames = 2, width = 160, height = 160, progress = FALSE)
  on.exit(unlink(dirname(implicit$frames[1L]), recursive = TRUE), add = TRUE)
  explicit <- animate_3d(p, frames = 2, file = NULL, format = NULL,
    width = 160, height = 160, progress = FALSE)
  on.exit(unlink(dirname(explicit$frames[1L]), recursive = TRUE), add = TRUE)
  expect_null(explicit$file)
  expect_length(explicit$frames, 2L)
  expect_identical(explicit$camera, implicit$camera)
  expect_identical(unname(tools::md5sum(explicit$frames)), unname(tools::md5sum(implicit$frames)))
})
