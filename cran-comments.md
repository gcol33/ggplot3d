## New submission

This is the first CRAN release of ggplot3d.

## Test environments

* local Windows 11, R 4.6.1
* win-builder, R-devel
* GitHub Actions: ubuntu-latest (R-devel, release, oldrel-1), windows-latest
  (release), macos-latest (release)

## R CMD check results

0 errors | 0 warnings | 2 notes

* New submission.
* Installed size: the compiled scene engine includes the bundled Manifold
  (Apache License 2.0) and Clipper2 (Boost Software License 1.0) geometry
  libraries, which make up most of the `libs` directory. Their authors are
  listed in `Authors@R` and their notices in `inst/COPYRIGHTS`.

## Notes for the reviewer

* Examples, tests and vignettes write only to `tempdir()`.
* The scene compiler and raster backend use at most 2 threads when
  `_R_CHECK_LIMIT_CORES_` is set (option `ggplot3d.threads`).
