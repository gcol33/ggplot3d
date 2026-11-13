# Contributing to ggplot3d

Contributions to code, examples and documentation are welcome. Participation follows the [Code of Conduct](CODE_OF_CONDUCT.md).

## Installation

Clone the source and install the development dependencies:

```sh
git clone https://github.com/gcol33/ggplot3d.git
cd ggplot3d
```

```r
install.packages(c("devtools", "roxygen2", "pkgdown"))
devtools::install_deps(dependencies = TRUE)
devtools::load_all()
```

Source installation needs a C++17 compiler. The [README](README.md#installation) lists build tools by operating system. Use `devtools::install()` to install a local build.

## Testing

```r
devtools::test()
devtools::check(args = c("--as-cran", "--no-manual"))
```

Use small deterministic examples that exercise the changed behaviour. Visibility changes need cases with overlapping geometry and checks of both output backends.

## Documentation

Edit function help in the roxygen comments in `R/`. Edit the README in `README.Rmd` and articles in `vignettes/`.

```r
devtools::document()
devtools::run_examples()
devtools::build_readme()
devtools::build_vignettes()
pkgdown::build_site()
```

Inspect rendered figures at phone width. Keep the smallest figure text at least 11 pixels high at a 390-pixel display width.

## Project organization

* `R/` defines scales, layers, camera controls and scene construction.
* `src/` implements projection, visibility and raster rendering.
* `tests/testthat/` contains automated checks.
* `man/` contains generated help; `man/figures/` contains README figures.
* `vignettes/` contains tutorials and articles.

## Contributing workflow

Discuss substantial API changes in an issue. Make focused changes, update relevant examples and `NEWS.md`, and run the checks above. A pull request should describe the resulting behaviour and the validation performed.

## Style

Keep shared geometry and visibility logic in a single implementation. Validate inputs at the public boundary. Comments explain behaviour and domain rationale. Documentation uses plain sentences and runnable examples.

## Reporting bugs

Include a small reproducible plot, expected and observed output, the backend and device used, and `sessionInfo()`. Camera angles and output dimensions help reproduce visibility and framing problems.

Contributions use the package's MIT licence.
