# Test tiers: which files and blocks run where.
#
#   Everywhere   blocks without a gate.
#   Not on CRAN  blocks whose first line is skip_on_cran(): renders, oracle
#                sweeps, backend parity. devtools::test() and CI set
#                NOT_CRAN=true and run them.
#
# Under the CRAN profile (NOT_CRAN unset) tests/testthat.R also restricts the
# run to the smoke files below, one per subsystem: sourcing a file and setting
# up its blocks costs about as much as the CRAN-run blocks of a typical file,
# so the number of files is what the check time reads. A smoke file is one
# whose ungated part covers its subsystem cheaply (measured in CRAN mode:
# expectations run per second). GGPLOT3D_CRAN_BUDGET=off runs every file under
# the CRAN profile, which reads the ungated blocks of the whole suite.
#
# Run profiles:
#   devtools::test()                                         # every file, every block
#   Sys.setenv(NOT_CRAN = "false"); devtools::test()         # ungated blocks, every file
#   R CMD check without NOT_CRAN (CRAN, win-builder)         # ungated blocks, smoke files

CRAN_SMOKE_FILES <- c(
  "engine-homogeneous", "scene-contract", "integration-scenes", "layers-glyph",
  "stats-streamline", "grid-clipping", "raster-traced", "referee-oracle",
  "motion-format", "raymarch-sdf", "export-print", "mesh-processing"
)

# TRUE under the CRAN profile, unless GGPLOT3D_CRAN_BUDGET=off opens it.
cran_budget_applies <- function() {
  !identical(Sys.getenv("NOT_CRAN"), "true") &&
    !identical(tolower(Sys.getenv("GGPLOT3D_CRAN_BUDGET")), "off")
}

# test_check()'s file filter: the smoke set under the CRAN budget, else every file.
cran_smoke_filter <- function() {
  if (!cran_budget_applies()) return(NULL)
  paste0("^(", paste(CRAN_SMOKE_FILES, collapse = "|"), ")$")
}
