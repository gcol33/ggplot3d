library(testthat)
library(ggplot3d)

# Under the CRAN profile only the smoke files run (testthat/helper-tiers.R).
source(file.path("testthat", "helper-tiers.R"), local = TRUE)
test_check("ggplot3d", filter = cran_smoke_filter())
