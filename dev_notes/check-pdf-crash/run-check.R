Sys.setenv(PATH = paste("C:/Program Files/Pandoc", Sys.getenv("PATH"), sep = ";"),
  RSTUDIO_PANDOC = "C:/Program Files/Pandoc", `_R_CHECK_FORCE_SUGGESTS_` = "false")
args <- commandArgs(TRUE)
path <- if (length(args)) args[1L] else "."
check <- rcmdcheck::rcmdcheck(path = path, args = c("--as-cran", "--no-manual"),
  check_dir = "x.Rcheck", error_on = "never")
saveRDS(check, "dev_notes/check-pdf-crash/check-result.rds")
cat("ERRORS", length(check$errors), "WARNINGS", length(check$warnings), "NOTES", length(check$notes), "\n")
if (length(check$errors) || length(check$warnings)) quit(status = 1L)
