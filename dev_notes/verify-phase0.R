options(warn = 1)
Sys.setenv(LC_ALL = "C", LC_CTYPE = "C", LANG = "C")
Rcpp::compileAttributes()
devtools::document()
devtools::load_all()
devtools::test(stop_on_failure = TRUE)
result <- devtools::check(document = FALSE, args = "--no-manual")
saveRDS(result, "dev_notes/phase0-check.rds")
cat("\nCHECK RESULT\n")
cat(length(result$errors), "errors |", length(result$warnings), "warnings |",
    length(result$notes), "notes\n")
for (kind in c("errors", "warnings", "notes")) {
  if (length(result[[kind]])) {
    cat("\n", kind, ":\n", sep = "")
    cat(paste(result[[kind]], collapse = "\n"), "\n")
  }
}
