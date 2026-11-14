pkgload::load_all()
res <- as.data.frame(devtools::test(reporter = "summary"))
cat("FAILED", sum(res$failed), "ERRORS", sum(res$error), "WARNINGS", sum(res$warning),
  "PASSED", sum(res$passed), "SKIPPED", sum(res$skipped), "\n")
if (sum(res$failed) || any(res$error)) quit(status = 1L)
