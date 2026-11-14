pkgload::load_all()
res <- as.data.frame(devtools::test(filter = "^grid-", reporter = "summary"))
cat("FAILED", sum(res$failed), "ERRORS", sum(res$error), "WARNINGS", sum(res$warning), "PASSED", sum(res$passed), "\n")
if (sum(res$failed) || any(res$error)) quit(status = 1L)
