# Headless Chrome removes its temporary profile only when it exits cleanly, so
# the shared browser is closed when the test run ends.
withr::defer({
  if (requireNamespace("chromote", quietly = TRUE) && chromote::has_default_chromote_object()) {
    try(chromote::default_chromote_object()$close(), silent = TRUE)
  }
}, testthat::teardown_env())
