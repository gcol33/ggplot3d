test_that("viewer startup skips browser launch and debugging port failures", {
  skip_if_not_installed("chromote")
  failures <- c(error_stop_port_search = "Chrome debugging port not open after 10 seconds.",
                system_command_error = "Chrome executable cannot be launched",
                error_no_available_port = "Cannot find an available port. Please try again.")
  for (kind in names(failures)) {
    local_mocked_bindings(ChromoteSession = list(new = function(...) {
      rlang::abort(failures[[kind]], class = kind)
    }), .package = "chromote")
    condition <- tryCatch(viewer_browser_session(), skip = identity)
    expect_s3_class(condition, "skip")
    expect_match(conditionMessage(condition), "Headless Chrome cannot start:", fixed = TRUE)
    expect_match(conditionMessage(condition), failures[[kind]], fixed = TRUE)
  }
})

test_that("viewer startup returns working sessions and propagates other errors", {
  skip_if_not_installed("chromote")
  session <- new.env(parent = emptyenv())
  local_mocked_bindings(ChromoteSession = list(new = function(width, height) {
    session$size <- c(width, height)
    session
  }), .package = "chromote")
  expect_identical(viewer_browser_session(640, 360), session)
  expect_equal(session$size, c(640, 360))
  local_mocked_bindings(ChromoteSession = list(new = function(...) {
    stop("Unexpected protocol error")
  }), .package = "chromote")
  expect_error(viewer_browser_session(), "Unexpected protocol error")
})
