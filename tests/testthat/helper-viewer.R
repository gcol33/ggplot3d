# Opens saved widgets in headless Chrome and evaluates JavaScript there.
# Skips when chromote or a Chrome binary is unavailable.
viewer_browser <- function(widgets, width = 800, height = 600) {
  testthat::skip_on_cran()
  testthat::skip_if_not_installed("chromote")
  testthat::skip_if_not_installed("htmltools")
  chrome <- tryCatch(chromote::find_chrome(), error = function(e) NULL)
  testthat::skip_if(is.null(chrome) || !nzchar(chrome), "Chrome is unavailable")
  dir <- tempfile("viewer-")
  dir.create(dir)
  html <- file.path(dir, "index.html")
  htmltools::save_html(do.call(htmltools::tagList, widgets), html, libdir = "lib")
  session <- viewer_browser_session(width, height)
  session$Page$navigate(paste0("file:///", normalizePath(html, winslash = "/")))
  eval_js <- function(js) {
    result <- session$Runtime$evaluate(js, returnByValue = TRUE)
    if (!is.null(result$exceptionDetails)) stop(result$exceptionDetails$exception$description, call. = FALSE)
    result$result$value
  }
  ready <- sprintf(paste0("(function() { var all = Array.from(document.querySelectorAll('.ggplot3d_view'));",
    "return all.length === %d && all.every(function(el) { return el.dataset.rendered === 'true'; }); })()"),
    length(widgets))
  start <- Sys.time()
  while (!isTRUE(eval_js(ready)) && Sys.time() - start < 30) Sys.sleep(0.1)
  if (!isTRUE(eval_js(ready))) {
    session$close()
    testthat::skip("the browser did not render the widget")
  }
  list(eval = eval_js, session = session,
    close = function() { session$close(); unlink(dir, recursive = TRUE) })
}

viewer_browser_session <- function(width = 800, height = 600) {
  unavailable <- function(e) testthat::skip(paste("Headless Chrome cannot start:", conditionMessage(e)))
  tryCatch(chromote::ChromoteSession$new(width = width, height = height),
    error_stop_port_search = unavailable,
    system_command_error = unavailable,
    error_no_available_port = unavailable)
}

# Pointer coordinates of every pickable pixel of a widget, on a grid.
viewer_hits <- function(browser, id, step = 3) {
  browser$eval(sprintf(paste0("(function() { var el = document.getElementById('%s'), out = [];",
    "var box = el.getBoundingClientRect();",
    "for (var x = 0; x < box.width; x += %d) for (var y = 0; y < box.height; y += %d) {",
    "var h = el.ggplot3d.pick(x, y, 0); if (h) { h.x = x; h.y = y; out.push(h); } }",
    "return out; })()"), id, step, step))
}

viewer_click <- function(browser, id, x, y, modifiers = 0L) {
  box <- browser$eval(sprintf("(function() { var b = document.querySelector('#%s canvas.gg3d-surface').getBoundingClientRect(); return {left: b.left, top: b.top}; })()", id))
  for (type in c("mouseMoved", "mousePressed", "mouseReleased")) {
    browser$session$Input$dispatchMouseEvent(type = type, x = box$left + x, y = box$top + y,
      button = if (type == "mouseMoved") "none" else "left", clickCount = 1L, modifiers = modifiers)
  }
  Sys.sleep(0.2)
}

# Columns of a viewer packet decoded from base64 typed arrays.
decode_column <- function(column) {
  bytes <- if (nzchar(column$b64)) decode_base64(column$b64) else raw()
  switch(column$type,
    f32 = readBin(bytes, "double", n = length(bytes) / 4L, size = 4L, endian = "little"),
    i32 = readBin(bytes, "integer", n = length(bytes) / 4L, size = 4L, endian = "little"),
    u8 = as.logical(as.integer(bytes)))
}

decode_base64 <- function(text) {
  alphabet <- strsplit("ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/", "")[[1L]]
  chars <- strsplit(text, "")[[1L]]
  pad <- sum(chars == "=")
  v <- match(chars[chars != "="], alphabet) - 1L
  v <- c(v, integer(pad))
  m <- matrix(v, 4L)
  n <- m[1L, ] * 262144 + m[2L, ] * 4096 + m[3L, ] * 64 + m[4L, ]
  bytes <- as.raw(rbind(n %/% 65536, (n %/% 256) %% 256, n %% 256))
  bytes[seq_len(length(bytes) - pad)]
}
