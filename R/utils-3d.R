# Row-wise cross products of two n x 3 matrices.
cross_rows_3d <- function(u, v) {
  cbind(u[, 2L] * v[, 3L] - u[, 3L] * v[, 2L], u[, 3L] * v[, 1L] - u[, 1L] * v[, 3L], u[, 1L] * v[, 2L] - u[, 2L] * v[, 1L])
}

# Rows scaled to unit length. Rows of length zero are set to `zero`: the zero
# vector by default, or NaN to mark them as undefined directions.
unit_rows_3d <- function(m, zero = 0) {
  length <- sqrt(rowSums(m^2))
  out <- m / length
  out[which(length == 0), ] <- zero
  out
}

# Rows of a data frame, sliced column by column without the checks of
# `[.data.frame`: the backends take a few style rows per draw run, thousands of
# times per panel.
slice_rows_3d <- function(df, rows) {
  out <- lapply(unclass(df), `[`, rows)
  n <- length(out[[1L]] %or3d% rows)
  attr(out, "row.names") <- if (n) c(NA_integer_, -n) else integer()
  class(out) <- "data.frame"
  out
}

# A single string from `choices`. `shown` lists the accepted values in the
# error message, which may name forms (TRUE, a mesh) handled by the caller.
check_choice_3d <- function(value, choices, name, shown = paste0('"', choices, '"')) {
  if (!is.character(value) || length(value) != 1L || is.na(value) || !value %in% choices) {
    listed <- if (length(shown) > 1L) paste(paste(shown[-length(shown)], collapse = ", "), "or", shown[length(shown)]) else shown
    stop(name, " must be one of ", listed, ".", call. = FALSE)
  }
  value
}
