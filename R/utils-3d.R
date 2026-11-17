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

# Ids 1..k of the distinct tuples formed by equal-length vectors, numbered in
# order of first appearance. Values are compared exactly.
tuple_ids_3d <- function(...) {
  id <- NULL
  for (column in list(...)) {
    k <- match(column, unique(column))
    if (is.null(id)) id <- k else {
      pair <- id * (max(k) + 1) + k
      id <- match(pair, unique(pair))
    }
  }
  id
}

# Minimum (or maximum) of x within each group of `group`, for every element.
group_extreme_3d <- function(x, group, maximum = FALSE) {
  o <- order(group, if (maximum) -x else x, method = "radix")
  first <- !duplicated(group[o])
  x[o][first][match(group, group[o][first])]
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

check_flag_3d <- function(value, name) {
  if (!is.logical(value) || length(value) != 1L || is.na(value)) stop(name, " must be TRUE or FALSE.", call. = FALSE)
  value
}

check_integer_3d <- function(value, name, min = 0, max = .Machine$integer.max) {
  if (!is.numeric(value) || length(value) != 1L || !is.finite(value) ||
      value != trunc(value) || value < min || value > max) {
    stop(name, " must be a representable integer between ", min, " and ", max, ".", call. = FALSE)
  }
  as.integer(value)
}

check_choices_3d <- function(value, choices, name) {
  if (!is.character(value) || anyNA(value)) check_choice_3d(value, choices, name)
  for (item in value) check_choice_3d(item, choices, name)
  unique(value)
}
