utils::globalVariables(c("raymarch_ops_cpp", "raymarch_eval_cpp", "raymarch_trace_cpp"))

# Scalar arithmetic is recorded while the registered SDF rule is evaluated.
# Coordinates transformed by a child rule become ordinary tape instructions.
sdf_compile_3d <- function(sdf) {
  tape <- new.env(parent = emptyenv())
  tape$code <- list(); tape$cache <- new.env(parent = emptyenv())
  tape$ops <- raymarch_ops_cpp()
  value <- function(op, a = 0L, b = 0L, constant = 0) {
    opcode <- match(op, tape$ops)
    if (is.na(opcode)) stop("No compiled implementation for arithmetic operation: ", op, call. = FALSE)
    key <- paste(opcode, a, b, sprintf("%.17g", constant), sep = ":")
    index <- tape$cache[[key]]
    if (is.null(index)) {
      index <- length(tape$code) + 1L
      tape$code[[index]] <- c(opcode - 1L, a - 1L, b - 1L, constant)
      tape$cache[[key]] <- index
    }
    structure(list(index = index, tape = tape), class = "ggplot3d_tape")
  }
  tape$value <- value
  helpers <- new.env(parent = environment(sdf_eval_3d))
  helpers$sdf_max_3d <- function(a, b) sdf_tape_binary_3d("max", a, b)
  helpers$sdf_min_3d <- function(a, b) sdf_tape_binary_3d("min", a, b)
  for (name in c("sdf_length_3d", "sdf_shift_3d", "sdf_dot_3d", "sdf_clamp_3d", "sdf_smin_3d")) {
    f <- get(name, envir = environment(sdf_eval_3d)); environment(f) <- helpers
    helpers[[name]] <- f
  }
  visit <- function(node, p) {
    entry <- sdf_ops_3d[[node$op]]
    if (is.null(entry) || !is.function(entry$eval)) stop("Unregistered SDF operation: ", node$op, call. = FALSE)
    rule <- entry$eval
    scope <- new.env(parent = environment(rule))
    for (name in ls(helpers)) scope[[name]] <- helpers[[name]]
    environment(rule) <- scope
    tryCatch(rule(node$params, p, function(i, q = p) visit(node$children[[i]], q)),
      error = function(e) stop("Cannot compile SDF operation '", node$op, "': ", conditionMessage(e), call. = FALSE))
  }
  node <- sdf_node_3d(sdf)
  out <- visit(node, lapply(c("x", "y", "z"), value))
  if (!inherits(out, "ggplot3d_tape")) out <- value("constant", constant = out)
  list(code = do.call(rbind, tape$code), root = out$index - 1L, lipschitz = node$lipschitz)
}

sdf_tape_binary_3d <- function(op, a, b) {
  if (!inherits(a, "ggplot3d_tape") && !inherits(b, "ggplot3d_tape"))
    return(switch(op, min = min(a, b), max = max(a, b)))
  tape <- if (inherits(a, "ggplot3d_tape")) a$tape else b$tape
  scalar <- function(x) {
    if (inherits(x, "ggplot3d_tape")) return(x)
    if (!is.numeric(x) || length(x) != 1L || !is.finite(x)) stop("Tape constants must be finite numeric scalars.", call. = FALSE)
    tape$value("constant", constant = x)
  }
  a <- scalar(a); b <- scalar(b)
  tape$value(op, a$index, b$index)
}

#' @export
Ops.ggplot3d_tape <- function(e1, e2) {
  if (missing(e2)) {
    if (.Generic == "+") return(e1)
    if (.Generic == "-") return(e1$tape$value("negate", e1$index))
    stop("Unsupported compiled unary operation: ", .Generic, call. = FALSE)
  }
  sdf_tape_binary_3d(.Generic, e1, e2)
}

#' @export
Math.ggplot3d_tape <- function(x, ...) x$tape$value(.Generic, x$index)
