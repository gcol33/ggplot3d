interactive_aesthetics_3d <- c("tooltip", "data_id")

# Shared crosstalk data whose rows the viewer selects and filters.
view_shared_data_3d <- function(data) {
  if (is.null(data)) return(NULL)
  motion_require_3d("crosstalk")
  scene_assert(crosstalk::is.SharedData(data), "data must be a crosstalk SharedData object")
  data
}

# Interaction tables of every layer: for each panel, the source rows its
# primitives carry, with their tooltip text and selection key. A layer whose
# rows keep their input index (.source, or .origin for derived geometry)
# reads tooltips from its input data; other layers index rows of the panel's
# computed data, as the emitters do.
view_layers_3d <- function(built, shared, tooltip) {
  labels <- ggplot2::get_labs(built)
  origin <- if (is.null(shared)) NULL else as.data.frame(shared$origData())
  linked <- logical(length(built$data))
  tables <- lapply(seq_along(built$data), function(i) {
    d <- built$data[[i]]
    layer <- built$plot$layers[[i]]
    rows <- d$.origin %or3d% d$.source
    input <- if (is.null(rows)) NULL else layer$layer_data(built$plot$data)
    keyed <- !is.null(origin) && !is.null(input) && view_same_data_3d(input, origin)
    linked[i] <<- keyed
    text <- if (!is.null(d$tooltip)) as.character(d$tooltip)
      else if (tooltip) view_default_tooltip_3d(layer$computed_mapping, labels, d, input, rows, built)
    key <- if (keyed) as.character(shared$key())[rows]
      else if (!is.null(d$data_id)) as.character(d$data_id)
      else paste0(i, ":", if (is.null(rows)) view_panel_rows_3d(d$PANEL) else rows)
    list(rows = rows, text = text, key = key, panel = d$PANEL, crosstalk = keyed)
  })
  scene_assert(is.null(shared) || any(linked), "No layer of the plot uses the data of the SharedData object")
  tables
}

view_panel_rows_3d <- function(panel) {
  out <- integer(length(panel))
  for (p in unique(panel)) out[panel == p] <- seq_len(sum(panel == p))
  out
}

view_same_data_3d <- function(input, origin) {
  nrow(input) == nrow(origin) && all(names(origin) %in% names(input)) &&
    isTRUE(all.equal(as.list(input[names(origin)]), as.list(origin), check.attributes = FALSE))
}

# The interaction entries of one panel, one per layer with primitives there.
view_panel_layers_3d <- function(tables, panel_id) {
  out <- lapply(seq_along(tables), function(i) {
    t <- tables[[i]]
    here <- which(as.integer(t$panel) == panel_id)
    if (!length(here)) return(NULL)
    rows <- if (is.null(t$rows)) seq_along(here) else as.integer(t$rows[here])
    keep <- here[!duplicated(rows)]
    rows <- rows[!duplicated(rows)]
    list(layer = i, rows = I(rows), key = I(t$key[keep]),
      tooltip = if (is.null(t$text)) NULL else I(t$text[keep]), crosstalk = t$crosstalk)
  })
  Filter(Negate(is.null), out)
}

# Default tooltips list each mapped aesthetic with its label, as ggplotly
# does: raw input values where the layer keeps its input rows, computed
# values (after_stat()) and data-scale positions otherwise.
view_default_tooltip_3d <- function(mapping, labels, d, input, rows, built) {
  names <- setdiff(names(mapping), c("group", interactive_aesthetics_3d))
  names <- union(intersect(c("x", "y", "z"), union(names, names(d))), names)
  if (!length(names) || !nrow(d)) return(NULL)
  lines <- lapply(names, function(aesthetic) {
    value <- if (is.null(mapping[[aesthetic]])) view_position_values_3d(d[[aesthetic]], aesthetic, built)
      else view_aes_value_3d(mapping[[aesthetic]], aesthetic, d, input, rows, built)
    if (is.null(value)) return(NULL)
    label <- labels[[aesthetic]]
    label <- if (is.null(label) || !length(label)) aesthetic else view_labels_3d(list(label))
    paste0(label, ": ", view_format_values_3d(value))
  })
  lines <- Filter(Negate(is.null), lines)
  if (!length(lines)) return(NULL)
  do.call(paste, c(lines, sep = "\n"))
}

view_aes_value_3d <- function(quo, aesthetic, d, input, rows, built) {
  quo <- rlang::as_quosure(quo, env = emptyenv())
  expr <- rlang::quo_get_expr(quo)
  stage <- view_call_name_3d(expr)
  if (stage %in% c("after_stat", "stat")) return(view_eval_3d(rlang::quo_set_expr(quo, expr[[2L]]), d, nrow(d)))
  if (stage == "after_scale") return(NULL)
  if (stage == "stage") {
    args <- as.list(expr)[-1L]
    start <- args$start %or3d% if (length(args) && (is.null(names(args)) || !nzchar(names(args)[1L]))) args[[1L]]
    if (is.null(start)) return(NULL)
    quo <- rlang::quo_set_expr(quo, start)
  }
  if (!is.null(input)) return(view_eval_3d(quo, input, nrow(input))[rows])
  value <- d[[aesthetic]]
  if (is.null(value) || !aesthetic %in% c("x", "y", z_aesthetics)) return(NULL)
  view_position_values_3d(value, aesthetic, built)
}

view_call_name_3d <- function(expr) {
  if (!is.call(expr)) return("")
  head <- expr[[1L]]
  if (is.call(head) && identical(head[[1L]], as.name("::"))) head <- head[[3L]]
  if (is.symbol(head)) as.character(head) else ""
}

view_eval_3d <- function(quo, data, n) rep_len(rlang::eval_tidy(quo, data), n)

# Positions in computed data are on the transformed scale.
view_position_values_3d <- function(value, aesthetic, built) {
  scale <- built$plot$scales$get_scales(aesthetic)
  if (is.null(scale) || scale$is_discrete() || !is.numeric(value)) return(value)
  scale$get_transformation()$inverse(value)
}

view_format_values_3d <- function(value) {
  if (is.numeric(value) && !inherits(value, c("Date", "POSIXt", "difftime", "hms"))) {
    out <- as.character(signif(as.numeric(value), 4L))
  } else out <- as.character(value)
  out[is.na(value)] <- "NA"
  out
}
