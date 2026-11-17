# Component keys share ggplot2's names and the package's _3d suffixes.
layer_component_key_3d <- function(name) tolower(gsub("_", "", name, fixed = TRUE))

.layer_components_3d <- new.env(parent = emptyenv())

layer_component_registry_3d <- function() {
  if (length(.layer_components_3d)) return(.layer_components_3d)
  for (kind in c("geom", "stat", "position")) {
    registry <- list()
    class <- switch(kind, geom = "Geom", stat = "Stat", position = "Position")
    for (env in list(asNamespace("ggplot2"), topenv())) {
      for (name in ls(env, pattern = paste0("^", class))) {
        prototype <- get(name, envir = env)
        if (!inherits(prototype, class) || identical(name, class)) next
        key <- layer_component_key_3d(substring(name, nchar(class) + 1L))
        registry[[key]] <- prototype
        if (identical(env, asNamespace("ggplot2"))) registry[[paste0(key, "3d")]] <- prototype
        else if (kind == "geom" && endsWith(key, "3d")) {
          plain <- substr(key, 1L, nchar(key) - 2L)
          registry[[plain]] <- prototype
        }
      }
      if (kind == "position") {
        for (name in ls(env, pattern = "^position_")) {
          constructor <- get(name, envir = env)
          if (!is.function(constructor) || !length(formals(constructor)) ||
              any(vapply(formals(constructor), identical, logical(1), quote(expr = )))) next
          key <- layer_component_key_3d(substring(name, 10L))
          registry[[key]] <- constructor()
        }
      }
    }
    if (kind == "geom") {
      for (name in ls(.emitters_3d)) {
        key <- layer_component_key_3d(substring(name, 5L))
        registry[[key]] <- .emitters_3d[[name]]$geom
        if (endsWith(key, "3d")) registry[[substr(key, 1L, nchar(key) - 2L)]] <- registry[[key]]
      }
    }
    .layer_components_3d[[kind]] <- registry
  }
  .layer_components_3d
}

resolve_layer_component_3d <- function(value, kind) {
  class <- switch(kind, geom = "Geom", stat = "Stat", position = "Position")
  if (inherits(value, class)) return(value)
  if (is.character(value) && length(value) == 1L && !is.na(value)) {
    resolved <- layer_component_registry_3d()[[kind]][[layer_component_key_3d(value)]]
    if (!is.null(resolved)) return(resolved)
  }
  label <- if (is.character(value)) paste(value, collapse = ", ") else paste(class(value), collapse = "/")
  stop("Unsupported ", kind, ": ", label,
    ". Use a registered ", kind, " name (including _3d names) or a ", class, " prototype.", call. = FALSE)
}

layer_params_3d <- function(params, geom, stat, default_stat = NULL) {
  names(params) <- ggplot2::standardise_aes_names(names(params))
  duplicate <- unique(names(params)[duplicated(names(params))])
  if (length(duplicate)) stop("Supply each layer argument once, including aesthetic aliases: ",
    paste(duplicate, collapse = ", "), ".", call. = FALSE)
  for (name in intersect(names(params), c("na.rm", "shade", "wireframe", "raster", "cast_shadow", "parse", "check_overlap"))) {
    params[[name]] <- check_flag_3d(params[[name]], name)
  }
  if ("max_edge" %in% names(params)) params$max_edge <- check_positive_3d(params$max_edge, "max_edge", infinite = TRUE)
  for (name in intersect(names(params), c("occlusion", "hidden", "render"))) {
    choices <- switch(name, occlusion = c("depth", "overlay"), hidden = c("hide", "dashed", "show"), render = c("marker", "splat"))
    params[[name]] <- check_choice_3d(params[[name]], choices, name)
  }
  known <- unique(c(geom$aesthetics(), geom$parameters(TRUE), geom$extra_params, stat$parameters(TRUE)))
  unused <- setdiff(names(params), known)
  unknown <- setdiff(unused, if (!is.null(default_stat)) default_stat$parameters(TRUE))
  if (length(unknown)) stop("Unknown 3D layer arguments: ", paste(unknown, collapse = ", "), call. = FALSE)
  params[setdiff(names(params), unused)]
}
