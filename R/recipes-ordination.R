ordination_scores_3d <- function(model, choices, scores_args) {
  scene_assert(is.list(scores_args), "scores_args must be a list")
  scene_assert(is.numeric(choices) && length(choices) == 3L && !anyDuplicated(choices) &&
    all(is.finite(choices) & choices >= 1 & choices == trunc(choices)), "choices must select three distinct positive axes")
  if (inherits(model, "prcomp")) {
    scene_assert(!is.null(model$x), "The prcomp fit must retain its scores (retx = TRUE)")
    scores <- list(sites = model$x, species = model$rotation)
  } else if (is.list(model) && !is.null(model$sites)) scores <- model
  else {
    rlang::check_installed("vegan")
    scene_assert(!any(c("x", "choices", "display", "tidy") %in% names(scores_args)), "scores_args must leave choices and display to the biplot")
    scores <- do.call(vegan::scores, c(list(x = model, choices = choices, display = c("sites", "species")), scores_args))
    if (is.matrix(scores)) scores <- list(sites = scores)
    choices <- 1:3
  }
  convert <- function(matrix, name) {
    if (is.null(matrix)) return(data.frame(x = numeric(), y = numeric(), z = numeric(), id = character()))
    matrix <- as.matrix(matrix)
    scene_assert(is.numeric(matrix) && ncol(matrix) >= max(choices) && all(is.finite(matrix)), paste(name, "must contain three finite score axes"))
    ids <- rownames(matrix) %or3d% as.character(seq_len(nrow(matrix)))
    scene_assert(!anyNA(ids) && !anyDuplicated(ids), paste(name, "score identifiers must be unique"))
    data.frame(x = matrix[, choices[1L]], y = matrix[, choices[2L]], z = matrix[, choices[3L]], id = ids)
  }
  list(sites = convert(scores$sites, "Site"), species = convert(scores$species, "Species"))
}

ordination_groups_3d <- function(sites, groups) {
  if (is.null(groups)) {sites$.biplot_group <- "Sites";return(sites)}
  if (is.data.frame(groups)) {
    scene_assert(all(c("id", "group") %in% names(groups)), "groups requires id and group columns")
    ids <- as.character(groups$id);values <- groups$group
  } else {ids <- names(groups);values <- groups}
  scene_assert(!is.null(ids) && !anyNA(ids) && !anyDuplicated(ids) && setequal(ids, sites$id),
    "groups must identify every site exactly once by its score row name")
  sites$.biplot_group <- values[match(sites$id, ids)]
  scene_assert(!anyNA(sites$.biplot_group), "Site groups must be non-missing")
  sites
}

#' Compose a three-axis ordination biplot
#'
#' Site scores become points; species or variable scores become solid-headed
#' quiver arrows from the origin. Optional group regions use the existing
#' ellipsoid or convex-hull statistic. For prcomp, the coordinates are `x`
#' and `rotation`. For vegan fits, extraction and scaling use [vegan::scores()].
#' A named list returned by that function can also be supplied directly.
#' @param model A prcomp fit, a vegan ordination fit, or a list with sites and
#'   optional species score matrices.
#' @param choices Three ordination axes.
#' @param groups Named group vector, keyed by site-score row names, or a data
#'   frame with id and group columns. Its row order is arbitrary.
#' @param region `"none"`, `"ellipsoid"` or `"hull"`.
#' @param loading_scale Explicit multiplier of variable/species scores.
#' @param point_size Site marker size in millimetres.
#' @param label_loadings Whether to label arrow tips.
#' @param level Ellipsoid coverage level.
#' @param scores_args Additional arguments to vegan's scores method.
#' @return A list of layers to add to a ggplot with coord_3d().
#' @examples
#' fit <- stats::prcomp(iris[1:4], scale. = TRUE)
#' groups <- stats::setNames(iris$Species, rownames(iris))
#' ggplot2::ggplot() + stat_biplot_3d(fit, groups = groups,
#'   region = "ellipsoid", loading_scale = 2) + coord_3d()
#' @export
stat_biplot_3d <- function(model, choices = 1:3, groups = NULL, region = c("none", "ellipsoid", "hull"),
                           loading_scale = 1, point_size = 1.5, label_loadings = TRUE, level = 0.95, scores_args = list()) {
  region <- match.arg(region)
  for (name in c("loading_scale", "point_size")) {
    value <- get(name)
    scene_assert(is.numeric(value) && length(value) == 1L && is.finite(value) && value >= 0, paste(name, "must be nonnegative"))
  }
  label_loadings <- check_flag_3d(label_loadings, "label_loadings")
  scores <- ordination_scores_3d(model, choices, scores_args)
  sites <- ordination_groups_3d(scores$sites, groups)
  mapping <- ggplot2::aes(x, y, z = z, colour = .biplot_group, fill = .biplot_group, group = .biplot_group)
  layers <- list(geom_point_3d(mapping, data = sites, size = point_size, inherit.aes = FALSE),
    ggplot2::labs(colour = "Group", fill = "Group"))
  if (region != "none") layers <- c(layers, list(if (region == "ellipsoid")
    stat_ellipsoid_3d(mapping, data = sites, type = "norm", level = level, alpha = 0.2, detail = 1, inherit.aes = FALSE)
    else stat_hull_3d(mapping, data = sites, alpha = 0.2, inherit.aes = FALSE)))
  arrows <- scores$species
  if (nrow(arrows)) {
    arrows[c("u", "v", "w")] <- arrows[c("x", "y", "z")] * loading_scale
    arrows[c("x", "y", "z")] <- 0
    layers <- c(layers, list(geom_quiver_3d(ggplot2::aes(x, y, z = z, u = u, v = v, w = w),
      data = arrows, inherit.aes = FALSE, colour = "grey25", fill = "grey25", head_length = 0.12, head_width = 0.04)))
    if (label_loadings) layers <- c(layers, list(geom_text_3d(ggplot2::aes(x = u, y = v, z = w, label = id),
      data = arrows, inherit.aes = FALSE, size = 3, vjust = -0.4)))
  }
  layers
}

utils::globalVariables(c(".biplot_group", "u", "v", "w", "id"))
