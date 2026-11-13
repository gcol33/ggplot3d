# Candidate bonds use a spatial hash with cells as wide as the longest
# permitted bond, so only the 27 neighbouring cells need examination.
molecule_bonds_3d <- function(atoms, scale) {
  xyz <- as.matrix(atoms[c("x", "y", "z")])
  radius <- atoms$covalent_radius
  if (!nrow(atoms) || !any(radius > 0)) return(data.frame(from = integer(), to = integer()))
  width <- 2 * max(radius) * scale
  cell <- floor(xyz / width)
  key <- function(x) paste(x[, 1L], x[, 2L], x[, 3L], sep = ":")
  cells <- list2env(split(seq_len(nrow(atoms)), key(cell)), hash = TRUE, parent = emptyenv())
  neighbours <- as.matrix(expand.grid(-1:1, -1:1, -1:1))
  bonds <- vector("list", nrow(atoms))
  for (i in seq_len(nrow(atoms))) {
    keys <- key(sweep(neighbours, 2L, cell[i, ], "+"))
    j <- unlist(mget(keys, envir = cells, ifnotfound = rep(list(NULL), length(keys)), inherits = FALSE), use.names = FALSE)
    j <- j[j > i]
    if (!length(j)) next
    distance <- sqrt(rowSums(sweep(xyz[j, , drop = FALSE], 2L, xyz[i, ], "-")^2))
    j <- j[distance > 0 & distance <= scale * (radius[i] + radius[j])]
    if (length(j)) bonds[[i]] <- data.frame(from = atoms$eleno[i], to = atoms$eleno[j])
  }
  out <- do.call(rbind, bonds)
  if (is.null(out)) data.frame(from = integer(), to = integer()) else out
}

#' Fortify molecular atoms and bonds
#'
#' Reads bio3d atom records and joins element metadata by the `elesy` symbol.
#' The returned atom table retains its serial numbers and has an attached
#' edge table for [get_edges_3d()] and [geom_edge_3d()]. Atom serial numbers
#' identify bond endpoints independently of atom-table order.
#'
#' Bio3d's reader retains atom records. Supply chemical connectivity as
#' `bonds`, or estimate candidate bonds by distances up to `bond_scale` times
#' the sum of the two covalent radii. This distance rule provides geometry for
#' a ball-and-stick view; supplied connectivity determines chemical bonds.
#' Element radii and colours come from bio3d's periodic table.
#' @param model A bio3d pdb object with finite coordinates and unique eleno
#'   serial numbers. Element symbols must be present in elesy.
#' @param data Unused, for compatibility with fortify().
#' @param bonds Optional data frame with from and to atom serial numbers.
#'   Supply an empty table to draw atoms alone.
#' @param bond_scale Positive covalent-radius distance multiplier.
#' @param ... Unused.
#' @return An atom data frame with element, covalent_radius and colour columns
#'   and an edges attribute containing endpoint coordinates.
#' @examples
#' if (requireNamespace("bio3d", quietly = TRUE)) {
#'   pdb <- bio3d::read.pdb(system.file("examples/1hel.pdb", package = "bio3d"), verbose = FALSE)
#'   atoms <- fortify(pdb)
#'   head(atoms[c("eleno", "element", "covalent_radius")])
#' }
#' @export
fortify.pdb <- function(model, data = NULL, bonds = NULL, bond_scale = 1.2, ...) {
  rlang::check_installed("bio3d")
  scene_assert(is.numeric(bond_scale) && length(bond_scale) == 1L && is.finite(bond_scale) && bond_scale > 0,
    "bond_scale must be positive")
  atoms <- model$atom
  scene_assert(is.data.frame(atoms) && all(c("x", "y", "z", "eleno", "elesy") %in% names(atoms)), "pdb requires atom coordinates, eleno and elesy")
  scene_assert(!anyNA(atoms$eleno) && !anyDuplicated(atoms$eleno) && all(is.finite(as.matrix(atoms[c("x", "y", "z")]))),
    "Atom serial numbers must be unique and coordinates finite")
  env <- new.env(parent = emptyenv())
  utils::data("elements", package = "bio3d", envir = env)
  elements <- env$elements
  element <- match(toupper(trimws(atoms$elesy)), toupper(as.character(elements$symb)))
  scene_assert(!anyNA(element), "Atom element symbols in elesy must match bio3d's periodic table")
  atoms$element <- as.character(elements$symb[element])
  atoms$covalent_radius <- elements$rcov[element]
  atoms$colour <- grDevices::rgb(elements$red[element], elements$green[element], elements$blue[element])
  if (is.null(bonds)) bonds <- molecule_bonds_3d(atoms, bond_scale)
  scene_assert(is.data.frame(bonds) && all(c("from", "to") %in% names(bonds)), "bonds requires from and to atom serial numbers")
  from <- match(bonds$from, atoms$eleno);to <- match(bonds$to, atoms$eleno)
  scene_assert(!anyNA(from) && !anyNA(to) && all(from != to), "Bond endpoints must identify two distinct atom serial numbers")
  key <- paste(pmin(from, to), pmax(from, to), sep = ":")
  keep <- !duplicated(key)
  bonds <- bonds[keep, , drop = FALSE];from <- from[keep];to <- to[keep]
  bonds[c("x", "y", "z")] <- atoms[from, c("x", "y", "z")]
  bonds[c("xend", "yend", "zend")] <- atoms[to, c("x", "y", "z")]
  rownames(bonds) <- NULL
  attr(atoms, "edges") <- bonds
  atoms
}
