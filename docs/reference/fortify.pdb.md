# Fortify molecular atoms and bonds

Reads bio3d atom records and joins element metadata by the `elesy`
symbol. The returned atom table retains its serial numbers and has an
attached edge table for
[`get_edges_3d()`](https://gillescolling.com/ggplot3d/reference/get_edges_3d.md)
and
[`geom_edge_3d()`](https://gillescolling.com/ggplot3d/reference/geom_edge_3d.md).
Atom serial numbers identify bond endpoints independently of atom-table
order.

## Usage

``` r
# S3 method for class 'pdb'
fortify(model, data = NULL, bonds = NULL, bond_scale = 1.2, ...)
```

## Arguments

- model:

  A bio3d pdb object with finite coordinates and unique eleno serial
  numbers. Element symbols must be present in elesy.

- data:

  Unused, for compatibility with fortify().

- bonds:

  Optional data frame with from and to atom serial numbers. Supply an
  empty table to draw atoms alone.

- bond_scale:

  Positive covalent-radius distance multiplier.

- ...:

  Unused.

## Value

An atom data frame with element, covalent_radius and colour columns and
an edges attribute containing endpoint coordinates.

## Details

Bio3d's reader retains atom records. Supply chemical connectivity as
`bonds`, or estimate candidate bonds by distances up to `bond_scale`
times the sum of the two covalent radii. This distance rule provides
geometry for a ball-and-stick view; supplied connectivity determines
chemical bonds. Element radii and colours come from bio3d's periodic
table.

## Examples

``` r
if (requireNamespace("bio3d", quietly = TRUE)) {
  pdb <- bio3d::read.pdb(system.file("examples/1hel.pdb", package = "bio3d"), verbose = FALSE)
  atoms <- fortify(pdb)
  head(atoms[c("eleno", "element", "covalent_radius")])
}
```
