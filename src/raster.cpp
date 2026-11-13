#include "raster/render.h"

// [[Rcpp::export(rng = false)]]
Rcpp::List raster_prepare_cpp(Rcpp::List packet, Rcpp::List camera, Rcpp::List viewport) {
  return ggplot3d::raster::prepare(packet, camera, viewport);
}

// [[Rcpp::export(rng = false)]]
Rcpp::List raster_draw_cpp(SEXP scene, Rcpp::List colours, Rcpp::List dashes,
                           int width, int height, int antialias, bool id_pass) {
  return ggplot3d::raster::draw(scene, colours, dashes, width, height, antialias, id_pass);
}
