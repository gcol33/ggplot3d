#include "raster/render.h"

// [[Rcpp::export(rng = false)]]
Rcpp::List raster_prepare_cpp(Rcpp::List packet, Rcpp::List camera, Rcpp::List viewport) {
  return ggplot3d::raster::prepare(packet, camera, viewport);
}

// [[Rcpp::export(rng = false)]]
Rcpp::List raster_draw_cpp(SEXP scene, Rcpp::List colours, Rcpp::List dashes,
                           int width, int height, int antialias, bool id_pass,
                           int threads, bool buffers, bool linear, Rcpp::List traced) {
  return ggplot3d::raster::draw(scene, colours, dashes, width, height, antialias, id_pass,
                                threads, buffers, linear, traced);
}

// [[Rcpp::export(rng = false)]]
Rcpp::List raster_canvas_cpp(SEXP scene, Rcpp::List colours, Rcpp::List dashes,
                             int width, int height, int antialias) {
  return ggplot3d::raster::canvas_info(scene, colours, dashes, width, height, antialias);
}

// [[Rcpp::export(rng = false)]]
Rcpp::List raster_occlusion_cpp(SEXP scene, Rcpp::List colours, Rcpp::List dashes,
                                Rcpp::NumericVector extent, int width, int height, int antialias,
                                Rcpp::NumericMatrix depth) {
  return ggplot3d::raster::occlusion(scene, colours, dashes, extent, width, height, antialias, depth);
}
