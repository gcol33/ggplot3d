#include "raster/render.h"
#include "shade/occlusion.h"

// [[Rcpp::export(rng = false)]]
void raster_ambient_cpp(SEXP scene, SEXP context) {
  Rcpp::XPtr<ggplot3d::raster::Scene> prepared(scene);
  Rcpp::XPtr<ggplot3d::ambient::Context> ao(context);
  for (const auto& item:prepared->items) if (item.kind==1) {
    Rcpp::checkUserInterrupt();
    for (ggplot3d::Index j=0;j<item.count;++j) {
      auto& v=prepared->shading[item.offset+j];
      auto normal=v.normal;
      if (!std::isfinite(normal[0]+normal[1]+normal[2]) || ggplot3d::engine::dot(normal,normal)==0)
        normal=prepared->normals[item.row];
      v.accessibility[0]=ao->visibility(v.position,normal);
      for (auto& x:normal) x=-x;
      v.accessibility[1]=ao->visibility(v.position,normal);
    }
  }
}

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
