#include "raster/render.h"
#include "shade/occlusion.h"

// [[Rcpp::export(rng = false)]]
Rcpp::NumericMatrix splat_covariance_cpp(Rcpp::NumericMatrix scales,Rcpp::NumericMatrix quaternion,Rcpp::NumericVector stretch) {
  ggplot3d::engine::require(scales.ncol()==3 && quaternion.ncol()==4 && scales.nrow()==quaternion.nrow() && stretch.size()==3,"Invalid Gaussian scales or quaternion");
  Rcpp::NumericMatrix out(scales.nrow(),6);
  for(int i=0;i<scales.nrow();++i){auto c=ggplot3d::gaussian::covariance({scales(i,0),scales(i,1),scales(i,2)},
    {quaternion(i,0),quaternion(i,1),quaternion(i,2),quaternion(i,3)},{stretch[0],stretch[1],stretch[2]});for(int k=0;k<6;++k)out(i,k)=c[k];}
  return out;
}

// [[Rcpp::export(rng = false)]]
Rcpp::List splat_resolve_cpp(Rcpp::List input,Rcpp::List camera,Rcpp::List viewport) {
  ggplot3d::engine::Projection p(ggplot3d::engine::read_camera(camera),ggplot3d::Viewport{Rcpp::as<double>(viewport["width_mm"]),Rcpp::as<double>(viewport["height_mm"]),Rcpp::as<double>(viewport["dpi"])});
  auto gaussians=ggplot3d::gaussian::read(input,p);int n=gaussians.size();
  Rcpp::NumericMatrix position(n,3),covariance(n,6),rgba(n,4);
  for(int i=0;i<n;++i){for(int k=0;k<3;++k)position(i,k)=gaussians[i].centre[k];
    for(int k=0;k<6;++k)covariance(i,k)=gaussians[i].covariance[k];
    for(int k=0;k<4;++k)rgba(i,k)=gaussians[i].rgba[k];}
  return Rcpp::List::create(Rcpp::Named("position")=position,Rcpp::Named("covariance")=covariance,Rcpp::Named("rgba")=rgba);
}

// [[Rcpp::export(rng = false)]]
Rcpp::NumericMatrix splat_project_cpp(Rcpp::List input,Rcpp::List camera,Rcpp::List viewport) {
  ggplot3d::engine::Projection p(ggplot3d::engine::read_camera(camera),ggplot3d::Viewport{Rcpp::as<double>(viewport["width_mm"]),Rcpp::as<double>(viewport["height_mm"]),Rcpp::as<double>(viewport["dpi"])});
  auto gaussians=ggplot3d::gaussian::read(input,p);Rcpp::NumericMatrix out(gaussians.size(),6);
  double pixel=25.4/p.viewport.dpi;
  for(std::size_t i=0;i<gaussians.size();++i){auto v=ggplot3d::gaussian::project(gaussians[i],p,pixel,pixel);
    out(i,0)=v.x;out(i,1)=v.y;out(i,2)=v.z;out(i,3)=v.xx;out(i,4)=v.xy;out(i,5)=v.yy;}
  return out;
}

// [[Rcpp::export(rng = false)]]
void raster_ambient_cpp(SEXP scene, SEXP context) {
  Rcpp::XPtr<ggplot3d::raster::Scene> prepared(scene);
  Rcpp::XPtr<ggplot3d::ambient::Context> ao(context);
  prepared->accessibility.assign(prepared->vertices.size(),{NAN,NAN});
  for (const auto& item:prepared->items) if (item.kind==1) {
    Rcpp::checkUserInterrupt();
    for (ggplot3d::Index j=0;j<item.count;++j) {
      auto& v=prepared->shading[item.offset+j];
      auto normal=v.normal;
      if (!std::isfinite(normal[0]+normal[1]+normal[2]) || ggplot3d::engine::dot(normal,normal)==0)
        normal=prepared->normals[item.row];
      prepared->accessibility[item.offset+j][0]=ao->visibility(v.position,normal);
      for (auto& x:normal) x=-x;
      prepared->accessibility[item.offset+j][1]=ao->visibility(v.position,normal);
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
