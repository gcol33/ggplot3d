#ifndef GGPLOT3D_GEOMETRY_EXTRACT_H
#define GGPLOT3D_GEOMETRY_EXTRACT_H
#include "slice.h"
#include "dual.h"
namespace ggplot3d_geometry {

// Isosurface extraction methods by name. Every method extracts all levels in
// one sweep and returns one mesh with per-triangle levels and cap flags.
// gradient, when set, gives the field gradient at a set of points; methods
// that use crossing normals take it in place of interpolated grid gradients.
using Extractor=Mesh(*)(const ScalarGrid&,const std::vector<double>&,bool,const dual::GradientFunction&);
struct Method { const char* name; Extractor extract; };

static inline const std::vector<Method>& isosurface_methods() {
  static const std::vector<Method> methods{
    {"cubes",[](const ScalarGrid& g,const std::vector<double>& l,bool c,const dual::GradientFunction&) { return march<cubes::cell,no_finish>(g,l,c); }},
    {"tetra",[](const ScalarGrid& g,const std::vector<double>& l,bool c,const dual::GradientFunction&) { return march<tetra::cell,tetra::regularise>(g,l,c); }},
    {"dual",dual::extract<true>},
    {"nets",dual::extract<false>}
  };
  return methods;
}

// With a clipping plane, the surface (and its box caps) keeps the part on the
// plane's negative side, and caps closes the sublevel region on the plane too.
static inline Rcpp::DataFrame extract_isosurface(const ScalarGrid& grid,Rcpp::NumericVector levels,bool caps,const std::string& name,
                                                 const dual::GradientFunction& gradient,const Plane* clip=nullptr) {
  for(const auto& m:isosurface_methods()) if(name==m.name) {
    for(double level:levels) if(!std::isfinite(level)) Rcpp::stop("Levels must be finite.");
    std::vector<double> l=Rcpp::as<std::vector<double>>(levels);
    Mesh mesh=m.extract(grid,l,caps,gradient);
    if(clip) {
      mesh=clip_mesh(mesh,*clip);
      if(caps) plane_caps(mesh,grid,l,*clip);
    }
    return mesh.output();
  }
  Rcpp::stop("Unknown isosurface method.");
}
}
#endif
