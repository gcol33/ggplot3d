#ifndef GGPLOT3D_GEOMETRY_EXTRACT_H
#define GGPLOT3D_GEOMETRY_EXTRACT_H
#include "isosurface.h"
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

static inline Rcpp::DataFrame extract_isosurface(const ScalarGrid& grid,Rcpp::NumericVector levels,bool caps,const std::string& name,
                                                 const dual::GradientFunction& gradient) {
  for(const auto& m:isosurface_methods()) if(name==m.name) {
    for(double level:levels) if(!std::isfinite(level)) Rcpp::stop("Levels must be finite.");
    return m.extract(grid,Rcpp::as<std::vector<double>>(levels),caps,gradient).output();
  }
  Rcpp::stop("Unknown isosurface method.");
}
}
#endif
