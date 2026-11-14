#include "shade/read.h"

using namespace ggplot3d::shade;

// Lit linear colours of rows: unit normals, model-space positions, linear
// base colours and material indices (0 for the default material).
// [[Rcpp::export(rng = false)]]
Rcpp::NumericMatrix shade_cpp(Rcpp::NumericMatrix normal, Rcpp::NumericMatrix position, Rcpp::NumericMatrix base,
                              Rcpp::IntegerVector material, Rcpp::List materials, Rcpp::List lights, Rcpp::List view) {
  const int n = base.nrow();
  if (normal.nrow() != n || position.nrow() != n || material.size() != n || normal.ncol() != 3 ||
      position.ncol() != 3 || base.ncol() != 3) Rcpp::stop("shade_cpp needs n x 3 normals, positions and colours");
  const Lights l = read_lights(lights);
  const std::vector<Material> m = read_materials(materials);
  const View v = read_view(view);
  Rcpp::NumericMatrix out(n, 3);
  for (int i = 0; i < n; ++i) {
    if (material[i] < 0 || material[i] >= static_cast<int>(m.size())) Rcpp::stop("invalid material index");
    const vec3 p = row3(position, i);
    const vec3 c = shade(normalize(row3(normal, i)), p, v.at(p), row3(base, i), m[material[i]], l);
    for (int k = 0; k < 3; ++k) out(i, k) = c[k];
  }
  return out;
}

// [[Rcpp::export(rng = false)]]
Rcpp::NumericMatrix shade_transfer_cpp(Rcpp::NumericMatrix linear, Rcpp::NumericVector brightness, bool multiply) {
  const int n = linear.nrow();
  if (brightness.size() != n) Rcpp::stop("one brightness per colour is required");
  Rcpp::NumericMatrix out(n, linear.ncol());
  for (int i = 0; i < n; ++i) for (int k = 0; k < linear.ncol(); ++k) out(i, k) = transfer(linear(i, k), brightness[i], multiply);
  return out;
}

// The shading kernel as GLSL for the viewer.
// [[Rcpp::export(rng = false)]]
std::string shade_glsl_cpp(int max_lights) {
  return shade_glsl(max_lights);
}
