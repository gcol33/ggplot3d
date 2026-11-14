#include "shade/read.h"
#include "shade/hatch.h"
#include "shade/occlusion.h"

using namespace ggplot3d::shade;

// Lit linear colours of rows: unit normals, model-space positions, linear
// base colours and material indices (0 for the default material).
// [[Rcpp::export(rng = false)]]
Rcpp::NumericMatrix shade_cpp(Rcpp::NumericMatrix normal, Rcpp::NumericMatrix position, Rcpp::NumericMatrix base,
                              Rcpp::IntegerVector material, Rcpp::List materials, Rcpp::List lights, Rcpp::List view,
                              SEXP occlusion = R_NilValue) {
  const int n = base.nrow();
  if (normal.nrow() != n || position.nrow() != n || material.size() != n || normal.ncol() != 3 ||
      position.ncol() != 3 || base.ncol() != 3) Rcpp::stop("shade_cpp needs n x 3 normals, positions and colours");
  const Lights l = read_lights(lights);
  const std::vector<Material> m = read_materials(materials);
  const View v = read_view(view);
  ggplot3d::ambient::Context* ao = Rf_isNull(occlusion) ? nullptr : Rcpp::XPtr<ggplot3d::ambient::Context>(occlusion).get();
  Rcpp::NumericMatrix out(n, 3);
  for (int i = 0; i < n; ++i) {
    if (material[i] < 0 || material[i] >= static_cast<int>(m.size())) Rcpp::stop("invalid material index");
    const vec3 p = row3(position, i);
    const vec3 n = normalize(row3(normal, i)), toward = v.at(p);
    const vec3 hemisphere = dot(n,toward) < 0 ? -n : n;
    const double visibility = ao ? ao->visibility({p.x,p.y,p.z},{hemisphere.x,hemisphere.y,hemisphere.z}) : 1;
    const vec3 c = shade(n, p, toward, row3(base, i), m[material[i]], l, -2, visibility);
    for (int k = 0; k < 3; ++k) out(i, k) = c[k];
  }
  return out;
}

// [[Rcpp::export(rng = false)]]
SEXP ambient_occlusion_prepare_cpp(Rcpp::List mesh, int samples, double radius, double amount, double epsilon) {
  return Rcpp::XPtr<ggplot3d::ambient::Context>(new ggplot3d::ambient::Context(mesh,samples,radius,amount,epsilon),true);
}

// [[Rcpp::export(rng = false)]]
Rcpp::NumericVector ambient_occlusion_cpp(SEXP context, Rcpp::NumericMatrix position, Rcpp::NumericMatrix normal) {
  Rcpp::XPtr<ggplot3d::ambient::Context> ao(context);
  if (position.ncol()!=3 || normal.ncol()!=3 || normal.nrow()!=position.nrow()) Rcpp::stop("Invalid ambient occlusion queries");
  Rcpp::NumericVector out(position.nrow());
  for (int i=0;i<position.nrow();++i) {
    if (i%256==0) Rcpp::checkUserInterrupt();
    out[i]=ao->visibility({position(i,0),position(i,1),position(i,2)},{normal(i,0),normal(i,1),normal(i,2)});
  }
  return out;
}

// Swatch colours: linear base colours lit as if every light met the surface
// at light value x (the cosine of the angle to the light), without the
// specular term, by the same kernel as shade_cpp().
// [[Rcpp::export(rng = false)]]
Rcpp::NumericMatrix shade_swatch_cpp(Rcpp::NumericMatrix base, Rcpp::NumericVector x, Rcpp::IntegerVector material,
                                     Rcpp::List materials, Rcpp::List lights) {
  const int n = base.nrow();
  if (x.size() != n || material.size() != n || base.ncol() != 3) Rcpp::stop("shade_swatch_cpp needs n x 3 colours and n light values");
  const Lights l = read_lights(lights);
  const std::vector<Material> m = read_materials(materials);
  const vec3 up(0, 0, 1), origin(0, 0, 0);
  Rcpp::NumericMatrix out(n, 3);
  for (int i = 0; i < n; ++i) {
    if (material[i] < 0 || material[i] >= static_cast<int>(m.size())) Rcpp::stop("invalid material index");
    const vec3 c = shade(up, origin, up, row3(base, i), m[material[i]], l, std::max(-1.0, std::min(1.0, static_cast<double>(x[i]))));
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

// Hatching strokes of a mesh (src/shade/hatch.h): positions (n x 3), 0-based
// triangle vertex indices (m x 3), unit plane normals per family (f x 3),
// spacing and the ranks drawn per triangle and family (m x f). Returns the
// polyline vertices with their 1-based line, family and 0-based face.
// [[Rcpp::export(rng = false)]]
Rcpp::List hatch_lines_cpp(Rcpp::NumericMatrix xyz, Rcpp::IntegerMatrix faces, Rcpp::NumericMatrix directions,
                           double spacing, Rcpp::IntegerMatrix levels) {
  if (xyz.ncol() != 3 || faces.ncol() != 3 || directions.ncol() != 3 || levels.nrow() != faces.nrow() ||
      levels.ncol() != directions.nrow() || !(spacing > 0)) Rcpp::stop("invalid hatching input");
  for (int i = 0; i < faces.size(); ++i) if (faces[i] < 0 || faces[i] >= xyz.nrow()) Rcpp::stop("invalid hatching face");
  const ggplot3d::hatch::Lines out = ggplot3d::hatch::lines(&xyz[0], xyz.nrow(), faces.nrow() ? &faces[0] : nullptr, faces.nrow(),
    &directions[0], directions.nrow(), spacing, levels.nrow() ? &levels[0] : nullptr);
  return Rcpp::List::create(Rcpp::Named("x") = out.x, Rcpp::Named("y") = out.y, Rcpp::Named("z") = out.z,
    Rcpp::Named("line") = out.line, Rcpp::Named("family") = out.family, Rcpp::Named("face") = out.face);
}

// The shading kernel as GLSL for the viewer.
// [[Rcpp::export(rng = false)]]
std::string shade_glsl_cpp(int max_lights) {
  return shade_glsl(max_lights);
}
