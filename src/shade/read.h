#pragma once
#include <Rcpp.h>
#include "shade.h"

namespace ggplot3d { namespace shade {
static inline vec3 row3(const Rcpp::NumericMatrix& m, int i) {return {m(i, 0), m(i, 1), m(i, 2)};}

// Lights resolved on the R side (resolved_light_3d()): kind, vector, colour,
// ground (rows), falloff, ambient (linear colour), transfer (a kernel transfer
// id), range (shadow and highlight moves), neutral (cosine), shadow and
// highlight (linear tone colours).
static inline Lights read_lights(const Rcpp::List& x) {
  Lights out;
  Rcpp::IntegerVector kind = x["kind"];
  Rcpp::NumericMatrix vector = x["vector"], colour = x["colour"], ground = x["ground"];
  Rcpp::NumericVector falloff = x["falloff"], ambient = x["ambient"];
  const int n = kind.size();
  if (vector.nrow() != n || colour.nrow() != n || ground.nrow() != n || falloff.size() != n || ambient.size() != 3)
    Rcpp::stop("invalid resolved lights");
  for (int i = 0; i < n; ++i) {
    Light l;
    l.kind = kind[i]; l.vector = row3(vector, i); l.colour = row3(colour, i); l.ground = row3(ground, i);
    l.falloff = falloff[i];
    out.list.push_back(l);
  }
  out.ambient = {ambient[0], ambient[1], ambient[2]};
  Rcpp::NumericVector range = x["range"], shadow = x["shadow"], highlight = x["highlight"];
  if (range.size() != 2 || shadow.size() != 3 || highlight.size() != 3) Rcpp::stop("invalid resolved lights");
  out.transfer = Rcpp::as<int>(x["transfer"]);
  out.r0 = range[0]; out.r1 = range[1];
  out.neutral = Rcpp::as<double>(x["neutral"]);
  out.shadow = {shadow[0], shadow[1], shadow[2]};
  out.highlight = {highlight[0], highlight[1], highlight[2]};
  return out;
}

// Materials resolved on the R side (kernel_materials_3d()): one entry per
// row, after the default material at index 0.
static inline std::vector<Material> read_materials(const Rcpp::List& x) {
  std::vector<Material> out(1);
  Rcpp::NumericVector ambient = x["ambient"], diffuse = x["diffuse"], specular = x["specular"],
    shininess = x["shininess"], reflectance = x["specular_colour"];
  Rcpp::NumericMatrix back = x["back"];
  for (R_xlen_t i = 0; i < ambient.size(); ++i) {
    Material m;
    m.ambient = ambient[i]; m.diffuse = diffuse[i]; m.specular = specular[i];
    m.shininess = shininess[i]; m.specular_colour = reflectance[i];
    m.has_back = std::isfinite(back(i, 0) + back(i, 1) + back(i, 2));
    if (m.has_back) m.back = row3(back, i);
    out.push_back(m);
  }
  return out;
}

// The viewing direction: toward the eye from each point under perspective,
// else the constant `toward`.
struct View {
  bool perspective = false;
  vec3 eye{0, 0, 0}, toward{0, 0, 1};
  vec3 at(vec3 p) const {return perspective ? normalize(eye - p) : toward;}
};
static inline View read_view(const Rcpp::List& x) {
  View v;
  v.perspective = Rcpp::as<bool>(x["perspective"]);
  Rcpp::NumericVector eye = x["eye"], toward = x["toward"];
  v.eye = {eye[0], eye[1], eye[2]};
  v.toward = normalize(vec3(toward[0], toward[1], toward[2]));
  return v;
}
} }
