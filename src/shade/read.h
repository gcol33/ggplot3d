#pragma once
#include <Rcpp.h>
#include "shade.h"

namespace ggplot3d { namespace shade {
// A light resolved on the R side: list(direction = model-space unit vector,
// ambient, diffuse, mode = "multiply" or "tint").
static inline Light read_light(const Rcpp::List& x) {
  Light light;
  Rcpp::NumericVector direction = x["direction"];
  if (direction.size() != 3) Rcpp::stop("light direction must have three components");
  light.direction = unit({direction[0], direction[1], direction[2]});
  light.ambient = Rcpp::as<double>(x["ambient"]);
  light.diffuse = Rcpp::as<double>(x["diffuse"]);
  light.multiply = Rcpp::as<std::string>(x["mode"]) == "multiply";
  return light;
}
} }
