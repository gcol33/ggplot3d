#include "shade/read.h"

using namespace ggplot3d::shade;

static inline Vec3 row3(const Rcpp::NumericMatrix& m, int i) {
  const int r = m.nrow() == 1 ? 0 : i;
  return {m(r, 0), m(r, 1), m(r, 2)};
}

// [[Rcpp::export(rng = false)]]
Rcpp::NumericMatrix shade_cpp(Rcpp::NumericMatrix normal, Rcpp::NumericMatrix toward,
                              Rcpp::NumericMatrix base, Rcpp::List light) {
  const int n = base.nrow();
  if (normal.ncol() != 3 || toward.ncol() != 3 || base.ncol() != 3 || normal.nrow() != n ||
      (toward.nrow() != 1 && toward.nrow() != n)) Rcpp::stop("shade_cpp needs n x 3 normals, colours and view directions");
  const Light l = read_light(light);
  Rcpp::NumericMatrix out(n, 3);
  for (int i = 0; i < n; ++i) {
    const Vec3 c = shade(unit(row3(normal, i)), unit(row3(toward, i)), row3(base, i), l);
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
