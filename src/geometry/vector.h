#ifndef GGPLOT3D_GEOMETRY_VECTOR_H
#define GGPLOT3D_GEOMETRY_VECTOR_H
#include <Rcpp.h>
#include <array>
#include <vector>
#include <map>
#include <algorithm>
#include <cmath>
#include <limits>
#include "../vec3.h"
namespace ggplot3d_geometry {
using V = ggplot3d::vec3::Vec3;
using ggplot3d::vec3::sub;
using ggplot3d::vec3::add;
using ggplot3d::vec3::mul;
using ggplot3d::vec3::dot;
using ggplot3d::vec3::cross;
using ggplot3d::vec3::norm;
using ggplot3d::vec3::unit;
// Rows of a numeric matrix as points; columns past the matrix read as zero.
inline std::vector<V> matrix_points(const Rcpp::NumericMatrix& m, bool finite = false) {
  std::vector<V> out(m.nrow(), V{0,0,0});
  const int columns = std::min(m.ncol(), 3);
  for(int i=0;i<m.nrow();++i) for(int k=0;k<columns;++k) {
    out[i][k]=m(i,k);
    if(finite && !std::isfinite(out[i][k])) Rcpp::stop("Coordinates must be finite.");
  }
  return out;
}
static inline int shortest_edge(V a,V b,V c) {
  double ab=dot(sub(b,a),sub(b,a)),bc=dot(sub(c,b),sub(c,b)),ca=dot(sub(a,c),sub(a,c));
  return bc<ab&&bc<=ca?1:(ca<ab&&ca<bc?2:0);
}
}
#endif
