#ifndef GGPLOT3D_GEOMETRY_VECTOR_H
#define GGPLOT3D_GEOMETRY_VECTOR_H
#include <Rcpp.h>
#include <array>
#include <vector>
#include <map>
#include <algorithm>
#include <cmath>
#include <limits>
namespace ggplot3d_geometry {
using V = std::array<double, 3>;
static inline V sub(V a, V b) { return {a[0]-b[0],a[1]-b[1],a[2]-b[2]}; }
static inline V add(V a, V b) { return {a[0]+b[0],a[1]+b[1],a[2]+b[2]}; }
static inline V mul(V a, double s) { return {a[0]*s,a[1]*s,a[2]*s}; }
static inline double dot(V a, V b) { return a[0]*b[0]+a[1]*b[1]+a[2]*b[2]; }
static inline V cross(V a, V b) { return {a[1]*b[2]-a[2]*b[1],a[2]*b[0]-a[0]*b[2],a[0]*b[1]-a[1]*b[0]}; }
static inline double norm(V a) { return std::sqrt(dot(a,a)); }
static inline V unit(V a) { double n=norm(a); return n>0 ? mul(a,1/n) : V{0,0,0}; }
static inline int shortest_edge(V a,V b,V c) {
  double ab=dot(sub(b,a),sub(b,a)),bc=dot(sub(c,b),sub(c,b)),ca=dot(sub(a,c),sub(a,c));
  return bc<ab&&bc<=ca?1:(ca<ab&&ca<bc?2:0);
}
}
#endif
