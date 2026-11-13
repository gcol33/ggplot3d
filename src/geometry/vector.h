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
struct Corner { V p, n; int id; };
struct Mesh {
  std::vector<Corner> corners;
  std::vector<double> levels;
  std::vector<int> caps;
  void triangle(Corner a, Corner b, Corner c, V direction, double level, bool cap=false) {
    V ab=sub(b.p,a.p), ac=sub(c.p,a.p), normal=cross(ab,ac);
    if (norm(normal)<=128*std::numeric_limits<double>::epsilon()*std::max(dot(ab,ab),dot(ac,ac))) return;
    if (dot(normal,direction)<0) std::swap(b,c);
    // A short first edge reduces cancellation in geometric face-normal calculations.
    int start=shortest_edge(a.p,b.p,c.p);
    if(start==1) {Corner first=a;a=b;b=c;c=first;}
    else if(start==2) {Corner first=c;c=b;b=a;a=first;}
    corners.insert(corners.end(),{a,b,c}); levels.push_back(level); caps.push_back(cap);
  }
  Rcpp::DataFrame output() const {
    int m=corners.size();
    Rcpp::NumericVector x(m),y(m),z(m),nx(m),ny(m),nz(m),level(m);
    Rcpp::IntegerVector face(m),corner(m),vertex(m),cap(m);
    for(int i=0;i<m;++i) {
      x[i]=corners[i].p[0]; y[i]=corners[i].p[1]; z[i]=corners[i].p[2];
      nx[i]=corners[i].n[0]; ny[i]=corners[i].n[1]; nz[i]=corners[i].n[2];
      level[i]=levels[i/3]; face[i]=i/3+1; corner[i]=i%3+1; vertex[i]=corners[i].id+1; cap[i]=caps[i/3];
    }
    return Rcpp::DataFrame::create(Rcpp::Named("x")=x,Rcpp::Named("y")=y,Rcpp::Named("z")=z,
      Rcpp::Named("nx")=nx,Rcpp::Named("ny")=ny,Rcpp::Named("nz")=nz,Rcpp::Named("level")=level,
      Rcpp::Named(".face")=face,Rcpp::Named(".corner")=corner,Rcpp::Named(".vertex")=vertex,Rcpp::Named("cap")=cap);
  }
};
}
#endif
