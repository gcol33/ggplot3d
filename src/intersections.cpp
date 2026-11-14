#include "geometry/intersections.h"
using namespace ggplot3d_geometry;

// Transverse triangle intersections, with parent faces retained at both ends.
// [[Rcpp::export(rng = false)]]
Rcpp::DataFrame geometry_intersections_cpp(Rcpp::NumericMatrix xyz,Rcpp::IntegerMatrix faces,Rcpp::IntegerVector parent,double tolerance) {
  if(xyz.ncol()!=3||faces.ncol()!=3||parent.size()!=faces.nrow()||!std::isfinite(tolerance)||tolerance<=0)
    Rcpp::stop("Invalid intersection arrays or tolerance.");
  std::vector<CrossingTriangle> triangles;
  for(int i=0;i<faces.nrow();++i) {
    CrossingTriangle t;
    for(int k=0;k<3;++k) {
      int v=faces(i,k)-1;
      if(v<0||v>=xyz.nrow()) Rcpp::stop("Invalid triangle index.");
      t.vertex[k]=v;
      for(int d=0;d<3;++d) {t.p[k][d]=xyz(v,d); if(!std::isfinite(t.p[k][d])) Rcpp::stop("Intersection positions must be finite.");}
    }
    t.normal=unit(cross(sub(t.p[1],t.p[0]),sub(t.p[2],t.p[0])));
    if(norm(t.normal)==0) continue;
    t.lo=t.hi=t.p[0]; t.parent=parent[i];
    for(V p:t.p) for(int d=0;d<3;++d) {t.lo[d]=std::min(t.lo[d],p[d]); t.hi[d]=std::max(t.hi[d],p[d]);}
    triangles.push_back(t);
  }
  auto segments=triangle_crossings(triangles,tolerance);
  int n=2*segments.size();
  Rcpp::NumericVector x(n),y(n),z(n);
  Rcpp::IntegerVector first(n),second(n),group(n);
  for(int i=0;i<n;++i) {
    const auto& s=segments[i/2]; V p=i%2?s.b:s.a;
    x[i]=p[0]; y[i]=p[1]; z[i]=p[2]; first[i]=s.first; second[i]=s.second; group[i]=i/2+1;
  }
  return Rcpp::DataFrame::create(Rcpp::Named("x")=x,Rcpp::Named("y")=y,Rcpp::Named("z")=z,
    Rcpp::Named("face1")=first,Rcpp::Named("face2")=second,Rcpp::Named("group")=group);
}
