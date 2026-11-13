#include "geometry/sweep.h"
using namespace ggplot3d_geometry;

// Rotation-minimising frames of the polyline in the rows of xyz. hint
// orients the first normal. Returns tangent, normal and binormal matrices
// and the arc length at every point.
// [[Rcpp::export(rng = false)]]
Rcpp::List sweep_frames_cpp(Rcpp::NumericMatrix xyz,bool closed,Rcpp::NumericVector hint) {
  if(xyz.ncol()!=3||hint.size()!=3) Rcpp::stop("Invalid curve arrays.");
  std::vector<V> x(xyz.nrow());
  for(int i=0;i<xyz.nrow();++i) x[i]={xyz(i,0),xyz(i,1),xyz(i,2)};
  auto f=sweep::frames(x,closed,V{hint[0],hint[1],hint[2]});
  auto matrix=[](const std::vector<V>& v) {
    Rcpp::NumericMatrix m(v.size(),3);
    for(size_t i=0;i<v.size();++i) for(int a=0;a<3;++a) m(i,a)=v[i][a];
    return m;
  };
  return Rcpp::List::create(Rcpp::Named("tangent")=matrix(f.t),Rcpp::Named("normal")=matrix(f.r),
    Rcpp::Named("binormal")=matrix(f.s),Rcpp::Named("arc")=Rcpp::wrap(f.arc),Rcpp::Named("length")=f.length);
}
