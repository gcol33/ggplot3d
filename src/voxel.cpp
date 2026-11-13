#include <Rcpp.h>
#include "geometry/voxel.h"

// Visible voxel face rectangles; translucent[m] flags material m (entry 0 unused).
// [[Rcpp::export(rng = false)]]
Rcpp::IntegerMatrix voxel_faces_cpp(Rcpp::IntegerVector n,Rcpp::IntegerVector material,Rcpp::LogicalVector translucent,bool greedy) {
  int dims[3]={n[0],n[1],n[2]};
  if(static_cast<double>(dims[0])*dims[1]*dims[2]!=material.size()) Rcpp::stop("Voxel grid dimensions do not match the materials.");
  std::vector<char> flags(translucent.size());
  for(int i=0;i<translucent.size();++i) flags[i]=translucent[i]==TRUE;
  for(int m:material) if(m<0||m>=translucent.size()) Rcpp::stop("Voxel materials must index the translucency flags.");
  auto faces=ggplot3d_geometry::voxel::greedy_faces(dims,Rcpp::as<std::vector<int>>(material),flags,greedy);
  Rcpp::IntegerMatrix out(faces.size(),8);
  for(size_t i=0;i<faces.size();++i) {
    const auto& f=faces[i];
    int row[8]={f.axis,f.side,f.plane,f.u0,f.v0,f.u1,f.v1,f.material};
    for(int c=0;c<8;++c) out(i,c)=row[c];
  }
  Rcpp::colnames(out)=Rcpp::CharacterVector::create("axis","side","plane","u0","v0","u1","v1","material");
  return out;
}
