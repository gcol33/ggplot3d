#include "geometry/processing.h"
using namespace ggplot3d_geometry;

namespace {
Rcpp::NumericMatrix named_matrix(const std::vector<double>& values,const std::vector<std::string>& names) {
  int c=names.size(),n=c?values.size()/c:0; Rcpp::NumericMatrix out(n,c);
  for(int i=0;i<n;++i) for(int j=0;j<c;++j) out(i,j)=values[static_cast<size_t>(i)*c+j];
  if(c) Rcpp::colnames(out)=Rcpp::wrap(names);
  return out;
}
}

// Applies a mesh verb to indexed triangles. xyz holds one vertex per row,
// tri one-based vertex indices, parent the input face of each triangle and
// channels numeric vertex attributes. Returns the processed vertices and
// triangles with the one-based input vertex (source) and the parent of
// each, interpolated channels, and the verb's corner and vertex values.
// [[Rcpp::export(rng = false)]]
Rcpp::List mesh_process_cpp(Rcpp::NumericMatrix xyz,Rcpp::IntegerMatrix tri,Rcpp::IntegerVector parent,
                            Rcpp::NumericMatrix channels,std::string verb,Rcpp::List args) {
  auto found=halfedge::verbs().find(verb);
  if(found==halfedge::verbs().end()) Rcpp::stop("Unknown mesh verb.");
  int n=xyz.nrow(),k=channels.ncol();
  if(xyz.ncol()!=3||tri.ncol()!=3||parent.size()!=tri.nrow()||channels.nrow()!=n) Rcpp::stop("Invalid mesh arrays.");
  halfedge::Soup in; in.k=k;
  for(int i=0;i<n;++i) {
    in.p.push_back({xyz(i,0),xyz(i,1),xyz(i,2)}); in.source.push_back(i);
    for(int c=0;c<k;++c) in.data.push_back(channels(i,c));
  }
  for(int f=0;f<tri.nrow();++f) { in.tri.push_back({tri(f,0)-1,tri(f,1)-1,tri(f,2)-1}); in.parent.push_back(parent[f]); }
  halfedge::Result r=found->second(in,args);
  const halfedge::Soup& s=r.soup; int m=s.p.size(),t=s.tri.size();
  Rcpp::NumericMatrix p(m,3),data(m,k); Rcpp::IntegerVector source(m),par(t); Rcpp::IntegerMatrix faces(t,3);
  for(int i=0;i<m;++i) { for(int a=0;a<3;++a) p(i,a)=s.p[i][a]; source[i]=s.source[i]+1; for(int c=0;c<k;++c) data(i,c)=s.data[static_cast<size_t>(i)*k+c]; }
  for(int f=0;f<t;++f) { for(int a=0;a<3;++a) faces(f,a)=s.tri[f][a]+1; par[f]=s.parent[f]; }
  Rcpp::NumericVector info(r.info.size()); Rcpp::CharacterVector info_names(r.info.size());
  for(size_t i=0;i<r.info.size();++i) { info[i]=r.info[i].second; info_names[i]=r.info[i].first; }
  info.attr("names")=info_names;
  return Rcpp::List::create(Rcpp::Named("xyz")=p,Rcpp::Named("triangles")=faces,Rcpp::Named("source")=source,
    Rcpp::Named("parent")=par,Rcpp::Named("channels")=data,Rcpp::Named("corner")=named_matrix(r.corner,r.corner_names),
    Rcpp::Named("vertex")=named_matrix(r.vertex,r.vertex_names),Rcpp::Named("info")=info);
}
