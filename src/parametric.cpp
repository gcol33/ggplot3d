#include "geometry/parametric.h"
#include <map>
using namespace ggplot3d_geometry;
using parametric::UV;

namespace {
std::vector<V> points(const Rcpp::NumericMatrix& m) {
  std::vector<V> out(m.nrow());
  for(int i=0;i<m.nrow();++i) out[i]={m(i,0),m(i,1),m(i,2)};
  return out;
}
std::vector<std::array<int,3>> triangles(const Rcpp::IntegerMatrix& m) {
  std::vector<std::array<int,3>> out(m.nrow());
  for(int i=0;i<m.nrow();++i) out[i]={m(i,0)-1,m(i,1)-1,m(i,2)-1};
  return out;
}
std::vector<std::array<UV,3>> corners(const Rcpp::NumericMatrix& u,const Rcpp::NumericMatrix& v) {
  std::vector<std::array<UV,3>> out(u.nrow());
  for(int i=0;i<u.nrow();++i) for(int k=0;k<3;++k) out[i][k]={u(i,k),v(i,k)};
  return out;
}
std::vector<std::array<bool,2>> flags(const Rcpp::LogicalMatrix& m) {
  std::vector<std::array<bool,2>> out(m.nrow());
  for(int i=0;i<m.nrow();++i) out[i]={m(i,0)==TRUE,m(i,1)==TRUE};
  return out;
}
Rcpp::List mesh_list(const std::vector<std::array<int,3>>& tri,const std::vector<std::array<UV,3>>& uv) {
  int n=tri.size(); Rcpp::IntegerMatrix t(n,3); Rcpp::NumericMatrix u(n,3),v(n,3);
  for(int i=0;i<n;++i) for(int k=0;k<3;++k) { t(i,k)=tri[i][k]+1; u(i,k)=uv[i][k][0]; v(i,k)=uv[i][k][1]; }
  return Rcpp::List::create(Rcpp::Named("triangles")=t,Rcpp::Named("u")=u,Rcpp::Named("v")=v);
}
}

// Triangulated (u, v) grid. xyz holds the n[0] x n[1] samples (u fastest);
// samples at coincident positions (poles, degenerate rows) are welded and the
// parameter that varies within a welded group is flagged free.
// [[Rcpp::export(rng = false)]]
Rcpp::List parametric_grid_cpp(Rcpp::IntegerVector n,Rcpp::LogicalVector closed,Rcpp::LogicalVector twist,
                               Rcpp::NumericMatrix xyz,Rcpp::NumericVector u,Rcpp::NumericVector v) {
  parametric::QuadGrid grid{{n[0],n[1]},{closed[0]==TRUE,closed[1]==TRUE},{twist[0]==TRUE,twist[1]==TRUE}};
  std::vector<V> p=points(xyz); int m=p.size();
  V lo=p[0],hi=p[0];
  for(const V& x:p) for(int a=0;a<3;++a) { lo[a]=std::min(lo[a],x[a]); hi[a]=std::max(hi[a],x[a]); }
  double eps=1e-10*std::max(norm(sub(hi,lo)),std::numeric_limits<double>::min());
  std::map<std::array<long long,3>,int> cells; std::vector<int> weld(m),sample;
  std::vector<std::array<bool,2>> free;
  for(int s=0;s<m;++s) {
    std::array<long long,3> key;
    for(int a=0;a<3;++a) key[a]=std::llround((p[s][a]-lo[a])/eps);
    auto found=cells.find(key);
    if(found==cells.end()) { int id=sample.size(); cells[key]=id; weld[s]=id; sample.push_back(s); free.push_back({false,false}); }
    else {
      int id=found->second,first=sample[id]; weld[s]=id;
      if(first%n[0]!=s%n[0]) free[id][0]=true;
      if(first/n[0]!=s/n[0]) free[id][1]=true;
    }
  }
  std::vector<V> welded(sample.size());
  for(size_t i=0;i<sample.size();++i) welded[i]=p[sample[i]];
  std::vector<std::array<int,3>> tri; std::vector<std::array<UV,3>> uv;
  grid.quads([&](std::array<int,4> q,std::array<int,4> qi,std::array<int,4> qj) {
    std::array<int,4> w; for(int a=0;a<4;++a) w[a]=weld[q[a]];
    std::vector<std::array<int,3>> local,which;
    parametric::split_quad(w,welded,local,&which);
    for(size_t t=0;t<local.size();++t) {
      tri.push_back(local[t]); std::array<UV,3> c;
      for(int k=0;k<3;++k) c[k]={u[0]+u[1]*qi[which[t][k]],v[0]+v[1]*qj[which[t][k]]};
      uv.push_back(c);
    }
  });
  Rcpp::List out=mesh_list(tri,uv);
  Rcpp::IntegerVector rep(sample.size()); Rcpp::LogicalMatrix fr(sample.size(),2);
  for(size_t i=0;i<sample.size();++i) { rep[i]=sample[i]+1; fr(i,0)=free[i][0]; fr(i,1)=free[i][1]; }
  out["sample"]=rep; out["free"]=fr;
  return out;
}

// Unique edges with their midpoint parameters and the edge of each triangle side.
// [[Rcpp::export(rng = false)]]
Rcpp::List parametric_edges_cpp(Rcpp::IntegerMatrix tri,Rcpp::NumericMatrix u,Rcpp::NumericMatrix v,Rcpp::LogicalMatrix free) {
  auto e=parametric::edges(triangles(tri),corners(u,v),flags(free));
  int m=e.ends.size(); Rcpp::IntegerMatrix ends(m,2); Rcpp::NumericMatrix mid(m,2);
  for(int i=0;i<m;++i) { ends(i,0)=e.ends[i][0]+1; ends(i,1)=e.ends[i][1]+1; mid(i,0)=e.uv[i][0]; mid(i,1)=e.uv[i][1]; }
  return Rcpp::List::create(Rcpp::Named("ends")=ends,Rcpp::Named("uv")=mid);
}

// One round of conforming longest-edge bisection of the marked edges.
// [[Rcpp::export(rng = false)]]
Rcpp::List parametric_bisect_cpp(Rcpp::IntegerMatrix tri,Rcpp::NumericMatrix u,Rcpp::NumericMatrix v,Rcpp::LogicalMatrix free,
                                 Rcpp::NumericMatrix xyz,Rcpp::LogicalVector marked) {
  auto t=triangles(tri); auto c=corners(u,v); auto f=flags(free);
  auto e=parametric::edges(t,c,f);
  if(static_cast<size_t>(marked.size())!=e.ends.size()) Rcpp::stop("One mark per edge is required.");
  std::vector<char> mark(marked.size()); for(int i=0;i<marked.size();++i) mark[i]=marked[i]==TRUE;
  auto s=parametric::bisect(t,c,f,points(xyz),e,mark);
  Rcpp::List out=mesh_list(s.tri,s.uv);
  Rcpp::IntegerVector split(s.split_edges.size());
  for(size_t i=0;i<s.split_edges.size();++i) split[i]=s.split_edges[i]+1;
  out["split"]=split;
  return out;
}

// [[Rcpp::export(rng = false)]]
Rcpp::NumericMatrix parametric_face_normals_cpp(Rcpp::IntegerMatrix tri,Rcpp::NumericMatrix xyz) {
  auto n=parametric::face_normals(triangles(tri),points(xyz));
  Rcpp::NumericMatrix out(n.size(),3);
  for(size_t i=0;i<n.size();++i) for(int a=0;a<3;++a) out(i,a)=n[i][a];
  return out;
}
