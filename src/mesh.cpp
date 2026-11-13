#include "geometry/verbs.h"
#include "geometry/flip.h"
using namespace ggplot3d_geometry;

namespace {
Rcpp::NumericMatrix named_matrix(const std::vector<double>& values,const std::vector<std::string>& names) {
  int c=names.size(),n=c?values.size()/c:0; Rcpp::NumericMatrix out(n,c);
  for(int i=0;i<n;++i) for(int j=0;j<c;++j) out(i,j)=values[static_cast<size_t>(i)*c+j];
  if(c) Rcpp::colnames(out)=Rcpp::wrap(names);
  return out;
}

// Indexed triangles of one-based vertex indices with their parent faces and
// k numeric channels per vertex.
halfedge::Soup soup_of(const Rcpp::NumericMatrix& xyz,const Rcpp::IntegerMatrix& tri,const Rcpp::IntegerVector& parent,
                       const Rcpp::NumericMatrix& channels) {
  int n=xyz.nrow(),k=channels.ncol();
  halfedge::Soup in; in.k=k;
  for(int i=0;i<n;++i) {
    in.p.push_back({xyz(i,0),xyz(i,1),xyz(i,2)}); in.source.push_back(i);
    for(int c=0;c<k;++c) in.data.push_back(channels(i,c));
  }
  for(int f=0;f<tri.nrow();++f) { in.tri.push_back({tri(f,0)-1,tri(f,1)-1,tri(f,2)-1}); in.parent.push_back(parent[f]); }
  return in;
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
  halfedge::Soup in=soup_of(xyz,tri,parent,channels);
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

// Geodesic paths between the vertices closest to each row of from and to:
// a Dijkstra edge path straightened by FlipOut on an intrinsic
// triangulation, then traced across the input faces. Returns the polyline
// points with their path number (one-based), and per path the geodesic
// length, the number of FlipOut steps, and the one-based end vertices.
// [[Rcpp::export(rng = false)]]
Rcpp::List mesh_geodesic_path_cpp(Rcpp::NumericMatrix xyz,Rcpp::IntegerMatrix tri,Rcpp::NumericMatrix from,
                                  Rcpp::NumericMatrix to,int max_iterations) {
  if(xyz.ncol()!=3||tri.ncol()!=3||from.ncol()!=3||to.ncol()!=3||from.nrow()!=to.nrow()) Rcpp::stop("Invalid geodesic path arrays.");
  Rcpp::IntegerVector parent(tri.nrow()); std::iota(parent.begin(),parent.end(),1);
  halfedge::Mesh mesh=halfedge::build(soup_of(xyz,tri,parent,Rcpp::NumericMatrix(xyz.nrow(),0)));
  halfedge::Intrinsic base(mesh);
  auto nearest=[&](double x,double y,double z) {
    int best=-1; double d2=std::numeric_limits<double>::infinity();
    for(size_t v=0;v<mesh.p.size();++v) {
      if(mesh.vdead[v]||mesh.out[v]<0) continue;
      V d=sub(mesh.p[v],V{x,y,z}); if(dot(d,d)<d2) { d2=dot(d,d); best=v; }
    }
    return best;
  };
  std::vector<double> px,py,pz; std::vector<int> id; int k=from.nrow();
  Rcpp::NumericVector length(k); Rcpp::IntegerVector steps(k),start(k),end(k);
  for(int i=0;i<k;++i) {
    int a=nearest(from(i,0),from(i,1),from(i,2)),b=nearest(to(i,0),to(i,1),to(i,2));
    if(a<0||b<0) Rcpp::stop("The mesh has no faces.");
    start[i]=mesh.source[a]+1; end[i]=mesh.source[b]+1;
    halfedge::Intrinsic s(base);
    std::vector<int> path=halfedge::dijkstra_path(s,a,b);
    if(path.empty()&&a!=b) { length[i]=NA_REAL; continue; }
    int iterations=0;
    path=halfedge::flip_out(s,path,max_iterations,&iterations);
    steps[i]=iterations;
    std::vector<V> points{mesh.p[a]}; double total=0;
    for(int q:path) { halfedge::trace(mesh,base.sign,s,q,points); total+=s.len[q>>1]; }
    length[i]=total;
    for(const V& p:points) { px.push_back(p[0]); py.push_back(p[1]); pz.push_back(p[2]); id.push_back(i+1); }
  }
  return Rcpp::List::create(Rcpp::Named("x")=px,Rcpp::Named("y")=py,Rcpp::Named("z")=pz,Rcpp::Named("path")=id,
    Rcpp::Named("length")=length,Rcpp::Named("iterations")=steps,Rcpp::Named("from")=start,Rcpp::Named("to")=end);
}
