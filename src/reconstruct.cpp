#include "geometry/alpha.h"
#include "geometry/normals.h"
#include "geometry/poisson.h"
using namespace ggplot3d_geometry;

namespace {
std::vector<V> rows(const Rcpp::NumericMatrix& m) {
  std::vector<V> out(m.nrow(),V{0,0,0});
  for(int i=0;i<m.nrow();++i) for(int k=0;k<m.ncol()&&k<3;++k) {
    out[i][k]=m(i,k);
    if(!std::isfinite(out[i][k])) Rcpp::stop("Coordinates must be finite.");
  }
  return out;
}
}

// Delaunay triangles of the rows of an n x 2 matrix, counterclockwise, as
// 1-based row indices; no rows when the points are collinear.
// [[Rcpp::export(rng = false)]]
Rcpp::IntegerMatrix geometry_delaunay_cpp(Rcpp::NumericMatrix xy) {
  if(xy.ncol()!=2) Rcpp::stop("Delaunay triangulation requires two columns.");
  Delaunay<2> dt(rows(xy));
  std::vector<int> kept;
  for(size_t s=0;s<dt.vertex.size();++s) if(dt.alive[s]&&!dt.ghost(s)) kept.push_back(s);
  Rcpp::IntegerMatrix out(kept.size(),3);
  for(size_t i=0;i<kept.size();++i) for(int k=0;k<3;++k) out(i,k)=dt.vertex[kept[i]][k]+1;
  return out;
}

// Delaunay tetrahedra of the rows of an n x 3 matrix, positively oriented,
// as 1-based row indices.
// [[Rcpp::export(rng = false)]]
Rcpp::IntegerMatrix geometry_delaunay3_cpp(Rcpp::NumericMatrix xyz) {
  if(xyz.ncol()!=3) Rcpp::stop("Delaunay tetrahedralisation requires three columns.");
  Delaunay<3> dt(rows(xyz));
  std::vector<int> kept;
  for(size_t s=0;s<dt.vertex.size();++s) if(dt.alive[s]&&!dt.ghost(s)) kept.push_back(s);
  Rcpp::IntegerMatrix out(kept.size(),4);
  for(size_t i=0;i<kept.size();++i) for(int k=0;k<4;++k) out(i,k)=dt.vertex[kept[i]][k]+1;
  return out;
}

// Boundary triangles of the alpha complex, as 1-based row indices with a
// column flagging triangles that bound a tetrahedron of the complex. A
// missing alpha selects the smallest alpha whose tetrahedra cover every
// point; an infinite alpha gives the convex hull. The alpha used is returned
// as an attribute.
// [[Rcpp::export(rng = false)]]
Rcpp::IntegerMatrix geometry_alpha_shape_cpp(Rcpp::NumericMatrix xyz,double alpha) {
  if(xyz.ncol()!=3) Rcpp::stop("Alpha shapes require three columns.");
  Delaunay<3> dt(rows(xyz));
  if(std::isnan(alpha)) alpha=alpha::covering_alpha(dt)*(1+1e-9);
  auto faces=alpha::faces(dt,alpha);
  Rcpp::IntegerMatrix out(faces.size(),4);
  for(size_t i=0;i<faces.size();++i) { for(int k=0;k<3;++k) out(i,k)=faces[i][k]+1; out(i,3)=faces[i][3]; }
  out.attr("alpha")=alpha;
  return out;
}

// Polyharmonic interpolant s(x) = sum_i w_i |x - c_i| + p0 + p . x at the
// rows of points.
// [[Rcpp::export(rng = false)]]
Rcpp::NumericVector geometry_rbf_cpp(Rcpp::NumericMatrix centres,Rcpp::NumericVector weights,Rcpp::NumericVector poly,Rcpp::NumericMatrix points) {
  int n=centres.nrow(),m=points.nrow();
  if(centres.ncol()!=3||points.ncol()!=3||weights.size()!=n||poly.size()!=4) Rcpp::stop("Invalid radial basis function input.");
  Rcpp::NumericVector out(m);
  for(int j=0;j<m;++j) {
    if(j%4096==0) Rcpp::checkUserInterrupt();
    double x=points(j,0),y=points(j,1),z=points(j,2),s=poly[0]+poly[1]*x+poly[2]*y+poly[3]*z;
    for(int i=0;i<n;++i) {
      double dx=x-centres(i,0),dy=y-centres(i,1),dz=z-centres(i,2);
      s+=weights[i]*std::sqrt(dx*dx+dy*dy+dz*dz);
    }
    out[j]=s;
  }
  return out;
}

// Unit normals of the rows of an n x 3 matrix from their k nearest
// neighbours, consistently oriented (outwards on a closed surface).
// [[Rcpp::export(rng = false)]]
Rcpp::NumericMatrix geometry_point_normals_cpp(Rcpp::NumericMatrix xyz,int k) {
  if(xyz.ncol()!=3||xyz.nrow()<2) Rcpp::stop("Normal estimation needs at least two points in three columns.");
  if(k<1) Rcpp::stop("k must be positive.");
  std::vector<V> n=normals::estimate(rows(xyz),k);
  Rcpp::NumericMatrix out(n.size(),3);
  for(size_t i=0;i<n.size();++i) for(int a=0;a<3;++a) out(i,a)=n[i][a];
  return out;
}

// Screened Poisson surface of oriented points (rows of xyz with unit normals
// in the rows of normal), extracted by octree dual contouring ("dual") or
// surface nets ("nets") with `refine` regula falsi steps per crossing. The
// iso-value, the nodes and conjugate gradient iterations per depth, and the
// seconds spent on the octree, right-hand sides, solve and extraction are
// returned as attributes.
// [[Rcpp::export(rng = false)]]
Rcpp::DataFrame geometry_poisson_cpp(Rcpp::NumericMatrix xyz,Rcpp::NumericMatrix normal,int depth,double screening,
                                     double samples_per_node,std::string method,int refine) {
  if(xyz.ncol()!=3||normal.ncol()!=3||normal.nrow()!=xyz.nrow()||xyz.nrow()<1) Rcpp::stop("Invalid Poisson reconstruction input.");
  if(depth<1||depth>10) Rcpp::stop("depth must lie between 1 and 10.");
  if(method!="dual"&&method!="nets") Rcpp::stop("Poisson reconstruction extracts with the \"dual\" or \"nets\" method.");
  poisson::Options opt; opt.depth=depth; opt.screening=screening; opt.samples_per_node=samples_per_node;
  poisson::Fit f=poisson::fit(rows(xyz),rows(normal),opt);
  auto clock=std::chrono::steady_clock::now();
  Mesh mesh=method=="dual"?poisson::extract<true>(f,refine):poisson::extract<false>(f,refine);
  f.seconds.push_back(poisson::seconds_since(clock));
  Rcpp::DataFrame out=mesh.output();
  out.attr("iso")=f.iso;
  out.attr("nodes")=Rcpp::wrap(f.nodes);
  out.attr("iterations")=Rcpp::wrap(f.iterations);
  out.attr("seconds")=Rcpp::wrap(f.seconds);
  return out;
}
