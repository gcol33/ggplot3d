#include "geometry/alpha.h"
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
