#include "geometry/outline.h"
using namespace ggplot3d_geometry;

// Feature lines of a triangle mesh: xyz the drawn positions, view the same
// vertices in camera model space, normals optional vertex normals in that
// space (zero columns for none), tri one-based triangles, eye the camera
// position (perspective) or viewing direction, silhouette 0 (none), 1
// (mesh edges) or 2 (interpolated contour), crease the cosine of the
// crease angle or NA. Returns the polyline points with their one-based line
// and the one-based vertex each point takes its source from.
// [[Rcpp::export(rng = false)]]
Rcpp::List outline_lines_cpp(Rcpp::NumericMatrix xyz,Rcpp::NumericMatrix view,Rcpp::NumericMatrix normals,
                             Rcpp::IntegerMatrix tri,Rcpp::NumericVector eye,bool perspective,int silhouette,
                             double crease,bool boundary) {
  if(xyz.ncol()!=3||view.ncol()!=3||view.nrow()!=xyz.nrow()||tri.ncol()!=3||eye.size()!=3||
     (normals.ncol()!=0&&(normals.ncol()!=3||normals.nrow()!=xyz.nrow()))) Rcpp::stop("Invalid outline arrays.");
  std::vector<outline::Tri> t(tri.nrow());
  for(int f=0;f<tri.nrow();++f) for(int c=0;c<3;++c) {
    int v=tri(f,c)-1; if(v<0||v>=xyz.nrow()) Rcpp::stop("Outline triangles reference missing vertices.");
    t[f][c]=v;
  }
  auto lines=outline::feature_lines(matrix_points(xyz),matrix_points(view),normals.ncol()?matrix_points(normals):std::vector<V>(),t,
    V{eye[0],eye[1],eye[2]},perspective,silhouette,crease,boundary);
  int n=lines.p.size(); Rcpp::NumericVector x(n),y(n),z(n); Rcpp::IntegerVector line(n),vertex(n);
  for(int i=0;i<n;++i) { x[i]=lines.p[i][0]; y[i]=lines.p[i][1]; z[i]=lines.p[i][2]; line[i]=lines.line[i]+1; vertex[i]=lines.vertex[i]+1; }
  return Rcpp::List::create(Rcpp::Named("x")=x,Rcpp::Named("y")=y,Rcpp::Named("z")=z,
    Rcpp::Named("line")=line,Rcpp::Named("vertex")=vertex);
}
