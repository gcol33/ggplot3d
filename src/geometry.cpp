#include "geometry/extract.h"
#include "geometry/octree.h"
#include "geometry/hull.h"
using ggplot3d_geometry::V;

namespace {
Rcpp::NumericMatrix point_matrix(const std::vector<V>& points) {
  int n=points.size(); Rcpp::NumericMatrix p(n,3);
  for(int i=0;i<n;++i) for(int a=0;a<3;++a) p(i,a)=points[i][a];
  return p;
}

std::vector<V> matrix_points(SEXP x,int n,const char* message) {
  Rcpp::NumericMatrix m(x);
  if(m.nrow()!=n||m.ncol()!=3) Rcpp::stop(message);
  std::vector<V> out(n);
  for(int i=0;i<n;++i) for(int a=0;a<3;++a) out[i][a]=m(i,a);
  return out;
}

// Gradient of an R function of an n x 3 matrix returning an n x 3 matrix;
// non-finite components are zero.
ggplot3d_geometry::dual::GradientFunction gradient_function(Rcpp::Nullable<Rcpp::Function> gradient) {
  if(gradient.isNull()) return {};
  Rcpp::Function f(gradient);
  return [f](const std::vector<V>& points,std::vector<V>& out) {
    out=matrix_points(f(point_matrix(points)),points.size(),"The gradient function must return one row per point and three columns.");
    for(V& g:out) for(double& x:g) if(!std::isfinite(x)) x=0;
  };
}
}

// [[Rcpp::export(rng = false)]]
Rcpp::DataFrame geometry_isosurface_cpp(Rcpp::NumericVector x,Rcpp::NumericVector y,Rcpp::NumericVector z,
                                       Rcpp::NumericVector value,Rcpp::NumericVector levels,bool caps,std::string method,
                                       Rcpp::Nullable<Rcpp::Function> gradient = R_NilValue) {
  ggplot3d_geometry::ScalarGrid grid(x,y,z,value);
  return ggplot3d_geometry::extract_isosurface(grid,levels,caps,method,gradient_function(gradient));
}

// Dual contouring on the terminal boxes of an octree over [lo, hi]. boxes
// holds lattice origins and sizes (columns i, j, k, size) on 2^depth
// intervals per axis; empty flags boxes the level set cannot meet. value
// returns the field at an n x 3 matrix of points; crossing(a, b, fa, fb,
// level) the level positions on segments a-b, or NULL for linear
// interpolation; gradient the field gradients at points.
// [[Rcpp::export(rng = false)]]
Rcpp::DataFrame geometry_octree_isosurface_cpp(Rcpp::NumericVector lo,Rcpp::NumericVector hi,int depth,Rcpp::IntegerMatrix boxes,
                                              Rcpp::LogicalVector empty,Rcpp::NumericVector levels,bool caps,std::string method,
                                              Rcpp::Function value,Rcpp::Nullable<Rcpp::Function> crossing = R_NilValue,
                                              Rcpp::Nullable<Rcpp::Function> gradient = R_NilValue) {
  using namespace ggplot3d_geometry::octree;
  if(depth<0||depth>12) Rcpp::stop("Octree depth must lie between 0 and 12.");
  if(lo.size()!=3||hi.size()!=3||boxes.ncol()!=4||empty.size()!=boxes.nrow()) Rcpp::stop("Invalid octree input.");
  for(double level:levels) if(!std::isfinite(level)) Rcpp::stop("Levels must be finite.");
  std::vector<P3> origin(boxes.nrow()); std::vector<int> size(boxes.nrow()); std::vector<char> blank(boxes.nrow());
  for(int i=0;i<boxes.nrow();++i) { origin[i]={boxes(i,0),boxes(i,1),boxes(i,2)}; size[i]=boxes(i,3); blank[i]=empty[i]==TRUE; }
  Tree tree(depth,V{lo[0],lo[1],lo[2]},V{hi[0],hi[1],hi[2]},caps,origin,size);
  ValueFunction field=[value](const std::vector<V>& points,std::vector<double>& out) {
    out=Rcpp::as<std::vector<double>>(value(point_matrix(points)));
  };
  CrossingFunction place;
  if(crossing.isNotNull()) {
    Rcpp::Function f(crossing);
    place=[f](const std::vector<V>& a,const std::vector<V>& b,const std::vector<double>& fa,const std::vector<double>& fb,double level,std::vector<V>& out) {
      Rcpp::NumericMatrix pa=point_matrix(a),pb=point_matrix(b);
      Rcpp::NumericVector va(fa.begin(),fa.end()),vb(fb.begin(),fb.end());
      out=matrix_points(f(pa,pb,va,vb,level),a.size(),
        "The crossing function must return one row per segment and three columns.");
    };
  }
  std::vector<double> l=Rcpp::as<std::vector<double>>(levels);
  ggplot3d_geometry::dual::GradientFunction g=gradient_function(gradient);
  if(method=="dual") return extract<true>(tree,blank,l,field,place,g).output();
  if(method=="nets") return extract<false>(tree,blank,l,field,place,g).output();
  Rcpp::stop("Adaptive extraction supports the \"dual\" and \"nets\" methods.");
}

// [[Rcpp::export(rng = false)]]
Rcpp::CharacterVector geometry_isosurface_methods_cpp() {
  Rcpp::CharacterVector out;
  for(const auto& m:ggplot3d_geometry::isosurface_methods()) out.push_back(m.name);
  return out;
}

// [[Rcpp::export(rng = false)]]
Rcpp::IntegerMatrix geometry_hull_cpp(Rcpp::NumericMatrix xyz) {
  return ggplot3d_geometry::hull(xyz);
}

// [[Rcpp::export(rng = false)]]
Rcpp::NumericVector geometry_kde_cpp(Rcpp::NumericMatrix data,Rcpp::List axes,Rcpp::NumericVector bandwidth) {
  int dim=data.ncol(),n=data.nrow();
  if((dim!=2&&dim!=3)||axes.size()!=dim||bandwidth.size()!=dim||n==0) Rcpp::stop("Invalid KDE dimensions.");
  std::vector<std::vector<double>> grid(dim),kernel(dim); size_t total=1;
  for(int a=0;a<dim;++a) {
    grid[a]=Rcpp::as<std::vector<double>>(axes[a]); kernel[a].resize(grid[a].size()); total*=grid[a].size();
    if(!std::isfinite(bandwidth[a])||bandwidth[a]<=0) Rcpp::stop("Bandwidths must be positive and finite.");
  }
  Rcpp::NumericVector out(total);
  for(int i=0;i<n;++i) {
    if(i%128==0) Rcpp::checkUserInterrupt();
    for(int a=0;a<dim;++a) {
      if(!std::isfinite(data(i,a))) Rcpp::stop("KDE positions must be finite.");
      for(size_t j=0;j<grid[a].size();++j) {double u=(grid[a][j]-data(i,a))/bandwidth[a];kernel[a][j]=std::exp(-u*u/2)/(std::sqrt(2*3.14159265358979323846)*bandwidth[a]);}
    }
    size_t k=0,nz=dim==3?grid[2].size():1;
    for(size_t z=0;z<nz;++z) for(size_t y=0;y<grid[1].size();++y) for(size_t x=0;x<grid[0].size();++x)
      out[k++]+=kernel[0][x]*kernel[1][y]*(dim==3?kernel[2][z]:1)/n;
  }
  return out;
}
