#include "geometry/isosurface.h"
#include "geometry/hull.h"

// [[Rcpp::export(rng = false)]]
Rcpp::DataFrame geometry_isosurface_cpp(Rcpp::NumericVector x,Rcpp::NumericVector y,Rcpp::NumericVector z,
                                       Rcpp::NumericVector value,Rcpp::NumericVector levels,bool caps) {
  return ggplot3d_geometry::Field(x,y,z,value).extract(levels,caps);
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
