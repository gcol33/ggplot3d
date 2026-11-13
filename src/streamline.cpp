#include "geometry/streamline.h"
using namespace ggplot3d_geometry;

// Trilinear rectilinear vector field, with x varying fastest in values.
// [[Rcpp::export(rng = false)]]
Rcpp::DataFrame streamlines_cpp(Rcpp::List axes,Rcpp::NumericMatrix values,Rcpp::NumericMatrix seeds,
                               double step,int max_steps,std::string method,std::string direction,
                               double tolerance,double separation,double dtest,int slice_axis,int max_lines) {
  if(axes.size()!=3||values.ncol()!=3||seeds.ncol()!=3||!(step>0)||!std::isfinite(step)||
     max_steps<1||max_lines<1||!(tolerance>0)||!std::isfinite(tolerance)||slice_axis< -1||slice_axis>2||
     !(separation>=0)||!(dtest>=0)||!std::isfinite(separation)||!std::isfinite(dtest)||
     (separation>0&&(slice_axis<0||dtest<=0||dtest>separation))) Rcpp::stop("Invalid streamline controls.");
  if(method!="rk4"&&method!="rk45") Rcpp::stop("method must be rk4 or rk45.");
  if(direction!="both"&&direction!="forward"&&direction!="backward") Rcpp::stop("Invalid streamline direction.");
  flow::Field field;field.slice=slice_axis;size_t size=1;
  for(int a=0;a<3;++a) {
    field.axis[a]=Rcpp::as<std::vector<double>>(axes[a]);const auto& x=field.axis[a];
    if(x.size()<2) Rcpp::stop("A vector field needs at least two coordinates per axis.");
    for(size_t i=0;i<x.size();++i) if(!std::isfinite(x[i])||(i&&x[i]<=x[i-1])) Rcpp::stop("Field axes must be finite and strictly increasing.");
    size*=x.size();
  }
  if(size!=static_cast<size_t>(values.nrow())) Rcpp::stop("Vector field size disagrees with its axes.");
  for(int i=0;i<values.nrow();++i) field.value.push_back({values(i,0),values(i,1),values(i,2)});
  std::vector<V> starts;
  for(int i=0;i<seeds.nrow();++i) {
    V p{seeds(i,0),seeds(i,1),seeds(i,2)};
    for(double v:p) if(!std::isfinite(v)) Rcpp::stop("Seeds must be finite.");
    starts.push_back(p);
  }
  auto lines=flow::integrate(field,starts,step,max_steps,method=="rk45",direction=="both"?0:(direction=="forward"?1:-1),
                             tolerance,separation,dtest,max_lines);
  std::vector<double> x,y,z,time,speed,arc;
  std::vector<int> group,index;
  for(size_t g=0;g<lines.size();++g) {
    double distance=0;const auto& line=lines[g];
    for(size_t i=0;i<line.size();++i) {
      if(i) distance+=norm(sub(line[i].p,line[i-1].p));
      x.push_back(line[i].p[0]);y.push_back(line[i].p[1]);z.push_back(line[i].p[2]);
      time.push_back(line[i].time);speed.push_back(line[i].speed);arc.push_back(distance);
      group.push_back(g+1);index.push_back(i);
    }
  }
  return Rcpp::DataFrame::create(Rcpp::Named("x")=x,Rcpp::Named("y")=y,Rcpp::Named("z")=z,
    Rcpp::Named("norm")=speed,Rcpp::Named("step")=index,Rcpp::Named("arclength")=arc,
    Rcpp::Named("time")=time,Rcpp::Named("group")=group);
}
