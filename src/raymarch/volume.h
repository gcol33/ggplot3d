#pragma once
#include "kernel.h"

namespace ggplot3d { namespace raymarch {
struct Volume {
  std::array<std::vector<double>,3> axes;
  std::vector<double> values,knots,sigma;
  std::vector<Vec3> colours;
  Vec3 lo,hi;double level;int mode,samples;
  explicit Volume(const Rcpp::List& input) {
    Rcpp::List a=input["axes"];std::size_t count=1;
    for(int k=0;k<3;++k) {
      axes[k]=Rcpp::as<std::vector<double>>(a[k]);
      require(axes[k].size()>=2,"Volume axes need at least two coordinates");
      for(std::size_t i=0;i<axes[k].size();++i)
        require(std::isfinite(axes[k][i]) && (i==0 || axes[k][i]>axes[k][i-1]),"Volume axes must increase strictly");
      count*=axes[k].size();lo[k]=axes[k].front();hi[k]=axes[k].back();
    }
    values=Rcpp::as<std::vector<double>>(input["value"]);
    require(values.size()==count,"Volume dimensions do not match values");
    for(double v:values)require(std::isfinite(v),"Volume values must be finite");
    knots=Rcpp::as<std::vector<double>>(input["knots"]);sigma=Rcpp::as<std::vector<double>>(input["sigma"]);
    Rcpp::NumericMatrix rgb=input["colours"];
    require(knots.size()>=2 && sigma.size()==knots.size() && rgb.nrow()==int(knots.size()) && rgb.ncol()==3,"Invalid volume transfer table");
    for(std::size_t i=0;i<knots.size();++i) {
      require(std::isfinite(knots[i]) && (i==0 || knots[i]>knots[i-1]) && std::isfinite(sigma[i]) && sigma[i]>=0,"Invalid volume transfer values");
      colours.push_back({rgb(i,0),rgb(i,1),rgb(i,2)});
    }
    level=Rcpp::as<double>(input["level"]);mode=Rcpp::as<int>(input["mode"]);samples=Rcpp::as<int>(input["samples"]);
    require(samples>0 && mode>=0 && mode<=2 && std::isfinite(level),"Invalid volume controls");
  }
  bool contains(const Vec3& p) const {for(int k=0;k<3;++k)if(p[k]<lo[k] || p[k]>hi[k])return false;return true;}
  Jet evaluate(const Vec3& p) const {
    std::array<int,3> cell;Vec3 u,h;
    for(int k=0;k<3;++k) {
      cell[k]=std::max(0,std::min(int(axes[k].size())-2,int(std::upper_bound(axes[k].begin(),axes[k].end(),p[k])-axes[k].begin())-1));
      h[k]=axes[k][cell[k]+1]-axes[k][cell[k]];u[k]=(p[k]-axes[k][cell[k]])/h[k];
    }
    Jet r;
    for(int z=0;z<2;++z)for(int y=0;y<2;++y)for(int x=0;x<2;++x) {
      int corner[3]={x,y,z};Vec3 w{x ? u[0]:1-u[0],y ? u[1]:1-u[1],z ? u[2]:1-u[2]};
      double v=values[cell[0]+x+axes[0].size()*(cell[1]+y+axes[1].size()*(cell[2]+z))];
      r.v+=v*w[0]*w[1]*w[2];
      for(int k=0;k<3;++k) r.d[k]+=v*(corner[k] ? 1:-1)/h[k]*w[(k+1)%3]*w[(k+2)%3];
    }
    return r;
  }
  std::pair<Vec3,double> transfer(double v) const {
    int i=std::max(0,std::min(int(knots.size())-2,int(std::upper_bound(knots.begin(),knots.end(),v)-knots.begin())-1));
    double u=std::max(0.0,std::min(1.0,(v-knots[i])/(knots[i+1]-knots[i])));
    return {point(colours[i],engine::subtract(colours[i+1],colours[i]),u),(1-u)*sigma[i]+u*sigma[i+1]};
  }
  std::vector<double> breaks(const Ray& ray) const {
    Ray r=ray;if(!clip_box(r,lo,hi))return {};
    std::vector<double> out{r.near,r.far};
    for(int k=0;k<3;++k)if(r.direction[k]!=0)for(double a:axes[k]) {
      double t=(a-r.origin[k])/r.direction[k];if(t>r.near && t<r.far)out.push_back(t);
    }
    std::sort(out.begin(),out.end());out.erase(std::unique(out.begin(),out.end()),out.end());return out;
  }
};

// Trilinear interpolation restricted to a line is cubic on each grid cell.
struct Cubic {
  std::array<double,4> c;
  double operator()(double u) const {return ((c[3]*u+c[2])*u+c[1])*u+c[0];}
  std::vector<double> stationary() const {
    std::vector<double> out{0,1};double a=3*c[3],b=2*c[2],d=c[1];
    auto keep=[&](double u){if(u>0 && u<1)out.push_back(u);};
    if(std::abs(a)<=64*std::numeric_limits<double>::epsilon()*std::max(std::abs(b),std::abs(d))) {if(b!=0)keep(-d/b);}
    else {double disc=b*b-4*a*d;if(disc>=0) {double q=-0.5*(b+std::copysign(std::sqrt(disc),b));
      if(q!=0){keep(q/a);keep(d/q);}else keep(-b/(2*a));}}
    std::sort(out.begin(),out.end());return out;
  }
};
inline Cubic cell_polynomial(const Volume& volume,const Ray& ray,double a,double b) {
  double f[4];for(int k=0;k<4;++k)f[k]=volume.evaluate(point(ray.origin,ray.direction,a+(b-a)*k/3)).v;
  double d1=f[1]-f[0],d2=f[2]-2*f[1]+f[0],d3=f[3]-3*f[2]+3*f[1]-f[0];
  return {{f[0],3*d1-1.5*d2+d3,4.5*d2-4.5*d3,4.5*d3}};
}
struct VolumeHit {Hit hit;double value=-std::numeric_limits<double>::infinity();};
inline VolumeHit volume_surface(const Volume& v,const Ray& ray) {
  VolumeHit out;auto breaks=v.breaks(ray);
  for(std::size_t i=1;i<breaks.size();++i) {
    double a=breaks[i-1],b=breaks[i];Cubic p=cell_polynomial(v,ray,a,b);auto stops=p.stationary();
    if(v.mode==1) {
      for(double u:stops)if(p(u)>out.value) {out.value=p(u);out.hit.found=true;out.hit.t=a+u*(b-a);}
    } else {
      double scale=std::abs(v.level);
      for(double c:p.c)scale=std::max(scale,std::abs(c));
      double epsilon=64*std::numeric_limits<double>::epsilon()*scale;
      for(std::size_t k=0;k<stops.size();++k) {
        double left=stops[k],fl=p(left)-v.level;
        if(std::abs(fl)>epsilon) {
          if(k+1==stops.size())continue;
          double right=stops[k+1],fr=p(right)-v.level;
          if(fl*fr>0)continue;
          for(int iteration=0;iteration<52;++iteration) {
            double mid=(left+right)/2,fm=p(mid)-v.level;
            if(fl*fm<=0) {right=mid;fr=fm;}else {left=mid;fl=fm;}
          }
          left=(left+right)/2;
        }
        out.hit.found=true;out.hit.t=a+left*(b-a);out.value=v.level;
        out.hit.normal=unit(v.evaluate(point(ray.origin,ray.direction,out.hit.t)).d);return out;
      }
    }
  }
  return out;
}
struct Medium {Vec3 colour{0,0,0};double alpha=0,entry=std::numeric_limits<double>::infinity();};
inline Medium absorb(const std::vector<Volume>& volumes,const Ray& ray) {
  Medium out;std::vector<double> breaks;double step=std::numeric_limits<double>::infinity();
  for(const auto& v:volumes) {
    auto b=v.breaks(ray);breaks.insert(breaks.end(),b.begin(),b.end());
    step=std::min(step,norm(engine::subtract(v.hi,v.lo))/v.samples);
  }
  std::sort(breaks.begin(),breaks.end());breaks.erase(std::unique(breaks.begin(),breaks.end()),breaks.end());
  for(std::size_t i=1;i<breaks.size();++i) {
    Vec3 middle=point(ray.origin,ray.direction,(breaks[i-1]+breaks[i])/2);
    bool occupied=false;for(const auto& v:volumes)occupied=occupied || v.contains(middle);
    if(!occupied)continue;
    int count=std::max(1,int(std::ceil((breaks[i]-breaks[i-1])/step)));double dt=(breaks[i]-breaks[i-1])/count;
    for(int s=0;s<count;++s) {
      Vec3 p=point(ray.origin,ray.direction,breaks[i-1]+(s+0.5)*dt),emission{0,0,0};double sigma=0;
      for(const auto& v:volumes)if(v.contains(p)) {
        auto tr=v.transfer(v.evaluate(p).v);sigma+=tr.second;
        for(int k=0;k<3;++k)emission[k]+=tr.first[k]*tr.second;
      }
      if(sigma==0)continue;
      if(!std::isfinite(out.entry))out.entry=breaks[i-1]+s*dt;
      double alpha=-std::expm1(-sigma*dt),weight=(1-out.alpha)*alpha;
      for(int k=0;k<3;++k)out.colour[k]+=weight*emission[k]/sigma;
      out.alpha+=weight;if(1-out.alpha<1e-8)return out;
    }
  }
  return out;
}
} }
