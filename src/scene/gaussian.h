#pragma once
#include "projection.h"
#include "parallel.h"

namespace ggplot3d { namespace gaussian {
using engine::Vec3;
using Matrix=std::array<double,9>;
using Covariance=std::array<double,6>;
static inline Vec3 multiply(const Matrix& a,const Vec3& x) {
  Vec3 y{};for(int i=0;i<3;++i)for(int j=0;j<3;++j)y[i]+=a[3*i+j]*x[j];return y;
}
static inline Matrix matrix(const Covariance& c) {return {c[0],c[1],c[2],c[1],c[3],c[4],c[2],c[4],c[5]};}
static inline Covariance transform(const Covariance& c,const Matrix& a) {
  Matrix m=matrix(c),out{};
  for(int i=0;i<3;++i)for(int j=i;j<3;++j)
    for(int k=0;k<3;++k)for(int l=0;l<3;++l)out[3*i+j]+=a[3*i+k]*m[3*k+l]*a[3*j+l];
  return {out[0],out[1],out[2],out[4],out[5],out[8]};
}
static inline Matrix inverse(const Matrix& a) {
  Matrix b{a[4]*a[8]-a[5]*a[7],a[2]*a[7]-a[1]*a[8],a[1]*a[5]-a[2]*a[4],
    a[5]*a[6]-a[3]*a[8],a[0]*a[8]-a[2]*a[6],a[2]*a[3]-a[0]*a[5],
    a[3]*a[7]-a[4]*a[6],a[1]*a[6]-a[0]*a[7],a[0]*a[4]-a[1]*a[3]};
  double d=a[0]*b[0]+a[1]*b[3]+a[2]*b[6];
  engine::require(std::isfinite(d) && d!=0,"Singular Gaussian affine transform");
  for(auto& x:b)x/=d;
  return b;
}
static inline Covariance covariance(const Vec3& s,std::array<double,4> q,const Vec3& stretch) {
  double magnitude=0;for(double x:q){engine::require(std::isfinite(x),"Gaussian quaternion must be finite and nonzero");magnitude=std::max(magnitude,std::abs(x));}
  engine::require(magnitude>0,"Gaussian quaternion must be finite and nonzero");
  double norm=0;for(double& x:q){x/=magnitude;norm+=x*x;}
  for(double& x:q)x/=std::sqrt(norm);
  double w=q[0],x=q[1],y=q[2],z=q[3];
  Matrix rotation{1-2*(y*y+z*z),2*(x*y-w*z),2*(x*z+w*y),
    2*(x*y+w*z),1-2*(x*x+z*z),2*(y*z-w*x),2*(x*z-w*y),2*(y*z+w*x),1-2*(x*x+y*y)};
  for(int i=0;i<3;++i){engine::require(std::isfinite(s[i]) && s[i]>0,"Gaussian scales must be finite and positive");
    for(int j=0;j<3;++j)rotation[3*i+j]*=stretch[i];}
  return transform({s[0]*s[0],0,0,s[1]*s[1],0,s[2]*s[2]},rotation);
}
static inline std::array<double,16> harmonics(Vec3 d) {
  double norm=std::sqrt(engine::dot(d,d));if(norm>0)for(auto& v:d)v/=norm;
  double x=d[0],y=d[1],z=d[2],xx=x*x,yy=y*y,zz=z*z;
  return {0.28209479177387814,-0.4886025119029199*y,0.4886025119029199*z,-0.4886025119029199*x,
    1.0925484305920792*x*y,-1.0925484305920792*y*z,0.31539156525252005*(2*zz-xx-yy),
    -1.0925484305920792*x*z,0.5462742152960396*(xx-yy),
    -0.5900435899266435*y*(3*xx-yy),2.890611442640554*x*y*z,-0.4570457994644658*y*(4*zz-xx-yy),
    0.3731763325901154*z*(2*zz-3*xx-3*yy),-0.4570457994644658*x*(4*zz-xx-yy),
    1.445305721320277*z*(xx-yy),-0.5900435899266435*x*(xx-3*yy)};
}
struct Gaussian {Vec3 centre;Covariance covariance;std::array<double,4> rgba;};
static inline std::vector<Gaussian> read(const Rcpp::List& input,const engine::Projection& p) {
  Rcpp::NumericMatrix position=input["position"],cov=input["covariance"],rgba=input["rgba"],sh=input["sh"];
  Rcpp::NumericVector span=input["span"];
  int n=position.nrow(),coeff=sh.ncol()/3;
  engine::require(position.ncol()==3 && cov.nrow()==n && cov.ncol()==6 && rgba.nrow()==n && rgba.ncol()==4 &&
    sh.nrow()==n && span.size()==3 && (coeff==0 || coeff==1 || coeff==4 || coeff==9 || coeff==16) && sh.ncol()==3*coeff,
    "Invalid Gaussian arrays");
  const auto& pre=p.camera.pre;
  engine::require(pre[3]==0 && pre[7]==0 && pre[11]==0 && pre[15]==1,"Gaussian splats require affine camera pre");
  Matrix affine;for(int i=0;i<3;++i)for(int j=0;j<3;++j)affine[3*i+j]=p.scale[i]*pre[4*j+i];
  Matrix back=inverse(affine);std::vector<Gaussian> out(n);
  for(int i=0;i<n;++i){auto& g=out[i];Vec3 original{position(i,0),position(i,1),position(i,2)};
    g.centre=multiply(affine,original);for(int k=0;k<3;++k)g.centre[k]+=p.scale[k]*pre[12+k];
    Covariance c;for(int k=0;k<6;++k)c[k]=cov(i,k);g.covariance=transform(c,affine);
    for(int k=0;k<4;++k)g.rgba[k]=rgba(i,k);
    if(coeff){Vec3 direction=p.camera.perspective ? engine::sub(g.centre,p.resolved.eye):p.resolved.ray;
      direction=multiply(back,direction);for(int k=0;k<3;++k)direction[k]*=span[k];
      auto basis=harmonics(direction);for(int k=0;k<3;++k){double v=.5;for(int j=0;j<coeff;++j)v+=basis[j]*sh(i,k*coeff+j);g.rgba[k]=std::clamp(v,0.0,1.0);}}
    for(double v:g.rgba)engine::require(std::isfinite(v) && v>=0 && v<=1,"Invalid Gaussian colour or alpha");
  }return out;
}
struct Projected {double x=0,y=0,z=0,xx=0,xy=0,yy=0;};
static inline Projected project(const Gaussian& g,const engine::Projection& p,double pixel_x,double pixel_y) {
  Vec3 delta=engine::sub(g.centre,p.resolved.eye);
  double x=engine::dot(delta,p.resolved.screen_right),y=engine::dot(delta,p.resolved.screen_up),z=engine::dot(delta,p.resolved.forward);
  double w=p.camera.perspective ? z:1,fx=.5*p.sx*p.viewport.width_mm/pixel_x,fy=.5*p.sy*p.viewport.height_mm/pixel_y;
  Matrix jacobian{};
  for(int k=0;k<3;++k){jacobian[k]=fx*(p.resolved.screen_right[k]/w-(p.camera.perspective ? x*p.resolved.forward[k]/(w*w):0));
    jacobian[3+k]=fy*(p.resolved.screen_up[k]/w-(p.camera.perspective ? y*p.resolved.forward[k]/(w*w):0));}
  auto c=transform(g.covariance,jacobian);
  return {p.viewport.width_mm/(2*pixel_x)+fx*x/w,p.viewport.height_mm/(2*pixel_y)+fy*y/w,z,c[0]+.3,c[1],c[3]+.3};
}
} }
