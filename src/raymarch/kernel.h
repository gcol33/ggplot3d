#pragma once
#include "../scene/parallel.h"
#include "../scene/projection.h"
#include "../scene/wire.h"

namespace ggplot3d { namespace raymarch {
using engine::Vec3;
using engine::require;
struct Jet { double v=0; Vec3 d{0,0,0}; };
inline Jet unary(const Jet& a,double v,double slope) {
  Jet r;r.v=v;for(int k=0;k<3;++k) r.d[k]=slope*a.d[k];return r;
}
inline Jet constant(const Jet&,const Jet&,double c) {return {c,{0,0,0}};}
inline Jet xcoord(const Jet& p,const Jet&,double) {return {p.d[0],{1,0,0}};}
inline Jet ycoord(const Jet& p,const Jet&,double) {return {p.d[1],{0,1,0}};}
inline Jet zcoord(const Jet& p,const Jet&,double) {return {p.d[2],{0,0,1}};}
inline Jet add(const Jet& a,const Jet& b,double) {Jet r;r.v=a.v+b.v;for(int k=0;k<3;++k)r.d[k]=a.d[k]+b.d[k];return r;}
inline Jet neg(const Jet& a,const Jet&,double) {return unary(a,-a.v,-1);}
inline Jet sub(const Jet& a,const Jet& b,double) {return add(a,neg(b,b,0),0);}
inline Jet mul(const Jet& a,const Jet& b,double) {Jet r;r.v=a.v*b.v;for(int k=0;k<3;++k)r.d[k]=a.d[k]*b.v+a.v*b.d[k];return r;}
inline Jet divide(const Jet& a,const Jet& b,double) {return mul(a,unary(b,1/b.v,-1/(b.v*b.v)),0);}
inline Jet power(const Jet& a,const Jet& b,double) {
  const double v=std::pow(a.v,b.v);Jet r=unary(a,v,b.v==0 ? 0:b.v*std::pow(a.v,b.v-1));
  for(int k=0;k<3;++k) if(b.d[k]!=0) r.d[k]+=v*std::log(a.v)*b.d[k];
  return r;
}
inline Jet absolute(const Jet& a,const Jet&,double) {return unary(a,std::abs(a.v),(a.v>0)-(a.v<0));}
inline Jet root(const Jet& a,const Jet&,double) {double v=std::sqrt(a.v);return unary(a,v,v>0 ? 0.5/v:0);}
inline Jet sine(const Jet& a,const Jet&,double) {return unary(a,std::sin(a.v),std::cos(a.v));}
inline Jet cosine(const Jet& a,const Jet&,double) {return unary(a,std::cos(a.v),-std::sin(a.v));}
inline Jet exponential(const Jet& a,const Jet&,double) {return unary(a,std::exp(a.v),std::exp(a.v));}
inline Jet logarithm(const Jet& a,const Jet&,double) {return unary(a,std::log(a.v),1/a.v);}
inline Jet minimum(const Jet& a,const Jet& b,double) {return a.v<b.v ? a:b.v<a.v ? b:unary(add(a,b,0),a.v,0.5);}
inline Jet maximum(const Jet& a,const Jet& b,double) {return a.v>b.v ? a:b.v>a.v ? b:unary(add(a,b,0),a.v,0.5);}
using Rule=Jet(*)(const Jet&,const Jet&,double);
struct Operation {const char* name;Rule rule;int arity;};
inline const std::vector<Operation>& operations() {
  static const std::vector<Operation> ops={{"constant",constant,0},{"x",xcoord,0},{"y",ycoord,0},{"z",zcoord,0},
    {"+",add,2},{"-",sub,2},{"*",mul,2},{"/",divide,2},{"^",power,2},{"negate",neg,1},
    {"abs",absolute,1},{"sqrt",root,1},{"sin",sine,1},{"cos",cosine,1},{"exp",exponential,1},
    {"log",logarithm,1},{"min",minimum,2},{"max",maximum,2}};
  return ops;
}
struct Instruction {Rule rule;int a,b;double c;};
struct Tape {
  std::vector<Instruction> code;int output;double lipschitz;
  explicit Tape(const Rcpp::List& input) {
    Rcpp::NumericMatrix m=input["code"];output=Rcpp::as<int>(input["root"]);lipschitz=Rcpp::as<double>(input["lipschitz"]);
    require(m.ncol()==4 && output>=0 && output<m.nrow() && std::isfinite(lipschitz) && lipschitz>0,"Invalid SDF tape");
    const auto& ops=operations();
    for(int i=0;i<m.nrow();++i) {
      int op=m(i,0),a=m(i,1),b=m(i,2);
      require(op>=0 && op<int(ops.size()),"Unknown tape opcode");
      require((ops[op].arity<1 || (a>=0 && a<i)) && (ops[op].arity<2 || (b>=0 && b<i)),"Tape operands must precede instructions");
      code.push_back({ops[op].rule,a,b,m(i,3)});
    }
  }
  Jet evaluate(const Vec3& p,std::vector<Jet>& scratch) const {
    scratch.resize(code.size());Jet coord{0,p};
    for(std::size_t i=0;i<code.size();++i) {
      const auto& c=code[i];scratch[i]=c.rule(c.a<0 ? coord:scratch[c.a],c.b<0 ? coord:scratch[c.b],c.c);
    }
    return scratch[output];
  }
};
inline Vec3 point(const Vec3& o,const Vec3& d,double t) {return {o[0]+d[0]*t,o[1]+d[1]*t,o[2]+d[2]*t};}
inline double norm(const Vec3& p) {return std::sqrt(engine::dot(p,p));}
inline Vec3 unit(Vec3 p) {double n=norm(p);if(n>0) for(double& x:p)x/=n;return p;}
struct Ray {Vec3 origin,direction;double near,far;};
inline Ray camera_ray(const engine::Projection& p,double x,double y) {
  const auto& c=p.resolved;double u=(2*x-1)/p.sx,v=(2*y-1)/p.sy;Ray r;
  for(int k=0;k<3;++k) {
    r.direction[k]=c.ray[k]+(p.camera.perspective ? u*c.right[k]+v*c.up[k]:0);
    r.origin[k]=c.eye[k]+(p.camera.perspective ? 0:u*c.right[k]+v*c.up[k]);
  }
  r.direction=unit(r.direction);double forward=engine::dot(r.direction,c.forward);
  r.near=c.near/forward;r.far=c.far/forward;return r;
}
inline bool clip_box(Ray& r,const Vec3& lo,const Vec3& hi) {
  for(int k=0;k<3;++k) {
    if(r.direction[k]==0) {if(r.origin[k]<lo[k] || r.origin[k]>hi[k])return false;}
    else {double a=(lo[k]-r.origin[k])/r.direction[k],b=(hi[k]-r.origin[k])/r.direction[k];
      r.near=std::max(r.near,std::min(a,b));r.far=std::min(r.far,std::max(a,b));}
  }
  return r.near<=r.far;
}
struct Hit {bool found=false;double t=0;Vec3 normal{0,0,0};int steps=0;};
inline Hit trace(const Tape& tape,const Ray& ray,double tolerance,int max_steps,std::vector<Jet>& scratch) {
  Hit hit;double t=ray.near;
  for(int i=0;i<max_steps && t<=ray.far;++i) {
    Jet f=tape.evaluate(point(ray.origin,ray.direction,t),scratch);
    require(std::isfinite(f.v),"Non-finite SDF evaluation");hit.steps=i+1;
    double step=std::abs(f.v)/tape.lipschitz;
    if(step<=tolerance) {hit.found=true;hit.t=t;hit.normal=unit(f.d);return hit;}
    double next=t+step;if(next==t)break;t=next;
  }
  return hit;
}
} }
