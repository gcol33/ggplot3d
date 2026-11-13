#ifndef GGPLOT3D_GEOMETRY_SWEEP_H
#define GGPLOT3D_GEOMETRY_SWEEP_H
#include "vector.h"
namespace ggplot3d_geometry {
namespace sweep {

// Orthonormal frame along a polyline: tangent t, normal r and binormal
// s = t x r at every point, with the arc length from the first point.
struct Frames { std::vector<V> t,r,s; std::vector<double> arc; double length=0; };

// Rotates r about the unit axis t (r perpendicular to t) by angle a.
static inline V rotate(V r,V t,double a) { return add(mul(r,std::cos(a)),mul(cross(t,r),std::sin(a))); }

static inline V perpendicular(V t,V hint) {
  V r=sub(hint,mul(t,dot(hint,t)));
  if(norm(r)>1e-8*std::max(1.0,norm(hint))) return unit(r);
  int a=0; for(int k=1;k<3;++k) if(std::abs(t[k])<std::abs(t[a])) a=k;
  V e{0,0,0}; e[a]=1;
  return unit(sub(e,mul(t,dot(e,t))));
}

// One double-reflection step (Wang, Juettler, Zheng and Liu 2008): reflect
// the frame in the bisector plane of the chord x0 -> x1, then in the plane
// that maps the reflected tangent onto t1.
static inline V reflect(V x0,V x1,V r0,V t0,V t1) {
  V v1=sub(x1,x0); double c1=dot(v1,v1);
  if(!(c1>0)) return r0;
  V rl=sub(r0,mul(v1,2*dot(v1,r0)/c1)),tl=sub(t0,mul(v1,2*dot(v1,t0)/c1));
  V v2=sub(t1,tl); double c2=dot(v2,v2);
  return c2>0?sub(rl,mul(v2,2*dot(v2,rl)/c2)):rl;
}

// Rotation-minimising frames by the double reflection method. Tangents are
// the bisectors of adjacent chords (the chord itself at the ends of an open
// curve). The first normal is hint made perpendicular to the first tangent,
// or the coordinate axis least aligned with it. On a closed curve the frame
// carried once around misses the first frame by an angle; that angle is
// distributed over the points in proportion to arc length, so the frames
// close up.
static inline Frames frames(const std::vector<V>& x,bool closed,V hint) {
  int n=x.size(); Frames f; f.t.resize(n); f.r.resize(n); f.s.resize(n); f.arc.assign(n,0);
  if(n<2) Rcpp::stop("A swept curve needs at least two distinct points.");
  auto chord=[&](int i) { return unit(sub(x[(i+1)%n],x[i])); };
  for(int i=0;i<n;++i) {
    bool first=i==0&&!closed,last=i==n-1&&!closed;
    V before=first?chord(0):chord((i-1+n)%n),after=last?chord(n-2):chord(i);
    V t=unit(add(before,after));
    f.t[i]=norm(t)>0?t:after;
    if(i>0) f.arc[i]=f.arc[i-1]+norm(sub(x[i],x[i-1]));
  }
  f.length=f.arc[n-1]+(closed?norm(sub(x[0],x[n-1])):0);
  f.r[0]=perpendicular(f.t[0],hint);
  for(int i=0;i+1<n;++i) f.r[i+1]=perpendicular(f.t[i+1],reflect(x[i],x[i+1],f.r[i],f.t[i],f.t[i+1]));
  if(closed&&f.length>0) {
    V around=perpendicular(f.t[0],reflect(x[n-1],x[0],f.r[n-1],f.t[n-1],f.t[0]));
    double miss=std::atan2(dot(f.t[0],cross(around,f.r[0])),dot(around,f.r[0]));
    for(int i=0;i<n;++i) f.r[i]=rotate(f.r[i],f.t[i],miss*f.arc[i]/f.length);
  }
  for(int i=0;i<n;++i) f.s[i]=cross(f.t[i],f.r[i]);
  return f;
}
}
}
#endif
