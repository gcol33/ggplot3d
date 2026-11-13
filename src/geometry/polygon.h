#ifndef GGPLOT3D_GEOMETRY_POLYGON_H
#define GGPLOT3D_GEOMETRY_POLYGON_H
#include <Rcpp.h>
#include "earcut.h"
namespace ggplot3d_geometry { namespace polygon {

struct Plane2 {
  const std::vector<double>& x;
  const std::vector<double>& y;
  double tolerance;
};
using Ring = std::vector<int>;

static inline double cross2(double ax,double ay,double bx,double by) { return ax*by-ay*bx; }
static inline int sign(double v) { return (v>0)-(v<0); }

static inline bool on_segment(double px,double py,double ux,double uy,double vx,double vy,double t) {
  return px>=std::min(ux,vx)-t&&px<=std::max(ux,vx)+t&&py>=std::min(uy,vy)-t&&py<=std::max(uy,vy)+t;
}

static inline bool edges_cross(const Plane2& p,int a,int b,int c,int d) {
  double ax=p.x[a],ay=p.y[a],bx=p.x[b],by=p.y[b],cx=p.x[c],cy=p.y[c],dx=p.x[d],dy=p.y[d],t=p.tolerance;
  double ab1=cross2(bx-ax,by-ay,cx-ax,cy-ay),ab2=cross2(bx-ax,by-ay,dx-ax,dy-ay);
  double cd1=cross2(dx-cx,dy-cy,ax-cx,ay-cy),cd2=cross2(dx-cx,dy-cy,bx-cx,by-cy);
  if(sign(ab1)*sign(ab2)<0&&sign(cd1)*sign(cd2)<0&&std::abs(ab1)>t&&std::abs(ab2)>t&&std::abs(cd1)>t&&std::abs(cd2)>t) return true;
  return (std::abs(ab1)<=t&&on_segment(cx,cy,ax,ay,bx,by,t))||(std::abs(ab2)<=t&&on_segment(dx,dy,ax,ay,bx,by,t))||
    (std::abs(cd1)<=t&&on_segment(ax,ay,cx,cy,dx,dy,t))||(std::abs(cd2)<=t&&on_segment(bx,by,cx,cy,dx,dy,t));
}

static inline bool rings_intersect(const Plane2& p,const std::vector<Ring>& rings) {
  for(size_t i=0;i<rings.size();++i) for(size_t j=i;j<rings.size();++j) {
    const Ring& a=rings[i]; const Ring& b=rings[j];
    for(size_t u=0;u<a.size();++u) for(size_t v=0;v<b.size();++v) {
      int e0=a[u],e1=a[(u+1)%a.size()],f0=b[v],f1=b[(v+1)%b.size()];
      if(i==j&&(e0==f0||e0==f1||e1==f0||e1==f1)) continue;
      if(edges_cross(p,e0,e1,f0,f1)) return true;
    }
  }
  return false;
}

// Strict polygons have simple nonintersecting boundaries with nonzero area.
// All polygons use the shared even-odd triangulator.
static inline std::vector<std::array<int,3>> triangulate(const Plane2& p,const std::vector<Ring>& rings,bool strict) {
  if(strict&&rings_intersect(p,rings)) Rcpp::stop("Polygon rings must be simple and nonintersecting.");
  std::vector<Ring> kept;
  for(const Ring& r:rings) {
    if(std::abs(earcut::ring_signed_area(p.x,p.y,r))>p.tolerance) kept.push_back(r);
    else if(strict) Rcpp::stop("Polygon rings must have nonzero area.");
  }
  return earcut::triangulate_rings(p.x,p.y,kept);
}

} }
#endif
