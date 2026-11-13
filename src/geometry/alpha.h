#ifndef GGPLOT3D_GEOMETRY_ALPHA_H
#define GGPLOT3D_GEOMETRY_ALPHA_H
#include "delaunay.h"
namespace ggplot3d_geometry {
namespace alpha {

// Circumradius of a tetrahedron; infinite when it is flat.
static inline double tetrahedron_radius(V a,V b,V c,V d) {
  V u=sub(b,a),v=sub(c,a),w=sub(d,a);
  double det=dot(u,cross(v,w));
  if(det==0) return std::numeric_limits<double>::infinity();
  V centre=mul(add(add(mul(cross(v,w),dot(u,u)),mul(cross(w,u),dot(v,v))),mul(cross(u,v),dot(w,w))),0.5/det);
  return norm(centre);
}

// Circumcentre (relative to c) and circumradius of a triangle.
static inline double triangle_circle(V a,V b,V c,V& centre) {
  V u=sub(a,c),v=sub(b,c),n=cross(u,v);
  double nn=dot(n,n);
  if(nn==0) { centre=c; return std::numeric_limits<double>::infinity(); }
  centre=add(c,mul(cross(sub(mul(v,dot(u,u)),mul(u,dot(v,v))),n),0.5/nn));
  return norm(sub(a,centre));
}

// Faces of the alpha complex of points (Edelsbrunner and Mucke 1994) that
// bound it: a Delaunay tetrahedron belongs to the complex when its
// circumradius is below alpha, and a Delaunay triangle when it is a face of
// such a tetrahedron, or when its circumradius is below alpha and its
// smallest circumsphere holds no point strictly inside (no vertex opposite it
// in an adjacent tetrahedron lies inside). Triangles in the complex with at
// most one adjacent tetrahedron in it are returned; a triangle of one
// tetrahedron is wound with its normal pointing out of it. Columns: three
// vertex indices (0-based) and whether the triangle bounds a tetrahedron.
static inline std::vector<std::array<int,4>> faces(const Delaunay<3>& dt,double alpha) {
  int ns=dt.vertex.size();
  std::vector<double> radius(ns,std::numeric_limits<double>::infinity());
  for(int s=0;s<ns;++s) if(dt.alive[s]&&!dt.ghost(s)) {
    const auto& v=dt.vertex[s];
    radius[s]=tetrahedron_radius(dt.points[v[0]],dt.points[v[1]],dt.points[v[2]],dt.points[v[3]]);
  }
  auto solid=[&](int s) { return dt.alive[s]&&!dt.ghost(s)&&radius[s]<alpha; };
  std::vector<std::array<int,4>> out;
  for(int s=0;s<ns;++s) if(dt.alive[s]&&!dt.ghost(s)) for(int j=0;j<4;++j) {
    int u=dt.neighbour[s][j];
    if(!dt.ghost(u)&&u<s) continue;
    bool in_s=solid(s),in_u=solid(u);
    if(in_s&&in_u) continue;
    const auto& v=dt.vertex[s];
    std::array<int,3> f; int k=0;
    for(int i=0;i<4;++i) if(i!=j) f[k++]=v[i];
    if((3-j)%2==1) std::swap(f[0],f[1]);
    if(!in_s&&!in_u) {
      V centre; double r=triangle_circle(dt.points[f[0]],dt.points[f[1]],dt.points[f[2]],centre);
      if(!(r<alpha)) continue;
      bool attached=norm(sub(dt.points[v[j]],centre))<r;
      if(!dt.ghost(u)) for(int w:dt.vertex[u]) if(w!=f[0]&&w!=f[1]&&w!=f[2]&&norm(sub(dt.points[w],centre))<r) attached=true;
      if(attached) continue;
      out.push_back({f[0],f[1],f[2],0});
    } else if(in_s) out.push_back({f[0],f[1],f[2],1});
    else out.push_back({f[1],f[0],f[2],1});
  }
  return out;
}

// Smallest alpha at which every triangulated point lies on a tetrahedron of
// the complex: the largest, over points, of the smallest circumradius of the
// tetrahedra around each point.
static inline double covering_alpha(const Delaunay<3>& dt) {
  std::vector<double> smallest(dt.points.size(),std::numeric_limits<double>::infinity());
  for(size_t s=0;s<dt.vertex.size();++s) if(dt.alive[s]&&!dt.ghost(s)) {
    const auto& v=dt.vertex[s];
    double r=tetrahedron_radius(dt.points[v[0]],dt.points[v[1]],dt.points[v[2]],dt.points[v[3]]);
    for(int w:v) smallest[w]=std::min(smallest[w],r);
  }
  double alpha=0;
  for(double r:smallest) if(std::isfinite(r)) alpha=std::max(alpha,r);
  return alpha;
}
}
}
#endif
