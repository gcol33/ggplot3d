#ifndef GGPLOT3D_GEOMETRY_INTERSECTIONS_H
#define GGPLOT3D_GEOMETRY_INTERSECTIONS_H
#include "vector.h"
#include <numeric>
namespace ggplot3d_geometry {
struct CrossingTriangle {
  std::array<V,3> p;
  std::array<int,3> vertex;
  V lo,hi,normal;
  int parent;
};
struct CrossingSegment {V a,b; int first,second;};

// A triangle cut by a plane is an interval on the common line of the planes.
static inline bool triangle_plane_interval(const CrossingTriangle& t,V point,V normal,
                                           V direction,double tolerance,double& lo,double& hi) {
  double distance[3];
  std::vector<V> points;
  for(int i=0;i<3;++i) {
    distance[i]=dot(sub(t.p[i],point),normal);
    if(std::abs(distance[i])<=tolerance) points.push_back(t.p[i]);
  }
  for(int i=0;i<3;++i) {
    int j=(i+1)%3;
    if((distance[i]<-tolerance&&distance[j]>tolerance)||
       (distance[j]<-tolerance&&distance[i]>tolerance)) {
      double s=distance[i]/(distance[i]-distance[j]);
      points.push_back(add(t.p[i],mul(sub(t.p[j],t.p[i]),s)));
    }
  }
  if(points.empty()) return false;
  lo=hi=dot(points[0],direction);
  for(V p:points) {double v=dot(p,direction); lo=std::min(lo,v); hi=std::max(hi,v);}
  return true;
}

static inline std::vector<CrossingSegment> triangle_crossings(std::vector<CrossingTriangle> triangles,double tolerance) {
  std::map<std::pair<int,int>,int> incidence;
  for(const auto& t:triangles) for(int k=0;k<3;++k) {
    int u=t.vertex[k],v=t.vertex[(k+1)%3];
    ++incidence[{std::min(u,v),std::max(u,v)}];
  }
  std::sort(triangles.begin(),triangles.end(),[](const CrossingTriangle& a,const CrossingTriangle& b) {return a.lo[0]<b.lo[0];});
  std::vector<CrossingSegment> out;
  for(size_t i=0;i<triangles.size();++i) {
    if(i%256==0) Rcpp::checkUserInterrupt();
    const auto& a=triangles[i];
    for(size_t j=i+1;j<triangles.size()&&triangles[j].lo[0]<=a.hi[0]+tolerance;++j) {
      const auto& b=triangles[j];
      if(a.parent==b.parent) continue;
      if(b.lo[1]>a.hi[1]+tolerance||a.lo[1]>b.hi[1]+tolerance||
         b.lo[2]>a.hi[2]+tolerance||a.lo[2]>b.hi[2]+tolerance) continue;
      std::vector<int> shared;
      for(int u:a.vertex) for(int v:b.vertex) if(u==v) shared.push_back(u);
      if(shared.size()==2&&incidence[{std::min(shared[0],shared[1]),std::max(shared[0],shared[1])}]==2) continue;
      V direction=cross(a.normal,b.normal);
      double sine=norm(direction);
      if(sine<=64*std::numeric_limits<double>::epsilon()) continue;
      direction=mul(direction,1/sine);
      double al,ah,bl,bh;
      if(!triangle_plane_interval(a,b.p[0],b.normal,direction,tolerance,al,ah)||
         !triangle_plane_interval(b,a.p[0],a.normal,direction,tolerance,bl,bh)) continue;
      double lo=std::max(al,bl),hi=std::min(ah,bh);
      if(hi-lo<=tolerance) continue;
      double ca=dot(a.normal,a.p[0]),cb=dot(b.normal,b.p[0]);
      V line=cross(a.normal,b.normal);
      V origin=mul(add(mul(cross(b.normal,line),ca),mul(cross(line,a.normal),cb)),1/dot(line,line));
      out.push_back({add(origin,mul(direction,lo)),add(origin,mul(direction,hi)),a.parent,b.parent});
    }
  }
  return out;
}
}
#endif
