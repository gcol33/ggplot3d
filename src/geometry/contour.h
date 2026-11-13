#ifndef GGPLOT3D_GEOMETRY_CONTOUR_H
#define GGPLOT3D_GEOMETRY_CONTOUR_H
#include "clip.h"
namespace ggplot3d_geometry {

// Contour line of one level: points in order, the triangle it starts in.
struct Polyline { std::vector<V> points; double level; int triangle; };

// Marching triangles on a triangle mesh with one value per vertex, linear on
// each triangle. A vertex is high when its value is at least the level; the
// level crosses an edge whose ends differ, at the linearly interpolated
// point. Crossings are keyed by the edge's two vertex ids, so triangles that
// share an edge share the crossing exactly. Each crossed triangle gives one
// segment between its two crossed edges, and segments are chained through
// shared crossings into open lines, started at crossings used by one segment
// (mesh boundary), and then into closed loops, which repeat their first
// point. Triangles with a non-finite value are skipped; repeated points are
// dropped.
static inline std::vector<Polyline> contour_mesh(const std::vector<V>& points,const std::vector<std::array<int,3>>& triangles,
                                                 const std::vector<double>& values,const std::vector<double>& levels) {
  std::vector<Polyline> out;
  for(double level:levels) {
    std::unordered_map<long long,int> key;
    std::vector<V> crossing;
    std::vector<std::array<int,2>> segments; std::vector<int> source;
    auto at=[&](int a,int b) {
      long long k=pair_key(a,b);
      auto found=key.find(k);
      if(found!=key.end()) return found->second;
      if(a>b) std::swap(a,b);
      double t=(level-values[a])/(values[b]-values[a]);
      crossing.push_back(add(points[a],mul(sub(points[b],points[a]),t)));
      key[k]=crossing.size()-1;
      return static_cast<int>(crossing.size())-1;
    };
    for(size_t f=0;f<triangles.size();++f) {
      const auto& t=triangles[f];
      if(!std::isfinite(values[t[0]])||!std::isfinite(values[t[1]])||!std::isfinite(values[t[2]])) continue;
      int ends[2],m=0;
      for(int e=0;e<3;++e) {
        int a=t[e],b=t[(e+1)%3];
        if((values[a]>=level)!=(values[b]>=level)) ends[m++]=at(a,b);
      }
      if(m==2) { segments.push_back({ends[0],ends[1]}); source.push_back(f); }
    }
    int nc=crossing.size(),ns=segments.size();
    std::vector<std::vector<int>> incident(nc);
    for(int s=0;s<ns;++s) for(int c:segments[s]) incident[c].push_back(s);
    std::vector<char> used(ns,0);
    auto trace=[&](int start) {
      int current=start,first=-1;
      std::vector<V> line{crossing[start]};
      while(true) {
        int next=-1;
        for(int s:incident[current]) if(!used[s]) { next=s; break; }
        if(next<0) break;
        if(first<0) first=next;
        used[next]=1;
        current=segments[next][0]==current?segments[next][1]:segments[next][0];
        if(norm(sub(crossing[current],line.back()))>0) line.push_back(crossing[current]);
      }
      if(line.size()>=2) out.push_back({line,level,source[first]});
    };
    for(int c=0;c<nc;++c) if(incident[c].size()==1) for(int s:incident[c]) if(!used[s]) trace(c);
    for(int s=0;s<ns;++s) if(!used[s]) trace(segments[s][0]);
  }
  return out;
}
}
#endif
