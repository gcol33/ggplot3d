#ifndef GGPLOT3D_GEOMETRY_OUTLINE_H
#define GGPLOT3D_GEOMETRY_OUTLINE_H
#include "vector.h"
#include <unordered_map>
namespace ggplot3d_geometry {
namespace outline {

using Tri=std::array<int,3>;
using Segment=std::array<int,2>;

// Joins segments between nodes into polylines. A chain runs through nodes
// met by exactly two segments and ends at nodes of any other degree; the
// segments left over form closed loops, whose first node is repeated at
// the end.
static inline std::vector<std::vector<int>> chains(int nodes,const std::vector<Segment>& seg) {
  std::vector<std::vector<int>> at(nodes);
  for(size_t s=0;s<seg.size();++s) { at[seg[s][0]].push_back(s); at[seg[s][1]].push_back(s); }
  std::vector<char> used(seg.size(),0); std::vector<std::vector<int>> out;
  auto walk=[&](int start,int s) {
    std::vector<int> line{start}; int node=start;
    while(s>=0&&!used[s]) {
      used[s]=1; node=seg[s][0]==node?seg[s][1]:seg[s][0]; line.push_back(node);
      int next=-1;
      if(at[node].size()==2) for(int t:at[node]) if(!used[t]) next=t;
      s=next;
    }
    out.push_back(line);
  };
  for(int v=0;v<nodes;++v) if(at[v].size()!=2) for(int s:at[v]) if(!used[s]) walk(v,s);
  for(size_t s=0;s<seg.size();++s) if(!used[s]) walk(seg[s][0],s);
  return out;
}

struct Lines { std::vector<V> p; std::vector<int> line,vertex; };

// Feature lines of a triangle mesh seen from a camera (Benard and Hertzmann
// 2019, sections 3-4). xyz are the positions the lines are drawn in and
// view the same vertices in the camera's model space, where each vertex is
// front facing when its normal n has n . (eye - p) > 0 under perspective,
// or n . eye < 0 when eye holds the orthographic viewing direction.
// Edges come from the triangles: an edge with one triangle (or more than
// two) is a boundary, one whose triangles' normals differ by more than the
// crease angle (crease_cos its cosine) is a crease, and with
// silhouette = 1 one whose triangles face opposite ways is a silhouette;
// the normals of triangles wound against each other are reversed first.
// With silhouette = 2 the silhouette is instead the interpolated contour,
// the zero set of n . (eye - p) from vertex normals, linear along each
// edge (Hertzmann and Zorin 2000); without given normals the vertex
// normals are the area-weighted face normals.
static inline Lines feature_lines(const std::vector<V>& xyz,const std::vector<V>& view,std::vector<V> normals,
                                  const std::vector<Tri>& tri,V eye,bool perspective,int silhouette,double crease_cos,bool boundary) {
  int n=xyz.size();
  std::vector<V> fn(tri.size());
  for(size_t f=0;f<tri.size();++f) fn[f]=cross(sub(view[tri[f][1]],view[tri[f][0]]),sub(view[tri[f][2]],view[tri[f][0]]));
  auto facing=[&](V normal,V at) { return perspective?dot(normal,sub(eye,at)):-dot(normal,eye); };

  struct Edge { int a,b,count=0; int f[2]={-1,-1}; bool forward[2]={false,false}; };
  std::unordered_map<long long,int> ids; std::vector<Edge> edges;
  for(size_t f=0;f<tri.size();++f) for(int c=0;c<3;++c) {
    int a=tri[f][c],b=tri[f][(c+1)%3]; if(a==b) continue;
    long long key=a<b?static_cast<long long>(a)*n+b:static_cast<long long>(b)*n+a;
    auto found=ids.find(key); int e;
    if(found==ids.end()) { e=edges.size(); ids[key]=e; Edge x; x.a=std::min(a,b); x.b=std::max(a,b); edges.push_back(x); } else e=found->second;
    Edge& x=edges[e];
    if(x.count<2) { x.f[x.count]=f; x.forward[x.count]=a==x.a; }
    ++x.count;
  }

  Lines out; int line=0;
  auto emit=[&](const std::vector<std::vector<int>>& parts,auto point,auto vertex) {
    for(const auto& part:parts) {
      if(part.size()<2) continue;
      for(int node:part) { out.p.push_back(point(node)); out.line.push_back(line); out.vertex.push_back(vertex(node)); }
      ++line;
    }
  };

  std::vector<Segment> marked;
  for(const Edge& x:edges) {
    bool keep=false;
    if(x.count!=2) keep=boundary;
    else {
      V n0=fn[x.f[0]],n1=fn[x.f[1]];
      if(x.forward[0]==x.forward[1]) n1=mul(n1,-1);
      if(!std::isnan(crease_cos)&&norm(n0)>0&&norm(n1)>0&&dot(unit(n0),unit(n1))<crease_cos) keep=true;
      if(silhouette==1) {
        V at=view[x.a];
        double s0=facing(n0,at),s1=facing(n1,at);
        if((s0>0)!=(s1>0)) keep=true;
      }
    }
    if(keep) marked.push_back({x.a,x.b});
  }
  emit(chains(n,marked),[&](int v) { return xyz[v]; },[&](int v) { return v; });

  if(silhouette==2) {
    if(normals.empty()) {
      normals.assign(n,V{0,0,0});
      for(size_t f=0;f<tri.size();++f) for(int v:tri[f]) normals[v]=add(normals[v],fn[f]);
    }
    std::vector<double> g(n);
    for(int v=0;v<n;++v) g[v]=facing(normals[v],view[v]);
    std::vector<Segment> segments; std::vector<double> where(edges.size(),-1);
    auto crossing=[&](int a,int b) {
      long long key=a<b?static_cast<long long>(a)*n+b:static_cast<long long>(b)*n+a;
      int e=ids[key]; const Edge& x=edges[e];
      where[e]=g[x.a]/(g[x.a]-g[x.b]);
      return e;
    };
    for(const Tri& t:tri) {
      int found[2],k=0;
      for(int c=0;c<3&&k<2;++c) {
        int a=t[c],b=t[(c+1)%3];
        if(a!=b&&(g[a]>0)!=(g[b]>0)) found[k++]=crossing(a,b);
      }
      if(k==2) segments.push_back({found[0],found[1]});
    }
    emit(chains(edges.size(),segments),
      [&](int e) { const Edge& x=edges[e]; double s=where[e]; return add(mul(xyz[x.a],1-s),mul(xyz[x.b],s)); },
      [&](int e) { const Edge& x=edges[e]; return where[e]<0.5?x.a:x.b; });
  }
  return out;
}
}
}
#endif
