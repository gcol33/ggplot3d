#pragma once
#include "clipping.h"
#include "parallel.h"
#include <unordered_map>

namespace ggplot3d { namespace engine {
// Closed opaque components cannot expose outward back faces to an exterior eye.
// A face with a corner at infinity or behind it (after camera pre) belongs to no
// closed component; its normal is left zero for the caller to take from its
// clipped ring.
static inline std::vector<bool> solid_backfaces(const ScenePacket& packet,
                                               const std::vector<Point4>& points,const Projection& p,
                                               std::vector<Index>& components,std::vector<Vec3>& normals,
                                               std::size_t threads) {
  const auto& f=packet.faces;std::size_t n=f.count.size();
  std::vector<bool> culled(n,false);
  components.assign(n,-1);
  normals.resize(n);
  std::vector<Index> parent(n);std::iota(parent.begin(),parent.end(),0);
  auto root=[&](Index a) {while(parent[a]!=a) {parent[a]=parent[parent[a]];a=parent[a];}return a;};
  struct Edge {Index face;int direction,count;};
  std::unordered_map<std::uint64_t,Edge> edges;edges.reserve(n*2);
  std::vector<bool> closed(n,true);
  for(Index i=0;i<static_cast<Index>(n);++i) {
    if(f.opacity[i]!=1 || f.partition[i]!=1) {closed[i]=false;continue;}
    for(Index k=0;k<f.count[i];++k) {
      Index a=packet.corners.vertex[f.offset[i]+k],b=packet.corners.vertex[f.offset[i]+(k+1)%f.count[i]];
      auto key=(static_cast<std::uint64_t>(std::min(a,b))<<32)|static_cast<std::uint32_t>(std::max(a,b));
      int direction=a<b ? 1:-1;
      auto hit=edges.emplace(key,Edge{i,direction,1});
      if(!hit.second) {
        auto& edge=hit.first->second;
        parent[root(i)]=root(edge.face);
        ++edge.count;edge.direction+=direction;
      }
    }
  }
  for(const auto& pair:edges) if(pair.second.count!=2 || pair.second.direction!=0) closed[root(pair.second.face)]=false;
  std::vector<Vec3> lo(n,Vec3{R_PosInf,R_PosInf,R_PosInf}),hi(n,Vec3{R_NegInf,R_NegInf,R_NegInf});
  std::vector<double> volume(n,0);
  std::vector<Vec3> origin(n);
  std::vector<Polygon> polygons(n);
  parallel_for(n,threads,[&](std::size_t i) {
    auto& polygon=polygons[i];
    for(Index k=0;k<f.count[i];++k) {
      Index corner=f.offset[i]+k;
      polygon.push_back(corner_vertex(points,corner,packet.corners.vertex[corner]));
    }
    if(outside_chart(polygon)) {polygon.clear();normals[i]=Vec3{0,0,0};return;}
    dehomogenise(polygon);
    normals[i]=face_normal(polygon,p);
  });
  auto planes=clipping_planes(p);if(p.camera.clip) planes.resize(planes.size()-4);
  for(Index i=0;i<static_cast<Index>(n);++i) {
    Index r=root(i);if(!closed[i]) closed[r]=false;
    const auto& polygon=polygons[i];
    if(polygon.empty()) {closed[r]=false;continue;}
    for(const auto& vertex:polygon) {
      auto m=p.model(vertex.xyz);
      if(!std::isfinite(lo[r][0])) origin[r]=m;
      for(const auto& plane:planes) if(signed_distance(plane,vertex)<0) closed[r]=false;
      for(int a=0;a<3;++a) {lo[r][a]=std::min(lo[r][a],m[a]);hi[r][a]=std::max(hi[r][a],m[a]);}
    }
    auto a=subtract(p.model(polygon[0].xyz),origin[r]);
    for(std::size_t k=1;k+1<polygon.size();++k)
      volume[r]+=dot(a,cross(subtract(p.model(polygon[k].xyz),origin[r]),subtract(p.model(polygon[k+1].xyz),origin[r])));
  }
  for(Index i=0;i<static_cast<Index>(n);++i) {
    Index r=root(i);if(!closed[r] || volume[r]==0) continue;
    bool inside=p.camera.perspective;
    for(int a=0;a<3;++a) inside=inside && p.resolved.eye[a]>=lo[r][a] && p.resolved.eye[a]<=hi[r][a];
    if(inside) continue;
    components[i]=r;
    Vec3 direction=p.camera.perspective ? subtract(p.model(polygons[i][0].xyz),p.resolved.eye):p.resolved.ray;
    double facing=dot(normals[i],direction)*(volume[r]>0 ? 1:-1);
    culled[i]=facing>p.resolved.tolerance;
  }
  return culled;
}
} }
