#ifndef GGPLOT3D_GEOMETRY_CLIP_H
#define GGPLOT3D_GEOMETRY_CLIP_H
#include "isosurface.h"
#include <unordered_map>
namespace ggplot3d_geometry {

static inline long long pair_key(int a,int b) {
  if(a>b) std::swap(a,b);
  return static_cast<long long>(a)*4294967296LL+b;
}

// Two unit vectors that complete the plane normal to a right-handed frame.
static inline void plane_frame(V normal,V& u,V& v) {
  int a=0;
  for(int k=1;k<3;++k) if(std::abs(normal[k])<std::abs(normal[a])) a=k;
  V e{0,0,0}; e[a]=1;
  u=unit(cross(e,normal)); v=cross(normal,u);
}

// Signed distances to the plane, with distances within 1e-9 of the extent of
// the points snapped onto the plane, so that no cut leaves a sliver thinner
// than that next to a point.
template<class Point>
static inline std::vector<double> plane_distances(const Plane& plane,int n,Point point) {
  std::vector<double> s(n); double extent=0;
  for(int i=0;i<n;++i) { V p=point(i); s[i]=plane.distance(p); extent=std::max(extent,norm(sub(p,plane.point))); }
  double tolerance=1e-9*extent;
  for(double& x:s) if(std::abs(x)<=tolerance) x=0;
  return s;
}

// The part of every triangle on the negative side of the plane, by
// Sutherland-Hodgman clipping against the one plane. A cut edge gets one
// vertex where the plane crosses it, shared by the triangles on that edge,
// with the normalised interpolated normal; it lies on no grid edge. A vertex
// on the plane is kept as it is.
static inline Mesh clip_mesh(const Mesh& mesh,const Plane& plane) {
  Mesh out; out.vertices=mesh.vertices;
  int n=mesh.vertices.size();
  std::vector<double> s=plane_distances(plane,n,[&](int i) { return mesh.vertices[i].p; });
  std::unordered_map<long long,int> cut;
  auto crossing=[&](int a,int b) {
    if(s[a]==0) return a;
    if(s[b]==0) return b;
    auto found=cut.find(pair_key(a,b));
    if(found!=cut.end()) return found->second;
    if(a>b) std::swap(a,b);
    double t=s[a]/(s[a]-s[b]);
    const Vertex &va=mesh.vertices[a],&vb=mesh.vertices[b];
    Vertex v; v.p=add(va.p,mul(sub(vb.p,va.p),t)); v.n=unit(add(va.n,mul(sub(vb.n,va.n),t)));
    int id=out.add(v); cut[pair_key(a,b)]=id;
    return id;
  };
  for(size_t f=0;f<mesh.triangles.size();++f) {
    const auto& t=mesh.triangles[f];
    if(t[0]<0) continue;
    std::vector<int> poly;
    for(int j=0;j<3;++j) {
      int a=t[j],b=t[(j+1)%3]; bool ia=s[a]<=0,ib=s[b]<=0;
      if(ia) poly.push_back(a);
      if(ia!=ib) { int c=crossing(a,b); if(poly.empty()||poly.back()!=c) poly.push_back(c); }
    }
    while(poly.size()>1&&poly.front()==poly.back()) poly.pop_back();
    for(size_t j=1;j+1<poly.size();++j) out.oriented(poly[0],poly[j],poly[j+1],mesh.levels[f],mesh.caps[f],mesh.cap_normals[f]);
  }
  return out;
}

// Caps of the sublevel region of every level on the plane, inside the grid
// box. Each cell is split into the six tetrahedra of the "tetra" method, on
// which the trilinear field is replaced by its linear interpolant. The plane
// cuts a tetrahedron in a triangle or a convex quad on which the field is
// linear, and the sublevel part of that polygon is fanned as on a box face.
// Plane points are shared through the tetrahedron edge they lie on, level
// crossings through the polygon edge; cap faces point along the plane normal.
static inline void plane_caps(Mesh& mesh,const ScalarGrid& grid,const std::vector<double>& levels,const Plane& plane) {
  constexpr int edges[6][2]={{0,1},{0,2},{0,3},{1,2},{1,3},{2,3}};
  V u,v; plane_frame(plane.normal,u,v);
  std::vector<double> distance=plane_distances(plane,grid.values.size(),[&](int i) { return grid.point(i); });
  for(double level:levels) {
    std::unordered_map<long long,int> points,crossings;
    std::unordered_map<int,double> field;
    // A grid point on the plane is one plane point for all its edges.
    auto point=[&](int a,int b) {
      if(distance[a]==0) b=a; else if(distance[b]==0) a=b;
      if(a>b) std::swap(a,b);
      auto found=points.find(pair_key(a,b));
      if(found!=points.end()) return found->second;
      double t=a==b?0:distance[a]/(distance[a]-distance[b]);
      Vertex x; V pa=grid.point(a); x.p=add(pa,mul(sub(grid.point(b),pa),t)); x.n=plane.normal;
      int id=mesh.add(x); points[pair_key(a,b)]=id;
      field[id]=grid.values[a]+t*(grid.values[b]-grid.values[a]);
      return id;
    };
    for(int k=0;k<grid.nz-1;++k) {
      Rcpp::checkUserInterrupt();
      for(int j=0;j<grid.ny-1;++j) for(int i=0;i<grid.nx-1;++i) {
        int ids[8]; bool valid=true, below=false, above=false;
        for(int c=0;c<8;++c) {
          ids[c]=grid.corner(i,j,k,c); valid=valid&&grid.finite(ids[c]);
          (distance[ids[c]]<=0?below:above)=true;
        }
        if(!valid||!below||!above) continue;
        for(const auto& tet:tetra::tets) {
          int t[4]={ids[tet[0]],ids[tet[1]],ids[tet[2]],ids[tet[3]]};
          std::vector<int> poly;
          for(const auto& e:edges) if((distance[t[e[0]]]<=0)!=(distance[t[e[1]]]<=0)) {
            int id=point(t[e[0]],t[e[1]]);
            if(std::find(poly.begin(),poly.end(),id)==poly.end()) poly.push_back(id);
          }
          if(poly.size()<3) continue;
          if(poly.size()==4) {
            V centre{0,0,0};
            for(int id:poly) centre=add(centre,mesh.vertices[id].p);
            centre=mul(centre,0.25);
            auto angle=[&](int id) { V d=sub(mesh.vertices[id].p,centre); return std::atan2(dot(d,v),dot(d,u)); };
            std::sort(poly.begin(),poly.end(),[&](int a,int b) { return angle(a)<angle(b); });
          }
          int n=poly.size();
          auto high=[&](int a) { return snap(field[poly[a]],level,grid.magnitude)>=level; };
          auto corner=[&](int a) { return poly[a]; };
          auto crossing=[&](int a,int b) {
            int p=poly[a],q=poly[b];
            if(p>q) std::swap(p,q);
            double fp=snap(field[p],level,grid.magnitude),fq=snap(field[q],level,grid.magnitude);
            if(fp==level) return p;
            if(fq==level) return q;
            auto found=crossings.find(pair_key(p,q));
            if(found!=crossings.end()) return found->second;
            double t=(level-fp)/(fq-fp);
            if(t<=1e-9) return p;
            if(t>=1-1e-9) return q;
            Vertex x; x.p=add(mesh.vertices[p].p,mul(sub(mesh.vertices[q].p,mesh.vertices[p].p),t)); x.n=plane.normal;
            int id=mesh.add(x); crossings[pair_key(p,q)]=id;
            return id;
          };
          cap_fan(mesh,sublevel_polygon(n,high,corner,crossing),plane.normal,level);
        }
      }
    }
  }
}
}
#endif
