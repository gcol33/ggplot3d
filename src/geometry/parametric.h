#ifndef GGPLOT3D_GEOMETRY_PARAMETRIC_H
#define GGPLOT3D_GEOMETRY_PARAMETRIC_H
#include "vector.h"
#include <unordered_map>
namespace ggplot3d_geometry {
namespace parametric {

using UV=std::array<double,2>;

// A rectangular grid of nu x nv samples in (u, v), u fastest. A closed
// direction wraps its last cell back to index 0; a twisted closed direction
// reverses the other parameter across that seam (Moebius strip, Klein bottle).
struct QuadGrid {
  int n[2]; bool closed[2]; bool twist[2];
  int cells(int d) const { return closed[d]?n[d]:n[d]-1; }
  int flip(int d,int j) const { return closed[d]?(n[d]-j)%n[d]:n[d]-1-j; }
  // Grid index of sample (i, j), with i in [0, n_u] and j in [0, n_v].
  int index(int i,int j) const {
    if(i==n[0]) { i=0; if(twist[0]) j=flip(1,j); }
    if(j==n[1]) { j=0; if(twist[1]) i=flip(0,i); }
    return i+n[0]*j;
  }
  // Quads in (u, v) order, so r_u x r_v orients each one.
  template<class F> void quads(F emit) const {
    for(int j=0;j<cells(1);++j) for(int i=0;i<cells(0);++i)
      emit(std::array<int,4>{index(i,j),index(i+1,j),index(i+1,j+1),index(i,j+1)},
           std::array<int,4>{i,i+1,i+1,i},std::array<int,4>{j,j,j+1,j+1});
  }
};

// Indexed quads split along the shorter diagonal into consistently wound
// triangles. Shared by parametric surfaces and swept tubes.
static inline void split_quad(const std::array<int,4>& q,const std::vector<V>& xyz,std::vector<std::array<int,3>>& out,
                              std::vector<std::array<int,3>>* corners=nullptr) {
  bool first=norm(sub(xyz[q[2]],xyz[q[0]]))<=norm(sub(xyz[q[3]],xyz[q[1]]));
  std::array<std::array<int,3>,2> local=first?std::array<std::array<int,3>,2>{{{0,1,2},{0,2,3}}}:
                                             std::array<std::array<int,3>,2>{{{0,1,3},{1,2,3}}};
  for(auto& t:local) {
    std::array<int,3> tri{q[t[0]],q[t[1]],q[t[2]]};
    if(tri[0]==tri[1]||tri[1]==tri[2]||tri[0]==tri[2]) continue;
    out.push_back(tri);
    if(corners) corners->push_back(t);
  }
}

// Midpoint parameters of an edge. A free parameter of a vertex (one along
// which the surface does not move there, e.g. u at a sphere pole) takes the
// value of the other endpoint, so the midpoint lies between the two points.
static inline UV midpoint(UV a,UV b,const std::array<bool,2>& fa,const std::array<bool,2>& fb) {
  for(int p=0;p<2;++p) {
    if(fa[p]&&!fb[p]) a[p]=b[p];
    else if(fb[p]&&!fa[p]) b[p]=a[p];
  }
  return {(a[0]+b[0])/2,(a[1]+b[1])/2};
}

static inline long long edge_key(int a,int b,long long n) { return a<b?a*n+b:b*n+a; }

struct Edges {
  std::vector<std::array<int,2>> ends; std::vector<UV> uv; std::vector<std::array<int,3>> of_triangle;
};

// Unique edges, the parameters of their midpoints taken from the first
// triangle containing them, and the edge of each triangle side.
static inline Edges edges(const std::vector<std::array<int,3>>& tri,const std::vector<std::array<UV,3>>& uv,
                          const std::vector<std::array<bool,2>>& free) {
  Edges out; std::unordered_map<long long,int> seen; long long n=free.size();
  for(size_t t=0;t<tri.size();++t) {
    std::array<int,3> ids{};
    for(int k=0;k<3;++k) {
      int a=tri[t][k],b=tri[t][(k+1)%3]; long long key=edge_key(a,b,n);
      auto found=seen.find(key);
      if(found==seen.end()) {
        int id=out.ends.size(); seen[key]=id; out.ends.push_back({a,b});
        out.uv.push_back(midpoint(uv[t][k],uv[t][(k+1)%3],free[a],free[b])); ids[k]=id;
      } else ids[k]=found->second;
    }
    out.of_triangle.push_back(ids);
  }
  return out;
}

// Conforming longest-edge bisection (Rivara 1984): every triangle with a
// marked side also marks its longest side, until stable; each triangle is
// then bisected along its longest side and the halves along their remaining
// marked sides. Midpoints are keyed by edge, so neighbours share them and no
// cracks open. New vertex ids follow the split edges in edge order.
struct Split { std::vector<std::array<int,3>> tri; std::vector<std::array<UV,3>> uv; std::vector<int> split_edges; };

static inline Split bisect(const std::vector<std::array<int,3>>& tri,const std::vector<std::array<UV,3>>& uv,
                           const std::vector<std::array<bool,2>>& free,const std::vector<V>& xyz,
                           const Edges& e,std::vector<char> marked) {
  auto longest=[&](size_t t) {
    int best=0; double length=-1;
    for(int k=0;k<3;++k) {
      double d=norm(sub(xyz[tri[t][(k+1)%3]],xyz[tri[t][k]]));
      if(d>length) { length=d; best=k; }
    }
    return best;
  };
  bool changed=true;
  while(changed) {
    changed=false;
    for(size_t t=0;t<tri.size();++t) {
      const auto& s=e.of_triangle[t];
      if(!(marked[s[0]]||marked[s[1]]||marked[s[2]])) continue;
      int l=s[longest(t)];
      if(!marked[l]) { marked[l]=1; changed=true; }
    }
  }
  Split out; std::vector<int> vertex(e.ends.size(),-1); int next=xyz.size();
  for(size_t k=0;k<e.ends.size();++k) if(marked[k]) { vertex[k]=next++; out.split_edges.push_back(k); }
  auto emit=[&](std::array<int,3> t,std::array<UV,3> p) { out.tri.push_back(t); out.uv.push_back(p); };
  for(size_t t=0;t<tri.size();++t) {
    const auto& s=e.of_triangle[t];
    int l=longest(t);
    if(!marked[s[l]]) { emit(tri[t],uv[t]); continue; }
    // Rotate so the longest side is A-B with apex C.
    int A=tri[t][l],B=tri[t][(l+1)%3],C=tri[t][(l+2)%3];
    UV a=uv[t][l],b=uv[t][(l+1)%3],c=uv[t][(l+2)%3];
    int ab=s[l],bc=s[(l+1)%3],ca=s[(l+2)%3],M=vertex[ab];
    UV m=midpoint(a,b,free[A],free[B]);
    if(marked[ca]) {
      int P=vertex[ca]; UV p=midpoint(c,a,free[C],free[A]);
      emit({A,M,P},{a,m,p}); emit({M,C,P},{m,c,p});
    } else emit({A,M,C},{a,m,c});
    if(marked[bc]) {
      int N=vertex[bc]; UV q=midpoint(b,c,free[B],free[C]);
      emit({M,B,N},{m,b,q}); emit({M,N,C},{m,q,c});
    } else emit({M,B,C},{m,b,c});
  }
  return out;
}

// Area-weighted face normals accumulated at vertices.
static inline std::vector<V> face_normals(const std::vector<std::array<int,3>>& tri,const std::vector<V>& xyz) {
  std::vector<V> out(xyz.size(),V{0,0,0});
  for(const auto& t:tri) {
    V n=cross(sub(xyz[t[1]],xyz[t[0]]),sub(xyz[t[2]],xyz[t[0]]));
    for(int v:t) out[v]=add(out[v],n);
  }
  for(auto& n:out) n=unit(n);
  return out;
}
}
}
#endif
