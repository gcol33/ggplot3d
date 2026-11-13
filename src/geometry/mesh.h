#ifndef GGPLOT3D_GEOMETRY_MESH_H
#define GGPLOT3D_GEOMETRY_MESH_H
#include "vector.h"
#include <numeric>
#include <iterator>
namespace ggplot3d_geometry {

// Vertex of an extracted surface. ea/eb are the grid ids bracketing an edge
// crossing at parameter t (ea == eb for a grid point, -1 when the vertex lies
// on no grid edge).
struct Vertex { V p, n; int ea=-1, eb=-1; double t=0; };

class UnionFind {
  std::vector<int> parent;
 public:
  explicit UnionFind(int n=0): parent(n) { std::iota(parent.begin(),parent.end(),0); }
  void reset(int n) { parent.resize(n); std::iota(parent.begin(),parent.end(),0); }
  int find(int a) { while(parent[a]!=a) { parent[a]=parent[parent[a]]; a=parent[a]; } return a; }
  bool unite(int a,int b) { a=find(a); b=find(b); if(a==b) return false; parent[std::max(a,b)]=std::min(a,b); return true; }
};

struct Mesh {
  std::vector<Vertex> vertices;
  std::vector<std::array<int,3>> triangles;
  std::vector<double> levels;
  std::vector<int> caps;
  std::vector<V> cap_normals;
  int add(const Vertex& v) { vertices.push_back(v); return static_cast<int>(vertices.size())-1; }
  // Adds a triangle wound so that its geometric normal points along direction.
  void triangle(int a,int b,int c,V direction,double level,bool cap=false) {
    if(a==b||b==c||a==c) return;
    V normal=cross(sub(vertices[b].p,vertices[a].p),sub(vertices[c].p,vertices[a].p));
    if(dot(normal,direction)<0) std::swap(b,c);
    triangles.push_back({a,b,c}); levels.push_back(level); caps.push_back(cap); cap_normals.push_back(cap?direction:V{0,0,0});
  }
  // Adds a triangle whose winding is already oriented towards increasing field value.
  void oriented(int a,int b,int c,double level,bool cap=false,V cap_normal=V{0,0,0}) {
    if(a==b||b==c||a==c) return;
    triangles.push_back({a,b,c}); levels.push_back(level); caps.push_back(cap); cap_normals.push_back(cap_normal);
  }
  void append(const Mesh& other) {
    int offset=vertices.size();
    vertices.insert(vertices.end(),other.vertices.begin(),other.vertices.end());
    for(auto t:other.triangles) { if(t[0]>=0) for(int& x:t) x+=offset; triangles.push_back(t); }
    levels.insert(levels.end(),other.levels.begin(),other.levels.end());
    caps.insert(caps.end(),other.caps.begin(),other.caps.end());
    cap_normals.insert(cap_normals.end(),other.cap_normals.begin(),other.cap_normals.end());
  }
  Rcpp::DataFrame output() const {
    std::vector<std::array<int,3>> kept; std::vector<int> source;
    for(size_t f=0;f<triangles.size();++f) {
      auto t=triangles[f];
      if(t[0]<0||t[0]==t[1]||t[1]==t[2]||t[0]==t[2]) continue;
      V a=vertices[t[0]].p,b=vertices[t[1]].p,c=vertices[t[2]].p,ab=sub(b,a),ac=sub(c,a);
      if(norm(cross(ab,ac))<=128*std::numeric_limits<double>::epsilon()*std::max(dot(ab,ab),dot(ac,ac))) continue;
      // A short first edge reduces cancellation in geometric face-normal calculations.
      int start=shortest_edge(a,b,c);
      kept.push_back({t[start],t[(start+1)%3],t[(start+2)%3]}); source.push_back(f);
    }
    int m=3*kept.size();
    Rcpp::NumericVector x(m),y(m),z(m),nx(m),ny(m),nz(m),level(m);
    Rcpp::IntegerVector face(m),corner(m),vertex(m),cap(m),edge_a(m),edge_b(m);
    for(int i=0;i<m;++i) {
      const Vertex& v=vertices[kept[i/3][i%3]]; int f=source[i/3];
      // Cap faces are flat box faces and carry the box normal at every corner.
      V n=caps[f]?cap_normals[f]:v.n;
      x[i]=v.p[0]; y[i]=v.p[1]; z[i]=v.p[2]; nx[i]=n[0]; ny[i]=n[1]; nz[i]=n[2];
      level[i]=levels[f]; face[i]=i/3+1; corner[i]=i%3+1; vertex[i]=kept[i/3][i%3]+1; cap[i]=caps[f];
      edge_a[i]=v.ea<0?NA_INTEGER:v.ea+1; edge_b[i]=v.eb<0?NA_INTEGER:v.eb+1;
    }
    return Rcpp::DataFrame::create(Rcpp::Named("x")=x,Rcpp::Named("y")=y,Rcpp::Named("z")=z,
      Rcpp::Named("nx")=nx,Rcpp::Named("ny")=ny,Rcpp::Named("nz")=nz,Rcpp::Named("level")=level,
      Rcpp::Named(".face")=face,Rcpp::Named(".corner")=corner,Rcpp::Named(".vertex")=vertex,Rcpp::Named("cap")=cap,
      Rcpp::Named(".edge_a")=edge_a,Rcpp::Named(".edge_b")=edge_b);
  }
};
}
#endif
