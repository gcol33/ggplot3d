#ifndef GGPLOT3D_GEOMETRY_OPERATORS_H
#define GGPLOT3D_GEOMETRY_OPERATORS_H
#include "halfedge.h"
namespace ggplot3d_geometry {
namespace halfedge {

constexpr double pi=3.14159265358979323846;

static inline double angle(V u,V w) { return std::atan2(norm(cross(u,w)),dot(u,w)); }

// Corner geometry of one triangle: corner positions, the cross product of
// its first two edges (twice the area times the unit normal), interior
// angles and their cotangents.
struct FaceGeometry {
  Tri t; V x[3]; V cr; double twice=0,a[3]={0,0,0},cot[3]={0,0,0}; bool obtuse=false;
};

static inline FaceGeometry face_geometry(const Mesh& m,int f) {
  FaceGeometry g; g.t=m.corners(f);
  for(int i=0;i<3;++i) g.x[i]=m.p[g.t[i]];
  g.cr=cross(sub(g.x[1],g.x[0]),sub(g.x[2],g.x[0])); g.twice=norm(g.cr);
  for(int i=0;i<3;++i) {
    V u=sub(g.x[(i+1)%3],g.x[i]),w=sub(g.x[(i+2)%3],g.x[i]);
    g.a[i]=angle(u,w); g.cot[i]=g.twice>0?dot(u,w)/g.twice:0; g.obtuse=g.obtuse||g.a[i]>pi/2;
  }
  return g;
}

// Share of the mixed Voronoi area of triangle g at corner i (Meyer et al.
// 2003): the Voronoi region for non-obtuse triangles, otherwise half the
// triangle at the obtuse corner and a quarter at the others.
static inline double mixed_area(const FaceGeometry& g,int i) {
  int j=(i+1)%3,k=(i+2)%3;
  if(!g.obtuse) return (dot(sub(g.x[j],g.x[i]),sub(g.x[j],g.x[i]))*g.cot[k]+dot(sub(g.x[k],g.x[i]),sub(g.x[k],g.x[i]))*g.cot[j])/8;
  return g.twice/2*(g.a[i]>pi/2?0.5:0.25);
}

// Half-edge of face f opposite its corner i (corners in the order of
// Mesh::corners).
static inline int opposite_halfedge(const Mesh& m,int f,int i) {
  int h=m.fh[f]; for(int s=0;s<(i+1)%3;++s) h=m.nx[h]; return h;
}

// Mixed Voronoi area of every vertex.
static inline std::vector<double> mixed_areas(const Mesh& m) {
  std::vector<double> area(m.p.size(),0);
  for(size_t f=0;f<m.fh.size();++f) {
    if(m.fdead[f]) continue;
    FaceGeometry g=face_geometry(m,f);
    for(int i=0;i<3;++i) area[g.t[i]]+=mixed_area(g,i);
  }
  return area;
}

// Cotangent weight (cot alpha + cot beta) / 2 of every edge, over the
// angles opposite it in its one or two triangles (MacNeal 1949; Pinkall and
// Polthier 1993).
static inline std::vector<double> cotan_weights(const Mesh& m) {
  std::vector<double> w(m.edges(),0);
  for(size_t f=0;f<m.fh.size();++f) {
    if(m.fdead[f]) continue;
    FaceGeometry g=face_geometry(m,f);
    for(int i=0;i<3;++i) w[opposite_halfedge(m,f,i)>>1]+=g.cot[i]/2;
  }
  return w;
}

// Interior angle of the triangle of half-edge h at its tail, from the
// positions.
static inline double tail_angle(const Mesh& m,int h) {
  V a=m.p[m.from(h)];
  return angle(sub(m.p[m.to[h]],a),sub(m.p[m.from(m.pv[h])],a));
}

// Polar angle of every outgoing half-edge in the tangent plane of its tail
// vertex (Knoeppel et al. 2013; Sharp, Soliman and Crane 2019, eq. 8-9):
// interior angles around a vertex are rescaled to sum to 2 pi (pi at a
// boundary vertex), angles grow counterclockwise about the face normal, and
// the vertex's outgoing half-edge (its boundary half-edge on the boundary)
// has angle 0. Mesh::rotate turns clockwise, so each step subtracts.
static inline std::vector<double> polar_angles(const Mesh& m) {
  std::vector<double> phi(m.to.size(),0);
  for(size_t v=0;v<m.p.size();++v) {
    if(m.vdead[v]||m.out[v]<0) continue;
    double total=0;
    m.around(v,[&](int h) { if(m.fc[h^1]>=0) total+=tail_angle(m,m.rotate(h)); });
    double scale=total>0?(m.vb(v)?pi:2*pi)/total:0,at=0;
    m.around(v,[&](int h) {
      phi[h]=std::fmod(std::fmod(at,2*pi)+2*pi,2*pi);
      if(m.fc[h^1]>=0) at-=scale*tail_angle(m,m.rotate(h));
    });
  }
  return phi;
}
}
}
#endif
