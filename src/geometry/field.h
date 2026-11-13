#ifndef GGPLOT3D_GEOMETRY_FIELD_H
#define GGPLOT3D_GEOMETRY_FIELD_H
#include "mesh.h"
namespace ggplot3d_geometry {

// Scalar field on a rectilinear grid, stored x fastest. Corner c of a cell
// uses bit 0 for x, bit 1 for y and bit 2 for z.
class ScalarGrid {
 public:
  std::array<std::vector<double>,3> axes;
  std::vector<double> values;
  std::vector<V> gradients;
  int nx,ny,nz;
  double magnitude=0;
  ScalarGrid(Rcpp::NumericVector x,Rcpp::NumericVector y,Rcpp::NumericVector z,Rcpp::NumericVector v):
    axes{Rcpp::as<std::vector<double>>(x),Rcpp::as<std::vector<double>>(y),Rcpp::as<std::vector<double>>(z)},
    values(Rcpp::as<std::vector<double>>(v)),nx(x.size()),ny(y.size()),nz(z.size()) {
    if(nx<2||ny<2||nz<2||static_cast<double>(nx)*ny*nz!=v.size()) Rcpp::stop("Invalid scalar grid dimensions.");
    for(const auto& axis:axes) for(size_t i=0;i<axis.size();++i)
      if(!std::isfinite(axis[i])||(i>0&&axis[i]<=axis[i-1])) Rcpp::stop("Grid axes must be finite and strictly increasing.");
    for(double value:values) if(std::isfinite(value)) magnitude=std::max(magnitude,std::abs(value));
    gradients.resize(values.size());
    for(int k=0;k<nz;++k) for(int j=0;j<ny;++j) for(int i=0;i<nx;++i) gradients[index(i,j,k)]=difference(i,j,k);
  }
  int index(int i,int j,int k) const { return i+nx*(j+ny*k); }
  std::array<int,3> ijk(int id) const { return {id%nx,(id/nx)%ny,id/(nx*ny)}; }
  V point(int id) const { auto q=ijk(id); return {axes[0][q[0]],axes[1][q[1]],axes[2][q[2]]}; }
  bool finite(int id) const { return std::isfinite(values[id]); }
  // Central differences in the interior, one-sided on the boundary; a missing neighbour gives zero.
  V difference(int i,int j,int k) const {
    int q[3]={i,j,k},dims[3]={nx,ny,nz}; V g{};
    for(int a=0;a<3;++a) {
      int lo[3]={i,j,k},hi[3]={i,j,k}; lo[a]=std::max(0,q[a]-1); hi[a]=std::min(dims[a]-1,q[a]+1);
      double l=values[index(lo[0],lo[1],lo[2])],h=values[index(hi[0],hi[1],hi[2])];
      g[a]=std::isfinite(l)&&std::isfinite(h) ? (h-l)/(axes[a][hi[a]]-axes[a][lo[a]]) : 0;
    }
    return g;
  }
  // Field values within 64 machine epsilons of the level, relative to the field
  // magnitude, are snapped onto the level so coincident crossings share grid vertices.
  double sample(int id,double level) const {
    double tolerance=64*std::numeric_limits<double>::epsilon()*std::max(magnitude,std::abs(level));
    return std::abs(values[id]-level)<=tolerance?level:values[id];
  }
  bool high(int id,double level) const { return sample(id,level)>=level; }
  // Crossing parameter along the edge a -> b of a sign-changing pair.
  double crossing(int a,int b,double level) const {
    double sa=sample(a,level),sb=sample(b,level);
    return (level-sa)/(sb-sa);
  }
  int corner(int i,int j,int k,int c) const { return index(i+(c&1),j+((c>>1)&1),k+((c>>2)&1)); }
  bool on_boundary(int id,int axis) const { int q=ijk(id)[axis],n=axis==0?nx:(axis==1?ny:nz); return q==0||q==n-1; }
};

// Vertex ids of grid points and of grid edges, keyed by the edge's lower grid
// point and its offset mask (bit 0 x, bit 1 y, bit 2 z; mask 0 is the point
// itself). Two z slices are kept, so a sweep over cell layers in increasing z
// welds shared vertices with array indexing and constant memory per layer.
class VertexSlab {
  int nx,ny;
  std::array<std::vector<int>,8> slots;
 public:
  VertexSlab(int nx,int ny): nx(nx),ny(ny) { for(auto& s:slots) s.assign(2*nx*ny,-1); }
  void begin_layer(int k) {
    int plane=nx*ny,next=((k+1)&1)*plane,current=(k&1)*plane;
    for(int d=0;d<8;++d) {
      std::fill(slots[d].begin()+next,slots[d].begin()+next+plane,-1);
      if(d&4) std::fill(slots[d].begin()+current,slots[d].begin()+current+plane,-1);
    }
  }
  int& at(int start,int mask) {
    int i=start%nx,j=(start/nx)%ny,k=start/(nx*ny);
    return slots[mask][(k&1)*nx*ny+j*nx+i];
  }
};

// Welded vertex at a grid point.
static inline int grid_vertex(Mesh& mesh,VertexSlab& slab,const ScalarGrid& grid,int a) {
  int& id=slab.at(a,0);
  if(id<0) { Vertex v; v.p=grid.point(a); v.n=unit(grid.gradients[a]); v.ea=v.eb=a; id=mesh.add(v); }
  return id;
}

// Welded vertex where the level crosses the grid edge a-b, with linearly
// interpolated position and normalised interpolated gradient.
static inline int edge_vertex(Mesh& mesh,VertexSlab& slab,const ScalarGrid& grid,int a,int b,double level) {
  if(a>b) std::swap(a,b);
  double t=grid.crossing(a,b,level);
  if(t<=0) return grid_vertex(mesh,slab,grid,a);
  if(t>=1) return grid_vertex(mesh,slab,grid,b);
  auto qa=grid.ijk(a),qb=grid.ijk(b);
  int mask=(qb[0]-qa[0])|((qb[1]-qa[1])<<1)|((qb[2]-qa[2])<<2);
  int& id=slab.at(a,mask);
  if(id<0) {
    Vertex v; V pa=grid.point(a),pb=grid.point(b);
    v.p=add(pa,mul(sub(pb,pa),t));
    v.n=unit(add(grid.gradients[a],mul(sub(grid.gradients[b],grid.gradients[a]),t)));
    v.ea=a; v.eb=b; v.t=t; id=mesh.add(v);
  }
  return id;
}

// Closes the sublevel region (field below the level) on a face of the grid box.
// corners lists the face's grid ids in cyclic order; a quad face with four
// crossings is split as the surface's face decision splits it.
static inline void cap_face(Mesh& mesh,VertexSlab& slab,const ScalarGrid& grid,const std::vector<int>& corners,
                            double level,int axis,double side,bool split_low) {
  int n=corners.size();
  V normal{0,0,0}; normal[axis]=side;
  // Fans start at a grid corner, so no cap diagonal joins two crossings; a
  // tunnel through the adjacent cell may join two crossings of this face.
  auto emit=[&](std::vector<int> poly) {
    for(size_t j=0;j<poly.size();++j) if(mesh.vertices[poly[j]].ea==mesh.vertices[poly[j]].eb) {
      std::rotate(poly.begin(),poly.begin()+j,poly.end()); break;
    }
    for(size_t j=1;j+1<poly.size();++j) mesh.triangle(poly[0],poly[j],poly[j+1],normal,level,true);
  };
  auto low_vertex=[&](int a) { int id=grid_vertex(mesh,slab,grid,a); return id; };
  std::vector<int> poly;
  for(int j=0;j<n;++j) {
    int a=corners[j],b=corners[(j+1)%n]; bool ha=grid.high(a,level),hb=grid.high(b,level);
    if(!ha) poly.push_back(low_vertex(a));
    if(ha!=hb) poly.push_back(edge_vertex(mesh,slab,grid,a,b,level));
  }
  if(poly.size()==6&&n==4&&split_low) {
    int first=grid.high(corners[0],level)?1:0;
    for(int j=first;j<4;j+=2) {
      int a=corners[j],before=corners[(j+3)%4],after=corners[(j+1)%4];
      emit({edge_vertex(mesh,slab,grid,before,a,level),low_vertex(a),edge_vertex(mesh,slab,grid,a,after,level)});
    }
  } else emit(poly);
}
}
#endif
