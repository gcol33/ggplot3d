#ifndef GGPLOT3D_GEOMETRY_SLICE_H
#define GGPLOT3D_GEOMETRY_SLICE_H
#include "clip.h"
namespace ggplot3d_geometry {

// Trilinear interpolation of the grid at p: NaN outside the grid box or where
// a corner with nonzero weight is missing. Weights are products of the
// fractional positions along each axis, so a grid point returns its own value.
static inline double trilinear(const ScalarGrid& grid,V p) {
  int cell[3]; double t[3];
  for(int a=0;a<3;++a) {
    const auto& axis=grid.axes[a]; int n=axis.size();
    double tolerance=1e-12*(axis.back()-axis.front()),x=p[a];
    if(x<axis.front()-tolerance||x>axis.back()+tolerance) return NAN;
    x=std::min(std::max(x,axis.front()),axis.back());
    int i=std::upper_bound(axis.begin(),axis.end(),x)-axis.begin()-1;
    i=std::min(std::max(i,0),n-2);
    cell[a]=i; t[a]=(x-axis[i])/(axis[i+1]-axis[i]);
  }
  double out=0;
  for(int c=0;c<8;++c) {
    double w=1;
    for(int a=0;a<3;++a) w*=(c>>a)&1?t[a]:1-t[a];
    if(w==0) continue;
    double value=grid.values[grid.corner(cell[0],cell[1],cell[2],c)];
    if(!std::isfinite(value)) return NAN;
    out+=w*value;
  }
  return out;
}

// Cyclic polygon, in plane coordinates (u, v), where the plane cuts the grid box.
static inline std::vector<std::array<double,2>> box_section(const ScalarGrid& grid,const Plane& plane,V u,V v) {
  V lo{grid.axes[0].front(),grid.axes[1].front(),grid.axes[2].front()},hi{grid.axes[0].back(),grid.axes[1].back(),grid.axes[2].back()};
  auto corner=[&](int c) { return V{c&1?hi[0]:lo[0],c&2?hi[1]:lo[1],c&4?hi[2]:lo[2]}; };
  std::vector<std::array<double,2>> out;
  for(int a=0;a<8;++a) for(int axis=0;axis<3;++axis) {
    int b=a|(1<<axis);
    if(b==a) continue;
    double sa=plane.distance(corner(a)),sb=plane.distance(corner(b));
    if((sa<0&&sb<0)||(sa>0&&sb>0)||(sa==0&&sb==0)) continue;
    double t=sa/(sa-sb);
    V p=add(corner(a),mul(sub(corner(b),corner(a)),t));
    out.push_back({dot(p,u),dot(p,v)});
  }
  if(out.size()<3) return {};
  double cu=0,cv=0;
  for(auto& q:out) { cu+=q[0]; cv+=q[1]; }
  cu/=out.size(); cv/=out.size();
  std::sort(out.begin(),out.end(),[&](const std::array<double,2>& a,const std::array<double,2>& b) {
    return std::atan2(a[1]-cv,a[0]-cu)<std::atan2(b[1]-cv,b[0]-cu);
  });
  double scale=norm(sub(hi,lo)),tolerance=1e-12*scale;
  std::vector<std::array<double,2>> unique;
  for(auto& q:out) if(unique.empty()||std::hypot(q[0]-unique.back()[0],q[1]-unique.back()[1])>tolerance) unique.push_back(q);
  while(unique.size()>1&&std::hypot(unique[0][0]-unique.back()[0],unique[0][1]-unique.back()[1])<=tolerance) unique.pop_back();
  return unique.size()<3?std::vector<std::array<double,2>>{}:unique;
}

// Planar section of the grid sampled on a lattice of plane coordinates. An
// axis-aligned plane is sampled on the grid's own coordinates along the two
// other axes, so its lattice points are grid points (on a grid layer, the
// values are the grid values); any other plane on n x n points over the
// section's bounding rectangle in the frame of plane_frame(). Lattice cells
// are split into two triangles, clipped to the section polygon, and every
// vertex takes the trilinear field value. Triangles touching a missing value
// are dropped.
struct Section {
  std::vector<V> points; std::vector<double> values;
  std::vector<std::array<int,3>> triangles;
};

static inline Section slice_grid(const ScalarGrid& grid,const Plane& plane,int n) {
  Section out;
  V u,v; int axis=-1;
  for(int a=0;a<3;++a) if(std::abs(plane.normal[a])==1) axis=a;
  if(axis>=0) {
    u=V{0,0,0}; v=V{0,0,0}; u[(axis+1)%3]=1; v[(axis+2)%3]=1;
  } else plane_frame(plane.normal,u,v);
  V origin=mul(plane.normal,dot(plane.normal,plane.point));
  std::vector<std::array<double,2>> section=box_section(grid,plane,u,v);
  if(section.empty()) return out;
  std::vector<double> su,sv;
  if(axis>=0) { su=grid.axes[(axis+1)%3]; sv=grid.axes[(axis+2)%3]; }
  else {
    double u0=INFINITY,u1=-INFINITY,v0=INFINITY,v1=-INFINITY;
    for(auto& q:section) { u0=std::min(u0,q[0]); u1=std::max(u1,q[0]); v0=std::min(v0,q[1]); v1=std::max(v1,q[1]); }
    for(int k=0;k<n;++k) { su.push_back(u0+(u1-u0)*k/(n-1)); sv.push_back(v0+(v1-v0)*k/(n-1)); }
  }
  int nu=su.size(),nv=sv.size(),m=section.size();
  double scale=0;
  for(int a=0;a<3;++a) scale=std::max(scale,grid.axes[a].back()-grid.axes[a].front());
  double tolerance=1e-12*scale*scale;
  // Signed area test of q against the section edge e (counterclockwise polygon).
  auto side=[&](int e,const std::array<double,2>& q) {
    const auto &a=section[e],&b=section[(e+1)%m];
    return (b[0]-a[0])*(q[1]-a[1])-(b[1]-a[1])*(q[0]-a[0]);
  };
  std::vector<int> lattice(nu*nv,-1);
  auto add_point=[&](const std::array<double,2>& q) {
    out.points.push_back(add(origin,add(mul(u,q[0]),mul(v,q[1]))));
    out.values.push_back(trilinear(grid,out.points.back()));
    return static_cast<int>(out.points.size())-1;
  };
  struct Corner { std::array<double,2> q; int id; };
  auto node=[&](int i,int j) {
    int& id=lattice[i+nu*j];
    if(id<0) id=add_point({su[i],sv[j]});
    return Corner{{su[i],sv[j]},id};
  };
  auto emit=[&](std::vector<Corner> poly) {
    for(int e=0;e<m&&poly.size()>=3;++e) {
      std::vector<Corner> next;
      for(size_t k=0;k<poly.size();++k) {
        const Corner &a=poly[k],&b=poly[(k+1)%poly.size()];
        double da=side(e,a.q),db=side(e,b.q);
        bool ia=da>=-tolerance,ib=db>=-tolerance;
        if(ia) next.push_back(a);
        if(ia!=ib) {
          double t=da/(da-db);
          next.push_back({{a.q[0]+(b.q[0]-a.q[0])*t,a.q[1]+(b.q[1]-a.q[1])*t},-1});
        }
      }
      poly.swap(next);
    }
    if(poly.size()<3) return;
    std::vector<int> ids;
    for(auto& c:poly) ids.push_back(c.id>=0?c.id:add_point(c.q));
    for(size_t k=1;k+1<ids.size();++k) {
      std::array<int,3> t{ids[0],ids[k],ids[k+1]};
      bool finite=true;
      for(int id:t) finite=finite&&std::isfinite(out.values[id]);
      V a=out.points[t[0]],b=out.points[t[1]],c=out.points[t[2]];
      if(!finite||norm(cross(sub(b,a),sub(c,a)))<=tolerance) continue;
      if(dot(cross(sub(b,a),sub(c,a)),plane.normal)<0) std::swap(t[1],t[2]);
      out.triangles.push_back(t);
    }
  };
  for(int j=0;j+1<nv;++j) for(int i=0;i+1<nu;++i) {
    emit({node(i,j),node(i+1,j),node(i+1,j+1)});
    emit({node(i,j),node(i+1,j+1),node(i,j+1)});
  }
  return out;
}
}
#endif
