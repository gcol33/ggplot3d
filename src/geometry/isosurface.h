#ifndef GGPLOT3D_GEOMETRY_ISOSURFACE_H
#define GGPLOT3D_GEOMETRY_ISOSURFACE_H
#include "vector.h"
namespace ggplot3d_geometry {
class Field {
  std::array<Rcpp::NumericVector,3> axes;
  Rcpp::NumericVector values;
  std::vector<V> gradients;
  int nx,ny,nz;
  double magnitude=0;
  int index(int i,int j,int k) const { return i+nx*(j+ny*k); }
  std::map<std::pair<int,int>,Corner> cache;
  int next_id=0;
 public:
  Field(Rcpp::NumericVector x,Rcpp::NumericVector y,Rcpp::NumericVector z,Rcpp::NumericVector v):
    axes{x,y,z},values(v),nx(x.size()),ny(y.size()),nz(z.size()) {
    if(nx<2||ny<2||nz<2||static_cast<double>(nx)*ny*nz!=v.size()) Rcpp::stop("Invalid scalar grid dimensions.");
    for(auto axis:axes) for(int i=0;i<axis.size();++i)
      if(!std::isfinite(axis[i])||(i>0&&axis[i]<=axis[i-1])) Rcpp::stop("Grid axes must be finite and strictly increasing.");
    for(double value:values) if(std::isfinite(value)) magnitude=std::max(magnitude,std::abs(value));
    gradients.resize(v.size());
    for(int k=0;k<nz;++k) for(int j=0;j<ny;++j) for(int i=0;i<nx;++i) {
      int q[3]={i,j,k},dims[3]={nx,ny,nz}; V g{};
      for(int a=0;a<3;++a) {
        int lo[3]={i,j,k},hi[3]={i,j,k}; lo[a]=std::max(0,q[a]-1); hi[a]=std::min(dims[a]-1,q[a]+1);
        double l=v[index(lo[0],lo[1],lo[2])],h=v[index(hi[0],hi[1],hi[2])];
        g[a]=std::isfinite(l)&&std::isfinite(h) ? (h-l)/(axes[a][hi[a]]-axes[a][lo[a]]) : 0;
      }
      gradients[index(i,j,k)]=g;
    }
  }
  double sample(int id,double level) const {
    double tolerance=64*std::numeric_limits<double>::epsilon()*std::max(magnitude,std::abs(level));
    return std::abs(values[id]-level)<=tolerance?level:values[id];
  }
  V point(int id) const { return {axes[0][id%nx],axes[1][(id/nx)%ny],axes[2][id/(nx*ny)]}; }
  Corner edge(int a,int b,double level) {
    double t=(level-sample(a,level))/(sample(b,level)-sample(a,level));
    if(t==0) b=a; else if(t==1) a=b;
    auto key=std::minmax(a,b); auto found=cache.find(key);
    if(found!=cache.end()) return found->second;
    Corner c;
    c.p=a==b?point(a):add(point(a),mul(sub(point(b),point(a)),t));
    c.n=unit(a==b?gradients[a]:add(gradients[a],mul(sub(gradients[b],gradients[a]),t)));
    c.id=next_id++; cache[key]=c; return c;
  }
  Corner vertex(int a) {
    auto key=std::make_pair(a,a); auto found=cache.find(key);
    if(found!=cache.end()) return found->second;
    Corner c{point(a),unit(gradients[a]),next_id++}; cache[key]=c; return c;
  }
  void tetra(Mesh& mesh,const std::array<int,4>& t,double level) {
    std::vector<int> hi,lo;
    for(int a:t) (sample(a,level)>=level?hi:lo).push_back(a);
    if(hi.empty()||lo.empty()) return;
    V direction=sub(point(hi[0]),point(lo[0]));
    if(hi.size()==1||lo.size()==1) {
      int a=hi.size()==1?hi[0]:lo[0]; const auto& others=hi.size()==1?lo:hi;
      mesh.triangle(edge(a,others[0],level),edge(a,others[1],level),edge(a,others[2],level),direction,level);
    } else {
      Corner a=edge(hi[0],lo[0],level),b=edge(hi[0],lo[1],level),c=edge(hi[1],lo[0],level),d=edge(hi[1],lo[1],level);
      mesh.triangle(a,b,d,direction,level); mesh.triangle(a,d,c,direction,level);
    }
  }
  void boundary(Mesh& mesh,std::array<int,3> t,double level,int axis,double side) {
    std::vector<Corner> poly;
    for(int j=0;j<3;++j) {
      int a=t[j],b=t[(j+1)%3]; bool ain=sample(a,level)<=level,bin=sample(b,level)<=level;
      if(ain) poly.push_back(vertex(a));
      if(ain!=bin) poly.push_back(edge(a,b,level));
    }
    V normal{0,0,0}; normal[axis]=side;
    for(auto& p:poly) p.n=normal;
    for(size_t j=1;j+1<poly.size();++j) mesh.triangle(poly[0],poly[j],poly[j+1],normal,level,true);
  }
  Rcpp::DataFrame extract(Rcpp::NumericVector levels,bool caps) {
    Mesh mesh;
    constexpr int tets[6][4]={{0,1,3,7},{0,3,2,7},{0,2,6,7},{0,6,4,7},{0,4,5,7},{0,5,1,7}};
    constexpr int faces[4][3]={{0,1,2},{0,1,3},{0,2,3},{1,2,3}};
    for(double level:levels) {
      if(!std::isfinite(level)) Rcpp::stop("Levels must be finite.");
      cache.clear();
      for(int k=0;k<nz-1;++k) {
        Rcpp::checkUserInterrupt();
        for(int j=0;j<ny-1;++j) for(int i=0;i<nx-1;++i) {
          int ids[8]; bool valid=true;
          for(int a=0;a<8;++a) { ids[a]=index(i+(a&1),j+((a>>1)&1),k+((a>>2)&1)); valid=valid&&std::isfinite(values[ids[a]]); }
          if(!valid) continue;
          for(auto& tet:tets) {
            std::array<int,4> t{ids[tet[0]],ids[tet[1]],ids[tet[2]],ids[tet[3]]}; tetra(mesh,t,level);
            if(!caps) continue;
            for(auto& f:faces) for(int axis=0;axis<3;++axis) {
              V a=point(t[f[0]]),b=point(t[f[1]]),c=point(t[f[2]]);
              if(a[axis]!=b[axis]||a[axis]!=c[axis]) continue;
              if(a[axis]==axes[axis][0]) boundary(mesh,{t[f[0]],t[f[1]],t[f[2]]},level,axis,-1);
              else if(a[axis]==axes[axis][axes[axis].size()-1]) boundary(mesh,{t[f[0]],t[f[1]],t[f[2]]},level,axis,1);
            }
          }
        }
      }
    }
    return mesh.output();
  }
};
}
#endif
