#ifndef GGPLOT3D_GEOMETRY_HULL_H
#define GGPLOT3D_GEOMETRY_HULL_H
#include "vector.h"
namespace ggplot3d_geometry {
struct HullFace {
  std::array<int,3> ids; V normal; double offset; std::vector<int> outside;
  bool alive=true; std::array<int,3> neighbor{{-1,-1,-1}};
};
static inline HullFace hull_face(int a,int b,int c,const std::vector<V>& p,V inside) {
  V n=unit(cross(sub(p[b],p[a]),sub(p[c],p[a])));
  if(dot(n,sub(inside,p[a]))>0) { std::swap(b,c); n=mul(n,-1); }
  return {{a,b,c},n,dot(n,p[a]),{},true};
}
static inline double distance(const HullFace& f,V p) { return dot(f.normal,p)-f.offset; }
static inline Rcpp::IntegerMatrix hull(Rcpp::NumericMatrix input) {
  if(input.ncol()!=3) Rcpp::stop("Hull input must have three columns.");
  int n=input.nrow(); if(n<3) return Rcpp::IntegerMatrix(0,3);
  std::vector<V> raw(n); std::vector<int> order(n);
  for(int i=0;i<n;++i) { order[i]=i; for(int a=0;a<3;++a) {
    raw[i][a]=input(i,a); if(!std::isfinite(raw[i][a])) Rcpp::stop("Hull points must be finite.");
  } }
  std::sort(order.begin(),order.end(),[&](int a,int b){return raw[a]<raw[b];});
  order.erase(std::unique(order.begin(),order.end(),[&](int a,int b){return raw[a]==raw[b];}),order.end());
  std::vector<V> p; for(int i:order) p.push_back(raw[i]); n=p.size();
  if(n<3) return Rcpp::IntegerMatrix(0,3);
  V centre=p[0],hi=p[0],lo=p[0];
  for(auto a:p) for(int j=0;j<3;++j) {lo[j]=std::min(lo[j],a[j]);hi[j]=std::max(hi[j],a[j]);}
  double scale=norm(sub(hi,lo)),tol=512*std::numeric_limits<double>::epsilon();
  if(scale==0) return Rcpp::IntegerMatrix(0,3);
  for(auto& a:p) a=mul(sub(a,centre),1/scale);
  int a=0,b=1,c=-1,d=-1; double best=-1;
  for(int i=1;i<n;++i) {double v=norm(sub(p[i],p[a])); if(v>best){best=v;b=i;}}
  V line=unit(sub(p[b],p[a])); best=-1;
  for(int i=0;i<n;++i) {double v=norm(cross(line,sub(p[i],p[a]))); if(v>best){best=v;c=i;}}
  if(best<=tol) return Rcpp::IntegerMatrix(0,3);
  V normal=unit(cross(sub(p[b],p[a]),sub(p[c],p[a]))); best=-1;
  for(int i=0;i<n;++i) {double v=std::abs(dot(normal,sub(p[i],p[a]))); if(v>best){best=v;d=i;}}
  std::vector<std::array<int,3>> triangles;
  if(best<=tol) {
    int drop=std::max_element(normal.begin(),normal.end(),[](double x,double y){return std::abs(x)<std::abs(y);})-normal.begin();
    int u=(drop+1)%3,v=(drop+2)%3; std::vector<int> ids(n);
    for(int i=0;i<n;++i) ids[i]=i;
    std::sort(ids.begin(),ids.end(),[&](int i,int j){return p[i][u]==p[j][u]?p[i][v]<p[j][v]:p[i][u]<p[j][u];});
    auto orient=[&](int i,int j,int k){return (p[j][u]-p[i][u])*(p[k][v]-p[i][v])-(p[j][v]-p[i][v])*(p[k][u]-p[i][u]);};
    std::vector<int> ring;
    for(int pass=0;pass<2;++pass) {
      size_t start=ring.size();
      for(int i:ids) {while(ring.size()>=start+2&&orient(ring[ring.size()-2],ring.back(),i)<=tol) ring.pop_back();ring.push_back(i);}
      ring.pop_back(); std::reverse(ids.begin(),ids.end());
    }
    for(size_t i=1;i+1<ring.size();++i) triangles.push_back({ring[0],ring[i],ring[i+1]});
  } else {
    V inside=mul(add(add(p[a],p[b]),add(p[c],p[d])),0.25);
    std::vector<HullFace> fs{hull_face(a,b,c,p,inside),hull_face(a,d,b,p,inside),hull_face(a,c,d,p,inside),hull_face(b,d,c,p,inside)};
    auto link_faces=[&](size_t start) {
      std::map<std::pair<int,int>,std::pair<int,int>> unmatched;
      for(size_t i=start;i<fs.size();++i) for(int j=0;j<3;++j) {
        if(fs[i].neighbor[j]>=0) continue;
        int u=fs[i].ids[j],v=fs[i].ids[(j+1)%3]; auto key=std::minmax(u,v);
        auto hit=unmatched.find(key);
        if(hit==unmatched.end()) unmatched[key]={i,j};
        else {
          int k=hit->second.first,e=hit->second.second;
          fs[i].neighbor[j]=k;fs[k].neighbor[e]=i;unmatched.erase(hit);
        }
      }
      if(!unmatched.empty()) Rcpp::stop("Hull horizon must form a closed cone.");
    };
    link_faces(0);
    for(int i=0;i<n;++i) {
      int face=-1; double far=tol;
      for(int j=0;j<4;++j) {double v=distance(fs[j],p[i]);if(v>far){far=v;face=j;}}
      if(face>=0) fs[face].outside.push_back(i);
    }
    std::vector<size_t> seen(4,0),visible(4,0); size_t generation=0;
    for(size_t seed=0;seed<fs.size();++seed) {
      if(!fs[seed].alive||fs[seed].outside.empty()) continue;
      Rcpp::checkUserInterrupt();
      int eye=fs[seed].outside[0]; double far=-1;
      for(int i:fs[seed].outside) {double v=distance(fs[seed],p[i]);if(v>far){far=v;eye=i;}}
      ++generation; seen.resize(fs.size(),0);visible.resize(fs.size(),0);
      std::vector<int> stack{static_cast<int>(seed)},removed,candidates;
      while(!stack.empty()) {
        int id=stack.back();stack.pop_back();if(seen[id]==generation) continue;seen[id]=generation;
        if(!fs[id].alive||distance(fs[id],p[eye])<=tol) continue;
        visible[id]=generation;removed.push_back(id);
        for(int neighbor:fs[id].neighbor) if(neighbor>=0) stack.push_back(neighbor);
      }
      struct Horizon {int a,b,neighbor;};std::vector<Horizon> horizon;
      for(int id:removed) {
        auto& f=fs[id]; f.alive=false;
        candidates.insert(candidates.end(),f.outside.begin(),f.outside.end());
        std::vector<int>().swap(f.outside);
        for(int j=0;j<3;++j) if(visible[f.neighbor[j]]!=generation)
          horizon.push_back({f.ids[j],f.ids[(j+1)%3],f.neighbor[j]});
      }
      size_t start=fs.size();
      for(auto e:horizon) {
        int id=fs.size();fs.push_back(hull_face(e.a,e.b,eye,p,inside));
        for(int j=0;j<3;++j) {
          auto key=std::minmax(fs[id].ids[j],fs[id].ids[(j+1)%3]);
          if(key!=std::minmax(e.a,e.b)) continue;
          fs[id].neighbor[j]=e.neighbor;
          for(int k=0;k<3;++k) if(std::minmax(fs[e.neighbor].ids[k],fs[e.neighbor].ids[(k+1)%3])==key)
            fs[e.neighbor].neighbor[k]=id;
        }
      }
      link_faces(start);
      for(int i:candidates) if(i!=eye) {
        int face=-1; double far=tol;
        for(size_t j=start;j<fs.size();++j) {double v=distance(fs[j],p[i]);if(v>far){far=v;face=j;}}
        if(face>=0) fs[face].outside.push_back(i);
      }
    }
    for(auto& f:fs) if(f.alive) triangles.push_back(f.ids);
  }
  Rcpp::IntegerMatrix result(triangles.size(),3);
  for(size_t i=0;i<triangles.size();++i) {
    auto t=triangles[i]; int start=shortest_edge(p[t[0]],p[t[1]],p[t[2]]);
    for(int j=0;j<3;++j) result(i,j)=order[t[(start+j)%3]]+1;
  }
  return result;
}
}
#endif
