#ifndef GGPLOT3D_GEOMETRY_PRINTING_H
#define GGPLOT3D_GEOMETRY_PRINTING_H
#include "vector.h"
#include "predicates.h"
#include <numeric>
#include <queue>
namespace ggplot3d_geometry {
namespace printing {
using Tri=std::array<int,3>;
using Edge=std::pair<int,int>;

static inline bool inside2(V p,const std::array<V,3>& t,int axis) {
  auto project=[&](V a) { return std::array<double,2>{a[(axis+1)%3],a[(axis+2)%3]}; };
  auto q=project(p); int positive=0,negative=0;
  for(int k=0;k<3;++k) { auto a=project(t[k]),b=project(t[(k+1)%3]); int s=predicates::orient2d(a.data(),b.data(),q.data()); positive+=s>0;negative+=s<0; }
  return positive==0||negative==0;
}
static inline bool on_segment2(V p,V a,V b,int axis) {
  double x[2]={p[(axis+1)%3],p[(axis+2)%3]},y[2]={a[(axis+1)%3],a[(axis+2)%3]},z[2]={b[(axis+1)%3],b[(axis+2)%3]};
  if(predicates::orient2d(y,z,x)) return false;
  return x[0]>=std::min(y[0],z[0])&&x[0]<=std::max(y[0],z[0])&&x[1]>=std::min(y[1],z[1])&&x[1]<=std::max(y[1],z[1]);
}
static inline bool proper_cross2(V a,V b,V c,V d,int axis) {
  auto project=[&](V p) { return std::array<double,2>{p[(axis+1)%3],p[(axis+2)%3]}; };
  auto p=project(a),q=project(b),r=project(c),s=project(d);
  return predicates::orient2d(p.data(),q.data(),r.data())*predicates::orient2d(p.data(),q.data(),s.data())<0&&
    predicates::orient2d(r.data(),s.data(),p.data())*predicates::orient2d(r.data(),s.data(),q.data())<0;
}
static inline int projection_axis(const std::array<V,3>& t) {
  V n=cross(sub(t[1],t[0]),sub(t[2],t[0]));int axis=0;
  for(int k=1;k<3;++k) if(std::abs(n[k])>std::abs(n[axis])) axis=k;
  return axis;
}
static inline bool segment_triangle(V a,V b,int ia,int ib,const std::array<V,3>& t,Tri ids) {
  auto shared=[&](int i) { return std::find(ids.begin(),ids.end(),i)!=ids.end(); };
  int sa=predicates::orient3d(t[0].data(),t[1].data(),t[2].data(),a.data());
  int sb=predicates::orient3d(t[0].data(),t[1].data(),t[2].data(),b.data());
  if(sa*sb>0) return false;
  if(sa==0&&sb==0) {
    int axis=projection_axis(t);
    if(!shared(ia)&&inside2(a,t,axis)) return true;
    if(!shared(ib)&&inside2(b,t,axis)) return true;
    for(int k=0;k<3;++k) {
      if(ids[k]!=ia&&ids[k]!=ib&&on_segment2(t[k],a,b,axis)) return true;
      if(proper_cross2(a,b,t[k],t[(k+1)%3],axis)) return true;
    }
    return false;
  }
  if((sa==0&&shared(ia))||(sb==0&&shared(ib))) return false;
  int positive=0,negative=0;
  for(int k=0;k<3;++k) {
    int s=predicates::orient3d(a.data(),b.data(),t[k].data(),t[(k+1)%3].data());
    positive+=s>0;negative+=s<0;
  }
  return positive==0||negative==0;
}
static inline bool intersects(const std::vector<V>& p,Tri a,Tri b) {
  std::array<V,3> x{p[a[0]],p[a[1]],p[a[2]]},y{p[b[0]],p[b[1]],p[b[2]]};
  for(int k=0;k<3;++k) {
    if(segment_triangle(x[k],x[(k+1)%3],a[k],a[(k+1)%3],y,b)) return true;
    if(segment_triangle(y[k],y[(k+1)%3],b[k],b[(k+1)%3],x,a)) return true;
  }
  return false;
}

struct Bounds { V low,high; };
static inline bool overlap(Bounds a,Bounds b) {
  for(int k=0;k<3;++k) if(a.low[k]>b.high[k]||b.low[k]>a.high[k]) return false;
  return true;
}
struct Tree {
  struct Node { Bounds box;int begin,end,left=-1,right=-1; };
  std::vector<Node> nodes;std::vector<int> order;std::vector<Bounds> boxes;
  const std::vector<V>& p;const std::vector<Tri>& tri;
  Tree(const std::vector<V>& p_,const std::vector<Tri>& t):p(p_),tri(t) {
    order.resize(tri.size());std::iota(order.begin(),order.end(),0);boxes.resize(tri.size());
    for(size_t i=0;i<tri.size();++i) for(int k=0;k<3;++k) {
      boxes[i].low[k]=std::min({p[tri[i][0]][k],p[tri[i][1]][k],p[tri[i][2]][k]});
      boxes[i].high[k]=std::max({p[tri[i][0]][k],p[tri[i][1]][k],p[tri[i][2]][k]});
    }
    if(!order.empty()) build(0,order.size());
  }
  int build(int begin,int end) {
    Bounds box=boxes[order[begin]];
    for(int i=begin+1;i<end;++i) for(int k=0;k<3;++k) { box.low[k]=std::min(box.low[k],boxes[order[i]].low[k]);box.high[k]=std::max(box.high[k],boxes[order[i]].high[k]); }
    int at=nodes.size();nodes.push_back({box,begin,end});
    if(end-begin>8) {
      int axis=0;for(int k=1;k<3;++k) if(box.high[k]-box.low[k]>box.high[axis]-box.low[axis]) axis=k;
      int mid=(begin+end)/2;
      std::nth_element(order.begin()+begin,order.begin()+mid,order.begin()+end,[&](int a,int b) { return boxes[a].low[axis]+boxes[a].high[axis]<boxes[b].low[axis]+boxes[b].high[axis]; });
      int left=build(begin,mid),right=build(mid,end);nodes[at].left=left;nodes[at].right=right;
    }
    return at;
  }
  void query(int i,int node,const std::vector<int>& component,bool cross_components) const {
    const Node& n=nodes[node];if(!overlap(boxes[i],n.box)) return;
    if(n.left>=0) { query(i,n.left,component,cross_components);query(i,n.right,component,cross_components);return; }
    for(int k=n.begin;k<n.end;++k) {
      int j=order[k];if(j<=i||(!cross_components&&component[i]!=component[j])||!overlap(boxes[i],boxes[j])) continue;
      if(intersects(p,tri[i],tri[j])) Rcpp::stop("self-intersection between faces %d and %d",i+1,j+1);
    }
  }
};

struct Topology {
  std::vector<int> component,boundary_face;
  std::vector<std::vector<int>> groups;
  std::vector<bool> closed;std::vector<Edge> boundary;
  explicit Topology(const std::vector<Tri>& tri) {
    std::map<Edge,std::vector<std::pair<int,int>>> edges;
    for(size_t i=0;i<tri.size();++i) for(int k=0;k<3;++k) {
      int a=tri[i][k],b=tri[i][(k+1)%3];edges[std::minmax(a,b)].push_back({i,a<b?1:-1});
    }
    std::vector<std::vector<int>> adjacent(tri.size());
    for(const auto& entry:edges) {
      const auto& e=entry.second;
      if(e.size()>2) Rcpp::stop("non-manifold edge belongs to more than two faces");
      if(e.size()==2) {
        if(e[0].second==e[1].second) Rcpp::stop("inconsistent face winding");
        adjacent[e[0].first].push_back(e[1].first);adjacent[e[1].first].push_back(e[0].first);
      }
    }
    component.assign(tri.size(),-1);
    for(size_t i=0;i<tri.size();++i) if(component[i]<0) {
      int group=groups.size();groups.emplace_back();std::queue<int> queue;queue.push(i);component[i]=group;
      while(!queue.empty()) { int j=queue.front();queue.pop();groups.back().push_back(j);
        for(int k:adjacent[j]) if(component[k]<0) { component[k]=group;queue.push(k); }
      }
    }
    closed.assign(groups.size(),true);
    for(const auto& entry:edges) if(entry.second.size()==1) {
      int i=entry.second[0].first;closed[component[i]]=false;
      int a=entry.first.first,b=entry.first.second;if(entry.second[0].second<0) std::swap(a,b);boundary.push_back({a,b});boundary_face.push_back(i);
    }
  }
};

struct Mesh {
  std::vector<V> p;std::vector<Tri> tri;std::vector<int> source,component;std::vector<Edge> boundary;
  std::vector<int> boundary_face;std::vector<std::vector<int>> groups;std::vector<bool> closed;std::vector<double> volume;
  Mesh(Rcpp::NumericMatrix position,Rcpp::IntegerMatrix faces,bool cross_components,Rcpp::IntegerVector identity) {
    if(position.ncol()!=3||faces.ncol()!=3) Rcpp::stop("positions and triangles need three columns");
    if(identity.size()&&identity.size()!=position.nrow()) Rcpp::stop("vertex identity has the wrong length");
    std::vector<V> raw(position.nrow());std::vector<Tri> input;
    for(int i=0;i<position.nrow();++i) {
      raw[i]={position(i,0),position(i,1),position(i,2)};
      for(double x:raw[i]) if(!std::isfinite(x)) Rcpp::stop("non-finite vertex");
    }
    for(int i=0;i<faces.nrow();++i) {
      Tri t;for(int k=0;k<3;++k) { int j=faces(i,k)-1;if(j<0||j>=position.nrow()) Rcpp::stop("triangle index outside its vertex table");t[k]=j; }
      if(norm(cross(sub(raw[t[1]],raw[t[0]]),sub(raw[t[2]],raw[t[0]])))==0) continue;
      input.push_back(t);source.push_back(i);
    }
    if(input.empty()) Rcpp::stop("layer has no non-degenerate triangles");
    std::vector<int> region(input.size(),0);
    if(identity.size()) {
      std::vector<Tri> indexed=input;
      for(Tri& t:indexed) for(int& j:t) j=identity[j];
      Topology original(indexed);
      for(size_t i=0;i<region.size();++i) if(original.closed[original.component[i]]) region[i]=original.component[i]+1;
    }
    std::map<std::pair<int,V>,int> unique;
    for(size_t i=0;i<input.size();++i) {
      Tri t;
      for(int k=0;k<3;++k) {
        V v=raw[input[i][k]];auto entry=unique.emplace(std::make_pair(region[i],v),p.size());
        if(entry.second) p.push_back(v);
        t[k]=entry.first->second;
      }
      tri.push_back(t);
    }
    Topology topology(tri);
    component=std::move(topology.component);groups=std::move(topology.groups);closed=std::move(topology.closed);
    boundary=std::move(topology.boundary);boundary_face=std::move(topology.boundary_face);volume.assign(groups.size(),0);
    for(size_t g=0;g<groups.size();++g) {
      V origin=p[tri[groups[g][0]][0]];long double sum=0;
      for(int i:groups[g]) {
        Tri t=tri[i];
        sum+=static_cast<long double>(dot(sub(p[t[0]],origin),cross(sub(p[t[1]],origin),sub(p[t[2]],origin))))/6;
      }
      volume[g]=sum;
      if(closed[g]&&sum==0) Rcpp::stop("closed component has zero volume");
    }
    Tree tree(p,tri);for(size_t i=0;i<tri.size();++i) { if(i%1024==0) Rcpp::checkUserInterrupt();tree.query(i,0,component,cross_components); }
  }
};
}
}
#endif
