#ifndef GGPLOT3D_GEOMETRY_NORMALS_H
#define GGPLOT3D_GEOMETRY_NORMALS_H
#include "dual.h"
#include <queue>
namespace ggplot3d_geometry {
namespace normals {

// Balanced k-d tree over points: each node splits its range of the index
// array at the median of its widest coordinate; ranges of at most 8 points
// are leaves.
struct KdTree {
  const std::vector<V>& p; std::vector<int> idx; std::vector<char> axis;
  explicit KdTree(const std::vector<V>& points): p(points),idx(points.size()),axis(points.size(),0) {
    std::iota(idx.begin(),idx.end(),0);
    build(0,idx.size());
  }
  void build(size_t lo,size_t hi) {
    if(hi-lo<=8) return;
    V a=p[idx[lo]],b=a;
    for(size_t i=lo;i<hi;++i) for(int c=0;c<3;++c) { a[c]=std::min(a[c],p[idx[i]][c]); b[c]=std::max(b[c],p[idx[i]][c]); }
    int ax=0; for(int c=1;c<3;++c) if(b[c]-a[c]>b[ax]-a[ax]) ax=c;
    size_t mid=(lo+hi)/2;
    std::nth_element(idx.begin()+lo,idx.begin()+mid,idx.begin()+hi,[&](int x,int y) { return p[x][ax]<p[y][ax]; });
    axis[mid]=ax;
    build(lo,mid); build(mid+1,hi);
  }
  // The k points nearest to q (q itself included when it is a data point),
  // nearest first.
  void nearest(const V& q,int k,std::vector<int>& out) const {
    std::priority_queue<std::pair<double,int>> heap;
    search(q,k,0,idx.size(),heap);
    out.resize(heap.size());
    for(int i=heap.size()-1;i>=0;--i) { out[i]=heap.top().second; heap.pop(); }
  }
  void consider(const V& q,int k,int i,std::priority_queue<std::pair<double,int>>& heap) const {
    V d=sub(p[i],q); double d2=dot(d,d);
    if(static_cast<int>(heap.size())<k) heap.push({d2,i});
    else if(d2<heap.top().first) { heap.pop(); heap.push({d2,i}); }
  }
  void search(const V& q,int k,size_t lo,size_t hi,std::priority_queue<std::pair<double,int>>& heap) const {
    if(hi-lo<=8) { for(size_t i=lo;i<hi;++i) consider(q,k,idx[i],heap); return; }
    size_t mid=(lo+hi)/2; int ax=axis[mid]; double gap=q[ax]-p[idx[mid]][ax];
    consider(q,k,idx[mid],heap);
    if(gap<0) search(q,k,lo,mid,heap); else search(q,k,mid+1,hi,heap);
    if(static_cast<int>(heap.size())<k||gap*gap<heap.top().first) {
      if(gap<0) search(q,k,mid+1,hi,heap); else search(q,k,lo,mid,heap);
    }
  }
};

// Unit normals of points from the least principal axis of each point and
// its k nearest neighbours, oriented consistently by propagation along a
// minimum spanning tree of the symmetrised neighbour graph weighted by
// 1 - |n_i . n_j| (Hoppe, DeRose, Duchamp, McDonald and Stuetzle 1992). Each
// tree of the forest starts at its highest point with an upward normal, so a
// closed surface gets outward normals.
static inline std::vector<V> estimate(const std::vector<V>& points,int k) {
  int n=points.size();
  k=std::min(k,n-1);
  KdTree tree(points);
  std::vector<V> normal(n);
  std::vector<std::vector<int>> graph(n);
  std::vector<int> near;
  for(int i=0;i<n;++i) {
    if(i%4096==0) Rcpp::checkUserInterrupt();
    tree.nearest(points[i],k+1,near);
    V mean{0,0,0};
    for(int j:near) mean=add(mean,points[j]);
    mean=mul(mean,1.0/near.size());
    double a[3][3]={{0,0,0},{0,0,0},{0,0,0}};
    for(int j:near) { V d=sub(points[j],mean); for(int r=0;r<3;++r) for(int c=0;c<3;++c) a[r][c]+=d[r]*d[c]; }
    double w[3],q[3][3]; dual::symmetric_eigen(a,w,q);
    int least=0; for(int c=1;c<3;++c) if(w[c]<w[least]) least=c;
    normal[i]=V{q[0][least],q[1][least],q[2][least]};
    for(int j:near) if(j!=i) { graph[i].push_back(j); graph[j].push_back(i); }
  }
  std::vector<int> order(n); std::iota(order.begin(),order.end(),0);
  std::sort(order.begin(),order.end(),[&](int a,int b) { return points[a][2]>points[b][2]; });
  std::vector<char> done(n,0);
  using Item=std::pair<double,std::pair<int,int>>;
  std::priority_queue<Item,std::vector<Item>,std::greater<Item>> heap;
  auto expand=[&](int v) {
    done[v]=1;
    for(int u:graph[v]) if(!done[u]) heap.push({1-std::abs(dot(normal[v],normal[u])),{u,v}});
  };
  for(int start:order) {
    if(done[start]) continue;
    if(normal[start][2]<0) normal[start]=mul(normal[start],-1);
    expand(start);
    while(!heap.empty()) {
      auto top=heap.top(); heap.pop();
      int v=top.second.first,parent=top.second.second;
      if(done[v]) continue;
      if(dot(normal[v],normal[parent])<0) normal[v]=mul(normal[v],-1);
      expand(v);
    }
  }
  return normal;
}
}
}
#endif
