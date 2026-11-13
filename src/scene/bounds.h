#pragma once
#include "common.h"

namespace ggplot3d { namespace engine {
using Bounds2 = std::array<double,4>;

static inline bool bounds_overlap(const Bounds2& a,const Bounds2& b) {
  return a[0]<=b[2] && b[0]<=a[2] && a[1]<=b[3] && b[1]<=a[3];
}

template<class Entry> class BoundsIndex {
  struct Node {Bounds2 bounds;std::size_t start,count;int left=-1,right=-1;};
  std::vector<Node> nodes;
  std::vector<std::size_t> ids;
  const std::vector<Entry>* entries=nullptr;

  int build(std::size_t start,std::size_t count) {
    Bounds2 bounds{R_PosInf,R_PosInf,R_NegInf,R_NegInf};
    for(std::size_t i=start;i<start+count;++i) for(int k=0;k<2;++k) {
      bounds[k]=std::min(bounds[k],(*entries)[ids[i]].bounds[k]);
      bounds[k+2]=std::max(bounds[k+2],(*entries)[ids[i]].bounds[k+2]);
    }
    int node=nodes.size();nodes.push_back({bounds,start,count});
    if(count>8) {
      int axis=bounds[2]-bounds[0]>bounds[3]-bounds[1] ? 0:1;
      std::size_t half=count/2;
      std::nth_element(ids.begin()+start,ids.begin()+start+half,ids.begin()+start+count,[&](std::size_t a,std::size_t b) {
        double x=(*entries)[a].bounds[axis]+(*entries)[a].bounds[axis+2];
        double y=(*entries)[b].bounds[axis]+(*entries)[b].bounds[axis+2];
        return x==y ? a<b : x<y;
      });
      int left=build(start,half),right=build(start+half,count-half);
      nodes[node].left=left;nodes[node].right=right;
    }
    return node;
  }
public:
  BoundsIndex()=default;
  explicit BoundsIndex(const std::vector<Entry>& source) {reset(source);}
  void reset(const std::vector<Entry>& source) {
    entries=&source;nodes.clear();ids.resize(source.size());
    std::iota(ids.begin(),ids.end(),0);
    if(!ids.empty()) build(0,ids.size());
  }
  template<class F> void query(const Bounds2& bounds,std::size_t minimum,F visit) const {
    if(nodes.empty()) return;
    std::vector<int> stack{0};
    while(!stack.empty()) {
      const auto& node=nodes[stack.back()];stack.pop_back();
      if(!bounds_overlap(bounds,node.bounds)) continue;
      if(node.left>=0) {stack.push_back(node.right);stack.push_back(node.left);}
      else for(std::size_t j=node.start;j<node.start+node.count;++j)
        if(ids[j]>=minimum && bounds_overlap(bounds,(*entries)[ids[j]].bounds)) visit(ids[j]);
    }
  }
};
} }
