#ifndef GGPLOT3D_GEOMETRY_DELAUNAY_H
#define GGPLOT3D_GEOMETRY_DELAUNAY_H
#include "vector.h"
#include "predicates.h"
#include <unordered_map>
namespace ggplot3d_geometry {

// Delaunay triangulation (D = 2) or tetrahedralisation (D = 3) by
// Bowyer-Watson insertion: the simplices whose circumball strictly contains
// the new point form a star-shaped cavity, which is replaced by the cone of
// its boundary facets over the point. The convex hull is closed by ghost
// simplices that share the point at infinity (stored last); a ghost
// conflicts with a point beyond its hull facet, or on the facet's affine hull
// and inside its circumball. Points are inserted in Morton order and located
// by a visibility walk from the last simplex created (Devillers, Pion and
// Teillaud 2002). Orientation and in-ball tests use exact predicates, so the
// result is the Delaunay triangulation of the exact input coordinates; ties
// among cospherical points are resolved by insertion order.
template<int D>
class Delaunay {
 public:
  using Simplex=std::array<int,D+1>;
  std::vector<V> points;
  std::vector<Simplex> vertex, neighbour;
  std::vector<char> alive;
  int infinite;

  explicit Delaunay(const std::vector<V>& input): points(input),infinite(input.size()) {
    int n=points.size();
    if(n>=(1<<21)) Rcpp::stop("Too many points for a Delaunay triangulation.");
    std::vector<int> order=morton_order();
    Simplex first;
    if(!initial(order,first)) return;
    for(int i=0;i<n;++i) {
      int p=order[i];
      if(std::find(first.begin(),first.end(),p)!=first.end()) continue;
      if(i%1024==0) Rcpp::checkUserInterrupt();
      insert(p);
    }
  }
  bool empty() const { return std::find(alive.begin(),alive.end(),1)==alive.end(); }
  bool ghost(int s) const { return vertex[s][D]==infinite; }

 private:
  int last=0,turn=0;
  const double* at(int v) const { return points[v].data(); }
  int orient(const Simplex& s) const {
    if constexpr(D==2) return predicates::orient2d(at(s[0]),at(s[1]),at(s[2]));
    else return predicates::orient3d(at(s[0]),at(s[1]),at(s[2]),at(s[3]));
  }
  // Three points are collinear when all three coordinate-plane projections are.
  bool collinear(int a,int b,int c) const {
    for(int k=0;k<3;++k) {
      int u=(k+1)%3,w=(k+2)%3;
      double pa[2]={points[a][u],points[a][w]},pb[2]={points[b][u],points[b][w]},pc[2]={points[c][u],points[c][w]};
      if(predicates::orient2d(pa,pb,pc)!=0) return false;
    }
    return true;
  }
  // Orientation of s with vertex j replaced by p.
  int orient_with(const Simplex& s,int j,int p) const { Simplex t=s; t[j]=p; return orient(t); }
  bool conflict(int s,int p) const {
    const Simplex& v=vertex[s];
    if(!ghost(s)) {
      if constexpr(D==2) return predicates::incircle(at(v[0]),at(v[1]),at(v[2]),at(p))>0;
      else return predicates::insphere(at(v[0]),at(v[1]),at(v[2]),at(v[3]),at(p))>0;
    }
    int side=orient_with(v,D,p);
    if(side!=0) return side>0;
    if constexpr(D==2) return between(v[0],v[1],p);
    else return in_facet_circle(v[0],v[1],v[2],p);
  }
  // p strictly inside the segment a-b of a collinear triple.
  bool between(int a,int b,int p) const {
    const V &A=points[a],&B=points[b],&P=points[p];
    int k=std::abs(B[0]-A[0])>=std::abs(B[1]-A[1])?0:1;
    return (P[k]-A[k])*(P[k]-B[k])<0;
  }
  // p, coplanar with a, b, c, strictly inside their circumcircle: the sphere
  // through a, b, c and any point q off their plane meets the plane in it.
  bool in_facet_circle(int a,int b,int c,int p) const {
    V q=points[a]; double reach=1;
    for(int k=0;k<3;++k) reach=std::max(reach,std::abs(points[a][k])+std::abs(points[b][k])+std::abs(points[c][k]));
    for(int k=0;k<3;++k) {
      V off=q; off[k]+=reach;
      int o=predicates::orient3d(at(a),at(b),at(c),off.data());
      if(o==0) continue;
      if(o<0) std::swap(a,b);
      return predicates::insphere(at(a),at(b),at(c),off.data(),at(p))>0;
    }
    return false;
  }
  std::vector<int> morton_order() const {
    int n=points.size(); V lo=points.empty()?V{0,0,0}:points[0],hi=lo;
    for(const V& p:points) for(int k=0;k<D;++k) { lo[k]=std::min(lo[k],p[k]); hi[k]=std::max(hi[k],p[k]); }
    int bits=D==2?16:10;
    std::vector<unsigned long long> code(n);
    for(int i=0;i<n;++i) {
      unsigned long long c=0;
      unsigned q[3]={0,0,0};
      for(int k=0;k<D;++k) q[k]=hi[k]>lo[k]?unsigned((points[i][k]-lo[k])/(hi[k]-lo[k])*((1u<<bits)-1)):0;
      for(int b=bits-1;b>=0;--b) for(int k=0;k<D;++k) c=(c<<1)|((q[k]>>b)&1u);
      code[i]=c;
    }
    std::vector<int> order(n); std::iota(order.begin(),order.end(),0);
    std::stable_sort(order.begin(),order.end(),[&](int a,int b) { return code[a]<code[b]; });
    return order;
  }
  int create(const Simplex& v) {
    vertex.push_back(v); Simplex none; none.fill(-1); neighbour.push_back(none); alive.push_back(1);
    return vertex.size()-1;
  }
  static unsigned long long facet_key(Simplex v,int skip) {
    int ids[D],m=0;
    for(int j=0;j<=D;++j) if(j!=skip) ids[m++]=v[j];
    std::sort(ids,ids+D);
    unsigned long long key=0;
    for(int j=0;j<D;++j) key=(key<<21)|unsigned(ids[j]);
    return key;
  }
  // Joins the unset neighbours of the given simplices across shared facets.
  void link(const std::vector<int>& made) {
    std::unordered_map<unsigned long long,std::pair<int,int>> open;
    for(int s:made) for(int j=0;j<=D;++j) if(neighbour[s][j]<0) {
      unsigned long long key=facet_key(vertex[s],j);
      auto found=open.find(key);
      if(found==open.end()) open.emplace(key,std::make_pair(s,j));
      else { neighbour[s][j]=found->second.first; neighbour[found->second.first][found->second.second]=s; open.erase(found); }
    }
  }
  // First simplex from D + 1 affinely independent points, with the ghosts
  // over each of its facets.
  bool initial(const std::vector<int>& order,Simplex& first) {
    int n=order.size();
    if(n<D+1) return false;
    first.fill(-1); first[0]=order[0]; int m=1;
    for(int i=1;i<n&&m<=D;++i) {
      Simplex s=first; s[m]=order[i];
      bool independent;
      if(m==1) independent=points[s[0]]!=points[s[1]];
      else if(m<D) independent=!collinear(s[0],s[1],s[2]);
      else independent=orient(s)!=0;
      if(independent) { first[m]=order[i]; ++m; }
    }
    if(m<=D) return false;
    if(orient(first)<0) std::swap(first[0],first[1]);
    int root=create(first); std::vector<int> made{root};
    for(int i=0;i<=D;++i) {
      Simplex g; int k=0;
      for(int j=0;j<=D;++j) if(j!=i) g[k++]=first[j];
      if((D-i)%2==0) std::swap(g[0],g[1]);
      g[D]=infinite;
      int s=create(g); neighbour[s][D]=root; neighbour[root][i]=s; made.push_back(s);
    }
    link(made);
    last=root;
    return true;
  }
  int locate(int p) {
    int s=last;
    if(!alive[s]) s=std::find(alive.begin(),alive.end(),1)-alive.begin();
    if(ghost(s)) { if(conflict(s,p)) return s; s=neighbour[s][D]; }
    for(size_t steps=0;steps<4*vertex.size()+16;++steps) {
      int next=-1; ++turn;
      for(int r=0;r<=D;++r) {
        int j=(r+turn)%(D+1);
        if(orient_with(vertex[s],j,p)<0) { next=neighbour[s][j]; break; }
      }
      if(next<0) return s;
      s=next;
      if(ghost(s)) return s;
    }
    for(size_t s2=0;s2<vertex.size();++s2) if(alive[s2]&&conflict(s2,p)) return s2;
    return -1;
  }
  void insert(int p) {
    int start=locate(p);
    if(start<0) return;
    for(int j=0;j<=D;++j) if(vertex[start][j]!=infinite&&points[vertex[start][j]]==points[p]) return;
    if(!conflict(start,p)) {
      bool found=false;
      for(int j=0;j<=D&&!found;++j) { int u=neighbour[start][j]; if(conflict(u,p)) { start=u; found=true; } }
      if(!found) return;
    }
    std::vector<int> cavity{start},stack{start};
    std::unordered_map<int,char> state{{start,1}};
    std::vector<std::pair<int,int>> boundary;
    while(!stack.empty()) {
      int s=stack.back(); stack.pop_back();
      for(int j=0;j<=D;++j) {
        int u=neighbour[s][j];
        auto it=state.find(u);
        if(it==state.end()) {
          char in=conflict(u,p)?1:2;
          state.emplace(u,in);
          if(in==1) { cavity.push_back(u); stack.push_back(u); }
          else boundary.push_back({s,j});
        } else if(it->second==2) boundary.push_back({s,j});
      }
    }
    std::vector<int> made;
    for(auto& b:boundary) {
      int s=b.first,j=b.second,u=neighbour[s][j];
      Simplex v=vertex[s]; v[j]=p;
      int t=create(v); neighbour[t][j]=u;
      for(int k=0;k<=D;++k) if(neighbour[u][k]==s) neighbour[u][k]=t;
      made.push_back(t);
    }
    for(int s:cavity) alive[s]=0;
    link(made);
    last=made.back();
  }
};
}
#endif
