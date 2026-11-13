#ifndef GGPLOT3D_GEOMETRY_OCTREE_H
#define GGPLOT3D_GEOMETRY_OCTREE_H
#include "dual.h"
#include <unordered_map>
namespace ggplot3d_geometry {
namespace octree {

using P3=std::array<int,3>;
using Key=long long;

struct PairHash {
  size_t operator()(const std::pair<Key,Key>& k) const {
    return std::hash<Key>()(k.first)^(std::hash<Key>()(k.second)+0x9e3779b97f4a7c15ULL+(std::hash<Key>()(k.first)<<6));
  }
};
using PairMap=std::unordered_map<std::pair<Key,Key>,int,PairHash>;

// Field values at points.
using ValueFunction=std::function<void(const std::vector<V>&,std::vector<double>&)>;
// Positions of the level on segments a-b whose endpoint values fa, fb bracket it.
using CrossingFunction=std::function<void(const std::vector<V>&,const std::vector<V>&,const std::vector<double>&,
                                          const std::vector<double>&,double,std::vector<V>&)>;

static constexpr int pow3[3]={1,3,9};

// A minimal face: a square of side m at lattice corner p, normal to axis, that
// no smaller cell subdivides. cycle runs around its boundary through the ends
// of its minimal edges; corner[k] is the position of its k-th corner in cycle.
struct Face { P3 p; int axis,m; std::vector<P3> cycle; int corner[4]; };

// Terminal boxes of an octree over a box, on a lattice of N = 2^depth
// intervals per axis: box l covers origin[l] + [0, size[l]]^3 in lattice
// units and the boxes partition the lattice. With caps, every box on the
// boundary is mirrored across each boundary face it touches; the mirrored
// cells form a padding layer whose outer lattice points lie above every level
// and map onto the box face, as the padding layer of the uniform grid does.
// A cell is a box and a reflection code (per axis 0 none, 1 low, 2 high face).
struct Tree {
  int N; V lo,hi; bool caps;
  std::vector<P3> origin; std::vector<int> size;
  std::unordered_map<Key,int> boxes;
  Tree(int depth,V lo,V hi,bool caps,const std::vector<P3>& origin,const std::vector<int>& size):
    N(1<<depth),lo(lo),hi(hi),caps(caps),origin(origin),size(size) {
    double volume=0;
    for(size_t l=0;l<origin.size();++l) {
      int s=size[l];
      if(s<1||(s&(s-1))||s>N) Rcpp::stop("Octree box sizes must be powers of two within the lattice.");
      for(int a=0;a<3;++a) if(origin[l][a]<0||origin[l][a]%s||origin[l][a]+s>N) Rcpp::stop("Octree boxes must be aligned to their size.");
      if(!boxes.emplace(box_key(s,origin[l]),l).second) Rcpp::stop("Octree boxes must be distinct.");
      volume+=std::pow(double(s),3);
    }
    if(volume!=std::pow(double(N),3)) Rcpp::stop("Octree boxes must partition the root box.");
  }
  Key box_key(int s,const P3& o) const { Key w=N+1; return ((Key(s)*w+o[0])*w+o[1])*w+o[2]; }
  Key point_key(const P3& p) const { Key w=4*Key(N)+3; return (Key(p[0]+N+1)*w+p[1]+N+1)*w+p[2]+N+1; }
  Key edge_key(const P3& p,int axis) const { return point_key(p)*3+axis; }
  Key face_key(const P3& p,int axis) const { return point_key(p)*3+axis; }
  bool outside(const P3& p) const { for(int a=0;a<3;++a) if(p[a]<0||p[a]>N) return true; return false; }
  V position(const P3& p) const {
    V x; for(int a=0;a<3;++a) x[a]=lo[a]+(hi[a]-lo[a])*std::min(std::max(p[a],0),N)/double(N);
    return x;
  }
  int cell_size(int cell) const { return size[cell/27]; }
  // Cell containing the point q, in doubled lattice coordinates; -1 outside
  // the root box (and its padding layer with caps).
  int locate(const P3& q) const {
    P3 r=q; int code=0;
    for(int a=0;a<3;++a) {
      if(q[a]<0) { r[a]=-q[a]; code+=pow3[a]; }
      else if(q[a]>2*N) { r[a]=4*N-q[a]; code+=2*pow3[a]; }
      if(r[a]<=0||r[a]>=2*N) return -1;
    }
    if(code&&!caps) return -1;
    P3 c{r[0]>>1,r[1]>>1,r[2]>>1};
    for(int s=1;s<=N;s<<=1) {
      P3 o{c[0]&~(s-1),c[1]&~(s-1),c[2]&~(s-1)};
      auto it=boxes.find(box_key(s,o));
      if(it!=boxes.end()) return it->second*27+code;
    }
    return -1;
  }
  void box(int cell,P3& o,int& s) const {
    int l=cell/27,code=cell%27; o=origin[l]; s=size[l];
    for(int a=0;a<3;++a) { int d=(code/pow3[a])%3; if(d==1) o[a]=-o[a]-s; else if(d==2) o[a]=2*N-o[a]-s; }
  }
  // An edge of length m is minimal when every cell around it is at least as
  // large, so no cell corner lies inside it (Ju et al. 2002).
  bool minimal_edge(const P3& p,int axis,int m) const {
    int u=(axis+1)%3,v=(axis+2)%3;
    for(int k=0;k<4;++k) {
      P3 q{2*p[0],2*p[1],2*p[2]}; q[axis]+=m; q[u]+=(k&1)?1:-1; q[v]+=(k&2)?1:-1;
      int c=locate(q);
      if(c>=0&&cell_size(c)<m) return false;
    }
    return true;
  }
  // Ends of the minimal edges along the segment from p over m lattice units
  // of axis, without the far end. Segments outside the root box hold no
  // crossing and stay whole.
  void segment(const P3& p,int axis,int m,std::vector<P3>& out) const {
    P3 q=p; q[axis]+=m;
    if(m==1||(outside(p)&&outside(q))||minimal_edge(p,axis,m)) { out.push_back(p); return; }
    P3 mid=p; mid[axis]+=m/2;
    segment(p,axis,m/2,out); segment(mid,axis,m/2,out);
  }
  // Minimal faces tiling the square of side m at corner p, normal to axis,
  // as seen from the cell on the other side of `side`.
  void tile(const P3& p,int axis,int side,int m,std::vector<Face>& out) const {
    int u=(axis+1)%3,v=(axis+2)%3;
    P3 c[4]={p,p,p,p}; c[1][u]+=m; c[2][u]+=m; c[2][v]+=m; c[3][v]+=m;
    if(outside(c[0])&&outside(c[1])&&outside(c[2])&&outside(c[3])) return;
    if(m>1) {
      P3 q{2*p[0],2*p[1],2*p[2]}; q[u]+=m; q[v]+=m; q[axis]+=side;
      int cell=locate(q);
      if(cell>=0&&cell_size(cell)<m) {
        int h=m/2;
        for(int k=0;k<4;++k) { P3 s=p; s[u]+=(k&1)*h; s[v]+=(k>>1)*h; tile(s,axis,side,h,out); }
        return;
      }
    }
    Face f; f.p=p; f.axis=axis; f.m=m;
    std::vector<P3> run;
    f.corner[0]=0; segment(c[0],u,m,f.cycle);
    f.corner[1]=f.cycle.size(); segment(c[1],v,m,f.cycle);
    f.corner[2]=f.cycle.size(); run.clear(); segment(c[3],u,m,run);
    f.cycle.push_back(c[2]); for(size_t i=run.size()-1;i>=1;--i) f.cycle.push_back(run[i]);
    f.corner[3]=f.cycle.size(); run.clear(); segment(c[0],v,m,run);
    f.cycle.push_back(c[3]); for(size_t i=run.size()-1;i>=1;--i) f.cycle.push_back(run[i]);
    out.push_back(f);
  }
  std::vector<Face> faces(int cell) const {
    P3 o; int s; box(cell,o,s); std::vector<Face> out;
    for(int axis=0;axis<3;++axis) for(int side=-1;side<=1;side+=2) {
      P3 p=o; if(side>0) p[axis]+=s;
      tile(p,axis,side,s,out);
    }
    return out;
  }
};

// Level-curve arcs on a minimal face, as pairs of positions in its cycle of
// the edges they join. v holds the cycle values relative to the level. Arcs
// separate the high runs of the cycle from each other, or the low runs when
// the high side is joined across the face: by the asymptotic decider when
// the face corners alternate in sign, otherwise when the mean of the corner
// values is not below the level.
static inline std::vector<std::pair<int,int>> face_arcs(const Face& f,const std::vector<double>& v) {
  int n=v.size(); std::vector<int> cut; std::vector<char> enter;
  for(int j=0;j<n;++j) {
    bool a=v[j]>=0,b=v[(j+1)%n]>=0;
    if(a!=b) { cut.push_back(j); enter.push_back(b); }
  }
  std::vector<std::pair<int,int>> arcs;
  int k=cut.size();
  if(k<2) return arcs;
  double c[4]; bool h[4];
  for(int a=0;a<4;++a) { c[a]=v[f.corner[a]]; h[a]=c[a]>=0; }
  bool joined=h[0]==h[2]&&h[1]==h[3]&&h[0]!=h[1] ? cubes::high_joined(c[0],c[1],c[2],c[3],h[0]) : c[0]+c[1]+c[2]+c[3]>=0;
  int start=0;
  while(start<k&&(enter[start]!=0)==joined) ++start;
  for(int i=0;i+1<k;i+=2) arcs.push_back({cut[(start+i)%k],cut[(start+i+1)%k]});
  return arcs;
}

struct Crossing { P3 a,b; int axis; double va,vb; V p,n; };

// Dual contouring on the cells of an octree (Ju, Losasso, Schaefer and
// Warren 2002): every sign-changing minimal edge gives one polygon through the
// vertices of the three or four cells around it, so cells of different sizes
// join without cracks. As in the uniform extraction, each cell carries one
// vertex per boundary loop of the level set on its minimal faces (manifold
// dual contouring, Schaefer, Ju and Warren 2007), and a second arc joining the
// same two cell vertices across a face gets a vertex of its own.
template<bool hermite>
static inline Mesh extract(const Tree& tree,const std::vector<char>& empty,const std::vector<double>& levels,
                           const ValueFunction& value,const CrossingFunction& crossing,const dual::GradientFunction& gradient) {
  std::vector<int> cells;
  int nb=tree.origin.size();
  for(int l=0;l<nb;++l) if(!empty[l]) cells.push_back(l*27);
  if(tree.caps) for(int l=0;l<nb;++l) {
    int options[3][3],count[3],s=tree.size[l];
    for(int a=0;a<3;++a) {
      count[a]=0; options[a][count[a]++]=0;
      if(tree.origin[l][a]==0) options[a][count[a]++]=1;
      if(tree.origin[l][a]+s==tree.N) options[a][count[a]++]=2;
    }
    for(int i=0;i<count[0];++i) for(int j=0;j<count[1];++j) for(int k=0;k<count[2];++k) {
      int code=options[0][i]+3*options[1][j]+9*options[2][k];
      if(code) cells.push_back(l*27+code);
    }
  }
  int nc=cells.size();
  std::vector<std::vector<Face>> faces(nc);
  for(int i=0;i<nc;++i) { if(i%1024==0) Rcpp::checkUserInterrupt(); faces[i]=tree.faces(cells[i]); }

  std::unordered_map<Key,double> values; std::vector<Key> keys; std::vector<V> points;
  for(const auto& list:faces) for(const Face& f:list) for(const P3& p:f.cycle) if(!tree.outside(p)) {
    Key key=tree.point_key(p);
    if(values.emplace(key,0).second) { keys.push_back(key); points.push_back(tree.position(p)); }
  }
  std::vector<double> sampled;
  if(!points.empty()) value(points,sampled);
  if(sampled.size()!=points.size()) Rcpp::stop("The value function must return one value per point.");
  double magnitude=0;
  for(size_t i=0;i<keys.size();++i) { values[keys[i]]=sampled[i]; if(std::isfinite(sampled[i])) magnitude=std::max(magnitude,std::abs(sampled[i])); }

  Mesh mesh;
  for(double level:levels) {
    Rcpp::checkUserInterrupt();
    Mesh out;
    auto relative=[&](const P3& p) {
      if(tree.outside(p)) return 1.0;
      double x=values.at(tree.point_key(p));
      return std::isfinite(x)?snap(x,level,magnitude)-level:NAN;
    };
    std::vector<std::vector<std::vector<double>>> cycle_values(nc);
    std::vector<char> bad(nc,0);
    std::unordered_map<Key,int> edge_index; std::vector<Crossing> edges;
    for(int i=0;i<nc;++i) {
      cycle_values[i].resize(faces[i].size());
      for(size_t f=0;f<faces[i].size();++f) {
        const Face& face=faces[i][f]; auto& v=cycle_values[i][f];
        for(const P3& p:face.cycle) { v.push_back(relative(p)); if(std::isnan(v.back())) bad[i]=1; }
      }
      if(bad[i]) continue;
      for(size_t f=0;f<faces[i].size();++f) {
        const auto& cycle=faces[i][f].cycle; const auto& v=cycle_values[i][f]; int n=cycle.size();
        for(int j=0;j<n;++j) {
          int k=(j+1)%n;
          if((v[j]>=0)==(v[k]>=0)) continue;
          int axis=0; while(cycle[j][axis]==cycle[k][axis]) ++axis;
          bool forward=cycle[j][axis]<cycle[k][axis];
          const P3& a=forward?cycle[j]:cycle[k]; const P3& b=forward?cycle[k]:cycle[j];
          Key key=tree.edge_key(a,axis);
          if(edge_index.emplace(key,edges.size()).second)
            edges.push_back({a,b,axis,forward?v[j]:v[k],forward?v[k]:v[j],V{0,0,0},V{0,0,0}});
        }
      }
    }
    std::vector<int> interior; std::vector<V> pa,pb,found; std::vector<double> fa,fb;
    for(size_t e=0;e<edges.size();++e) {
      Crossing& c=edges[e];
      bool hb=c.vb>=0; V direction{0,0,0}; direction[c.axis]=hb?1:-1;
      c.n=direction;
      if(tree.outside(c.a)||tree.outside(c.b)) { c.p=tree.position(tree.outside(c.a)?c.b:c.a); continue; }
      V A=tree.position(c.a),B=tree.position(c.b); double t=c.va/(c.va-c.vb);
      c.p=add(A,mul(sub(B,A),t));
      interior.push_back(e); pa.push_back(A); pb.push_back(B); fa.push_back(c.va+level); fb.push_back(c.vb+level);
    }
    if(crossing&&!interior.empty()) {
      crossing(pa,pb,fa,fb,level,found);
      if(found.size()!=interior.size()) Rcpp::stop("The crossing function must return one point per segment.");
      for(size_t r=0;r<interior.size();++r) edges[interior[r]].p=found[r];
    }
    if(gradient&&!interior.empty()) {
      std::vector<V> at,normals;
      for(int e:interior) at.push_back(edges[e].p);
      gradient(at,normals);
      for(size_t r=0;r<interior.size();++r) { V n=unit(normals[r]); if(norm(n)>0) edges[interior[r]].n=n; }
    }

    PairMap vertex;
    struct Arcs { Key key; int a,b; std::vector<std::pair<Key,Key>> arcs; };
    std::unordered_map<Key,int> face_index; std::vector<Arcs> arcs;
    for(int i=0;i<nc;++i) {
      if(bad[i]) continue;
      std::unordered_map<Key,int> local; std::vector<Key> ids; std::vector<std::pair<int,int>> joins;
      auto id=[&](Key key) { auto it=local.emplace(key,ids.size()); if(it.second) ids.push_back(key); return it.first->second; };
      for(size_t f=0;f<faces[i].size();++f) {
        const Face& face=faces[i][f]; const auto& v=cycle_values[i][f]; int n=face.cycle.size();
        auto key_of=[&](int j) {
          const P3& a=face.cycle[j]; const P3& b=face.cycle[(j+1)%n];
          int axis=0; while(a[axis]==b[axis]) ++axis;
          return tree.edge_key(a[axis]<b[axis]?a:b,axis);
        };
        auto found=face_arcs(face,v);
        if(found.empty()) continue;
        Key fk=tree.face_key(face.p,face.axis);
        auto it=face_index.emplace(fk,arcs.size());
        if(it.second) arcs.push_back({fk,cells[i],-1,{}});
        Arcs& record=arcs[it.first->second];
        bool first=record.a==cells[i];
        if(!first&&record.b<0) record.b=cells[i];
        for(auto& arc:found) {
          Key e1=key_of(arc.first),e2=key_of(arc.second);
          joins.push_back({id(e1),id(e2)});
          if(first) record.arcs.push_back({e1,e2});
        }
      }
      if(ids.empty()) continue;
      UnionFind loops(ids.size());
      for(auto& j:joins) loops.unite(j.first,j.second);
      P3 o; int s; tree.box(cells[i],o,s);
      P3 top{o[0]+s,o[1]+s,o[2]+s};
      V lo=tree.position(o),hi=tree.position(top);
      std::map<int,std::vector<int>> members;
      for(size_t k=0;k<ids.size();++k) members[loops.find(k)].push_back(k);
      for(auto& m:members) {
        std::vector<V> p,n;
        for(int k:m.second) { const Crossing& c=edges[edge_index.at(ids[k])]; p.push_back(c.p); n.push_back(c.n); }
        int v=out.add(dual::loop_vertex<hermite>(p,n,lo,hi));
        for(int k:m.second) vertex[{Key(cells[i]),ids[k]}]=v;
      }
    }

    std::map<std::pair<int,int>,int> first; PairMap split;
    for(const Arcs& record:arcs) {
      if(record.b<0) continue;
      for(auto& arc:record.arcs) {
        auto A=vertex.find({Key(record.a),arc.first}),B=vertex.find({Key(record.b),arc.first});
        if(A==vertex.end()||B==vertex.end()) continue;
        auto pair=std::minmax(A->second,B->second);
        if(first.emplace(pair,1).second) continue;
        int m=dual::midpoint_vertex(out,A->second,B->second);
        split[{record.key,arc.first}]=m; split[{record.key,arc.second}]=m;
      }
    }

    static const int du[4]={-1,1,1,-1},dv[4]={-1,-1,1,1};
    for(const Crossing& c:edges) {
      int axis=c.axis,u=(axis+1)%3,w=(axis+2)%3,m=c.b[axis]-c.a[axis];
      Key ek=tree.edge_key(c.a,axis);
      int around[4]; bool ok=true;
      for(int k=0;k<4&&ok;++k) {
        P3 q{2*c.a[0],2*c.a[1],2*c.a[2]}; q[axis]+=m; q[u]+=du[k]; q[w]+=dv[k];
        around[k]=tree.locate(q); ok=around[k]>=0;
      }
      if(!ok) continue;
      int start=0; while(start<4&&around[start]==around[(start+3)%4]) ++start;
      if(start==4) continue;
      std::vector<std::pair<int,int>> distinct;
      for(int step=0;step<4;++step) { int k=(start+step)%4; if(around[k]!=around[(k+3)%4]) distinct.push_back({around[k],k}); }
      std::vector<int> ids; int nd=distinct.size();
      for(int d=0;d<nd&&ok;++d) {
        auto found=vertex.find({Key(distinct[d].first),ek});
        if(found==vertex.end()) { ok=false; break; }
        ids.push_back(found->second);
        int next=distinct[(d+1)%nd].first,last=(distinct[(d+1)%nd].second+3)%4,plane=last%2==0?u:w;
        int smaller=tree.cell_size(next)<tree.cell_size(distinct[d].first)?next:distinct[d].first;
        P3 o; int s; tree.box(smaller,o,s); o[plane]=c.a[plane];
        auto between=split.find({tree.face_key(o,plane),ek});
        if(between!=split.end()) ids.push_back(between->second);
      }
      if(!ok) continue;
      bool hb=c.vb>=0; V normal{0,0,0}; normal[axis]=hb?1:-1;
      dual::polygon(out,ids,!hb,level,tree.outside(c.a)||tree.outside(c.b),normal);
    }
    mesh.append(out);
  }
  return mesh;
}
}
}
#endif
