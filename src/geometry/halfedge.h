#ifndef GGPLOT3D_GEOMETRY_HALFEDGE_H
#define GGPLOT3D_GEOMETRY_HALFEDGE_H
#include "mesh.h"
#include <unordered_map>
namespace ggplot3d_geometry {
namespace halfedge {

using Tri=std::array<int,3>;

// Indexed triangles with provenance: source is the input vertex each vertex
// derives from, parent the input face each triangle derives from, and data
// holds k numeric channels per vertex (row-major).
struct Soup {
  std::vector<V> p; std::vector<Tri> tri;
  std::vector<int> source, parent;
  int k=0; std::vector<double> data;
};

// Half-edge mesh of a 2-manifold triangle surface with boundary. The two
// halves of edge e are 2e and 2e+1, so twin(h) = h ^ 1. Boundary half-edges
// exist explicitly with face -1 and are linked into boundary loops, so every
// half-edge has a next, a previous and a twin. The outgoing half-edge of a
// boundary vertex is its boundary half-edge. Removed elements are flagged
// and dropped when the mesh is written back to a Soup.
struct Mesh {
  std::vector<V> p; std::vector<int> out, source; std::vector<char> vdead;
  int k=0; std::vector<double> data;
  std::vector<int> to, nx, pv, fc; std::vector<char> edead;
  std::vector<int> fh, parent; std::vector<char> fdead;
  int faces_alive=0;

  int from(int h) const { return to[h^1]; }
  bool hb(int h) const { return fc[h]<0; }
  bool vb(int v) const { return out[v]<0||fc[out[v]]<0; }
  bool eb(int e) const { return fc[2*e]<0||fc[2*e+1]<0; }
  int edges() const { return static_cast<int>(to.size()/2); }
  // The outgoing half-edge after h in the rotation around from(h).
  int rotate(int h) const { return nx[h^1]; }
  void link(int a,int b) { nx[a]=b; pv[b]=a; }
  template<class F> void around(int v,F f) const {
    int s=out[v]; if(s<0) return;
    int h=s; do { f(h); h=rotate(h); } while(h!=s);
  }
  int valence(int v) const { int n=0; around(v,[&](int) { ++n; }); return n; }
  int find(int a,int b) const { int r=-1; around(a,[&](int h) { if(to[h]==b) r=h; }); return r; }
  void adjust(int v) { around(v,[&](int h) { if(fc[h]<0) out[v]=h; }); }
  V normal(int f) const { int h=fh[f]; return cross(sub(p[to[h]],p[from(h)]),sub(p[to[nx[h]]],p[from(h)])); }
  double channel(int v,int c) const { return data[static_cast<size_t>(v)*k+c]; }

  int new_vertex(V x,int src) {
    p.push_back(x); out.push_back(-1); source.push_back(src); vdead.push_back(0);
    data.resize(data.size()+k);
    return static_cast<int>(p.size())-1;
  }
  // Half-edge a -> b of a new edge; its twin runs b -> a.
  int new_edge(int a,int b) {
    int h=to.size(); to.push_back(b); to.push_back(a);
    for(int i=0;i<2;++i) { nx.push_back(-1); pv.push_back(-1); fc.push_back(-1); }
    edead.push_back(0);
    return h;
  }
  int new_face(int par) { fh.push_back(-1); parent.push_back(par); fdead.push_back(0); ++faces_alive; return static_cast<int>(fh.size())-1; }

  // Inserts a vertex at x on edge e and splits the adjacent triangles in two.
  // The new vertex takes the channels averaged over the edge ends.
  int split(int e,V x) {
    int h0=2*e,o0=h0^1,a=to[o0],b=to[h0];
    int m=new_vertex(x,source[a]);
    for(int c=0;c<k;++c) data[static_cast<size_t>(m)*k+c]=(channel(a,c)+channel(b,c))/2;
    int e1=new_edge(m,a),t1=e1^1;
    int f0=fc[h0],f3=fc[o0];
    out[m]=h0; to[o0]=m;
    if(f0>=0) {
      int h1=nx[h0],h2=nx[h1],c=to[h1];
      int e0=new_edge(m,c),t0=e0^1,f1=new_face(parent[f0]);
      fh[f0]=h0; fh[f1]=h2;
      fc[h1]=f0; fc[t0]=f0; fc[h0]=f0; fc[h2]=f1; fc[t1]=f1; fc[e0]=f1;
      link(h0,h1); link(h1,t0); link(t0,h0);
      link(e0,h2); link(h2,t1); link(t1,e0);
    } else { link(pv[h0],t1); link(t1,h0); }
    if(f3>=0) {
      int o1=nx[o0],o2=nx[o1],d=to[o1];
      int e2=new_edge(m,d),t2=e2^1,f2=new_face(parent[f3]);
      fh[f2]=o1; fh[f3]=o0;
      fc[o1]=f2; fc[t2]=f2; fc[e1]=f2; fc[o2]=f3; fc[o0]=f3; fc[e2]=f3;
      link(e1,o1); link(o1,t2); link(t2,e1);
      link(o0,e2); link(e2,o2); link(o2,o0);
    } else { int n=nx[o0]; link(e1,n); link(o0,e1); }
    if(out[a]==h0) out[a]=t1;
    adjust(m);
    return m;
  }

  // Replaces the diagonal of the two triangles on interior edge e by the
  // other diagonal.
  bool can_flip(int e) const {
    if(eb(e)) return false;
    int a=to[2*e+1],b=to[2*e],c=to[nx[2*e]],d=to[nx[2*e+1]];
    if(c==d||valence(a)<=3||valence(b)<=3) return false;
    return find(c,d)<0;
  }
  void flip(int e) {
    int a0=2*e,b0=a0^1,a1=nx[a0],a2=nx[a1],b1=nx[b0],b2=nx[b1];
    int va0=to[a0],va1=to[a1],vb0=to[b0],vb1=to[b1],fa=fc[a0],fb=fc[b0];
    to[a0]=va1; to[b0]=vb1;
    link(a0,a2); link(a2,b1); link(b1,a0);
    link(b0,b2); link(b2,a1); link(a1,b0);
    fc[a1]=fb; fc[b1]=fa; fh[fa]=a0; fh[fb]=b0;
    if(out[va0]==b0) out[va0]=a1;
    if(out[vb0]==a0) out[vb0]=b1;
  }

  // Link condition for collapsing from(h) into to(h) (Dey et al. 1999, as in
  // OpenMesh and pmp): the one-rings of the two ends share only the apices
  // of the edge's triangles, no triangle loses both its other edges to the
  // boundary, an interior edge does not join two boundary vertices, and a
  // tetrahedron is not collapsed flat.
  bool can_collapse(int h) const {
    int o=h^1,v0=to[o],v1=to[h],vl=-1,vr=-1;
    if(!hb(h)) { vl=to[nx[h]]; if(hb(nx[h]^1)&&hb(nx[nx[h]]^1)) return false; }
    if(!hb(o)) { vr=to[nx[o]]; if(hb(nx[o]^1)&&hb(nx[nx[o]]^1)) return false; }
    if(vl==vr) return false;
    if(vb(v0)&&vb(v1)&&!hb(h)&&!hb(o)) return false;
    bool shared=false;
    around(v0,[&](int g) { int w=to[g]; if(w!=v1&&w!=vl&&w!=vr&&find(w,v1)>=0) shared=true; });
    if(shared) return false;
    if(vl>=0&&vr>=0&&valence(v0)==3&&valence(v1)==3&&valence(vl)==3&&valence(vr)==3) return false;
    return true;
  }
  void remove_edge(int h) {
    int hn=nx[h],hp=pv[h],o=h^1,on=nx[o],op=pv[o],fa=fc[h],fo=fc[o],vh=to[h],vo=to[o];
    { int s=out[vo],g=s; do { to[g^1]=vh; g=nx[g^1]; } while(g!=s); }
    link(hp,hn); link(op,on);
    if(fa>=0) fh[fa]=hn;
    if(fo>=0) fh[fo]=on;
    if(out[vh]==o) out[vh]=hn;
    adjust(vh);
    out[vo]=-1; vdead[vo]=1; edead[h>>1]=1;
  }
  void remove_loop(int h) {
    int h1=nx[h],o0=h^1,o1=h1^1,v0=to[h],v1=to[h1],fa=fc[h],fo=fc[o0];
    int n=nx[o0]; link(h1,n); link(pv[o0],h1);
    fc[h1]=fo;
    out[v0]=h1; adjust(v0);
    out[v1]=o1; adjust(v1);
    if(fo>=0&&fh[fo]==o0) fh[fo]=h1;
    if(fa>=0) { fdead[fa]=1; --faces_alive; }
    edead[h>>1]=1;
  }
  // Removes from(h); to(h) keeps its position and channels.
  void collapse(int h) {
    int h1=pv[h],o1=nx[h^1];
    remove_edge(h);
    if(nx[nx[h1]]==h1) remove_loop(h1);
    if(nx[nx[o1]]==o1) remove_loop(o1);
  }

  // Corners of face f, in winding order.
  Tri corners(int f) const { int h=fh[f]; return {from(h),to[h],to[nx[h]]}; }

  // Live faces and the vertices they use, renumbered in order of first use.
  // vertex_of and face_of, when given, receive the mesh index of each output
  // vertex and face.
  Soup soup(std::vector<int>* vertex_of=nullptr,std::vector<int>* face_of=nullptr) const {
    Soup s; s.k=k; std::vector<int> id(p.size(),-1);
    for(size_t f=0;f<fh.size();++f) {
      if(fdead[f]) continue;
      Tri t=corners(f);
      for(int& v:t) {
        if(id[v]<0) {
          id[v]=s.p.size(); s.p.push_back(p[v]); s.source.push_back(source[v]);
          for(int c=0;c<k;++c) s.data.push_back(channel(v,c));
          if(vertex_of) vertex_of->push_back(v);
        }
        v=id[v];
      }
      s.tri.push_back(t); s.parent.push_back(parent[f]);
      if(face_of) face_of->push_back(f);
    }
    return s;
  }
};

static inline long long edge_key(int a,int b,long long n) { return a<b?a*n+b:b*n+a; }

// Builds the half-edge mesh of a triangle soup. Triangles with a repeated
// corner are dropped. An edge shared by more than two triangles is an error.
// Each connected component is wound consistently, flipping the triangles
// that disagree with the majority (with a warning); a non-orientable
// component is an error. A vertex whose triangles form several fans joined
// only at the vertex is split into one vertex per fan.
static inline Mesh build(const Soup& in) {
  const int n=in.p.size();
  std::vector<Tri> tri; std::vector<int> parent;
  for(size_t f=0;f<in.tri.size();++f) {
    const Tri& t=in.tri[f];
    for(int v:t) if(v<0||v>=n) Rcpp::stop("Mesh faces reference missing vertices.");
    if(t[0]==t[1]||t[1]==t[2]||t[0]==t[2]) continue;
    tri.push_back(t); parent.push_back(in.parent[f]);
  }
  const int F=tri.size();
  std::unordered_map<long long,int> ids; std::vector<std::array<int,2>> faces; std::vector<int> count;
  for(int f=0;f<F;++f) for(int c=0;c<3;++c) {
    long long key=edge_key(tri[f][c],tri[f][(c+1)%3],n);
    auto found=ids.find(key); int e;
    if(found==ids.end()) { e=faces.size(); ids[key]=e; faces.push_back({-1,-1}); count.push_back(0); } else e=found->second;
    if(count[e]<2) faces[e][count[e]]=f;
    ++count[e];
  }
  int crowded=0;
  for(int c:count) crowded+=c>2;
  if(crowded) Rcpp::stop("The mesh is not a 2-manifold: %d edges are shared by more than two faces.",crowded);

  // Orientation: across each shared edge the two triangles must traverse it
  // in opposite directions.
  auto forward=[&](int f,int a,int b) { for(int c=0;c<3;++c) if(tri[f][c]==a&&tri[f][(c+1)%3]==b) return true; return false; };
  std::vector<std::vector<std::array<int,2>>> adjacent(F);
  for(const auto& item:ids) {
    int e=item.second; if(count[e]!=2) continue;
    int a=item.first/n,b=item.first%n,f=faces[e][0],g=faces[e][1];
    int same=forward(f,a,b)==forward(g,a,b);
    adjacent[f].push_back({g,same}); adjacent[g].push_back({f,same});
  }
  std::vector<int> flip(F,-1),stack; int flipped=0;
  for(int seed=0;seed<F;++seed) {
    if(flip[seed]>=0) continue;
    std::vector<int> component; flip[seed]=0; stack.push_back(seed);
    while(!stack.empty()) {
      int f=stack.back(); stack.pop_back(); component.push_back(f);
      for(const auto& a:adjacent[f]) {
        int want=flip[f]^a[1];
        if(flip[a[0]]<0) { flip[a[0]]=want; stack.push_back(a[0]); }
        else if(flip[a[0]]!=want) Rcpp::stop("The mesh is not orientable, so faces cannot be wound consistently.");
      }
    }
    int k=0; for(int f:component) k+=flip[f];
    bool invert=2*k>static_cast<int>(component.size());
    for(int f:component) if(flip[f]^invert) { std::swap(tri[f][1],tri[f][2]); ++flipped; }
  }
  if(flipped) Rcpp::warning("Reversed the winding of %d triangles to orient the mesh consistently.",flipped);

  // Fans: corners of one vertex are joined through the edges at that vertex.
  UnionFind fans(3*F);
  auto corner=[&](int f,int v) { for(int c=0;c<3;++c) if(tri[f][c]==v) return 3*f+c; return -1; };
  for(const auto& item:ids) {
    int e=item.second; if(count[e]!=2) continue;
    int a=item.first/n,b=item.first%n,f=faces[e][0],g=faces[e][1];
    fans.unite(corner(f,a),corner(g,a)); fans.unite(corner(f,b),corner(g,b));
  }
  Mesh m; m.k=in.k;
  m.p=in.p; m.source.resize(n); m.out.assign(n,-1); m.vdead.assign(n,0); m.data=in.data;
  for(int v=0;v<n;++v) m.source[v]=in.source[v];
  std::vector<int> fan_vertex(3*F,-1),first_fan(n,-1);
  for(int c=0;c<3*F;++c) {
    int v=tri[c/3][c%3],r=fans.find(c);
    if(fan_vertex[r]<0) {
      if(first_fan[v]<0) { first_fan[v]=r; fan_vertex[r]=v; }
      else {
        fan_vertex[r]=m.new_vertex(m.p[v],m.source[v]);
        for(int q=0;q<m.k;++q) m.data[static_cast<size_t>(fan_vertex[r])*m.k+q]=m.channel(v,q);
      }
    }
    tri[c/3][c%3]=fan_vertex[r];
  }

  const int N=m.p.size();
  std::unordered_map<long long,int> edge;
  for(int f=0;f<F;++f) {
    int face=m.new_face(parent[f]); int hs[3];
    for(int c=0;c<3;++c) {
      int a=tri[f][c],b=tri[f][(c+1)%3]; long long key=edge_key(a,b,N);
      auto found=edge.find(key); int h;
      if(found==edge.end()) { h=m.new_edge(a,b); edge[key]=h; }
      else { h=found->second; if(m.to[h]!=b) h^=1; }
      m.fc[h]=face; hs[c]=h; m.out[a]=h;
    }
    for(int c=0;c<3;++c) m.link(hs[c],hs[(c+1)%3]);
    m.fh[face]=hs[0];
  }
  std::vector<int> boundary_out(N,-1);
  for(int h=0;h<static_cast<int>(m.to.size());++h) if(m.fc[h]<0) {
    int v=m.from(h);
    if(boundary_out[v]>=0) Rcpp::stop("The mesh is not a 2-manifold at a boundary vertex.");
    boundary_out[v]=h; m.out[v]=h;
  }
  for(int h=0;h<static_cast<int>(m.to.size());++h) if(m.fc[h]<0) m.link(h,boundary_out[m.to[h]]);
  return m;
}

// Soup from a single list of triangles with identity provenance.
static inline Soup soup(std::vector<V> p,std::vector<Tri> tri) {
  Soup s; s.p=std::move(p); s.tri=std::move(tri);
  s.source.resize(s.p.size()); std::iota(s.source.begin(),s.source.end(),0);
  s.parent.resize(s.tri.size()); std::iota(s.parent.begin(),s.parent.end(),0);
  return s;
}
}
}
#endif
