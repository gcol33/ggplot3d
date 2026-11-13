#ifndef GGPLOT3D_GEOMETRY_FLIP_H
#define GGPLOT3D_GEOMETRY_FLIP_H
#include "operators.h"
#include <queue>
namespace ggplot3d_geometry {
namespace halfedge {

// Intrinsic triangulation on the vertices of a mesh (Sharp, Soliman and
// Crane 2019b): the connectivity of a half-edge mesh with a length per
// edge, and a signpost per half-edge, the angle of its direction at its
// tail measured from the vertex's outgoing half-edge in Mesh::rotate order,
// in [0, total angle). Flips change connectivity, lengths and the signposts
// of the new edge only, so the original surface is unchanged.
struct Intrinsic {
  Mesh t; std::vector<double> len,sign,total;

  explicit Intrinsic(const Mesh& m): t(m),len(m.edges()),sign(m.to.size(),0),total(m.p.size(),0) {
    for(int e=0;e<m.edges();++e) len[e]=norm(sub(m.p[m.to[2*e]],m.p[m.to[2*e+1]]));
    for(size_t v=0;v<m.p.size();++v) {
      if(m.vdead[v]||m.out[v]<0) continue;
      double at=0;
      t.around(v,[&](int h) { sign[h]=at; if(t.fc[h^1]>=0) at+=corner(t.rotate(h)); });
      total[v]=at;
    }
  }
  // Interior angle at the tail of h in its face, from the edge lengths.
  double corner(int h) const {
    double a=len[h>>1],b=len[t.pv[h]>>1],c=len[t.nx[h]>>1];
    return std::acos(std::max(-1.0,std::min(1.0,(a*a+b*b-c*c)/(2*a*b))));
  }
  // Angle from signpost a to signpost b at vertex v in rotate order, or
  // infinity when that turn crosses the boundary.
  double turn(int v,double a,double b) const {
    if(t.vb(v)) return b>=a?b-a:std::numeric_limits<double>::infinity();
    double d=std::fmod(b-a,total[v]); return d<0?d+total[v]:d;
  }
  bool flippable(int e) const {
    int h=2*e,o=h^1;
    if(t.fc[h]<0||t.fc[o]<0) return false;
    int i=t.from(h),j=t.to[h],k=t.to[t.nx[h]],l=t.to[t.nx[o]];
    if(i==j||k==l||t.valence(i)<2||t.valence(j)<2) return false;
    return corner(h)+corner(t.nx[o])<pi&&corner(t.nx[h])+corner(o)<pi;
  }
  // Flips interior edge e: lays out its two triangles in the plane, reads
  // off the distance between the two apices, and sets the signposts of the
  // new half-edges from their predecessors in rotate order.
  void flip(int e) {
    int h=2*e,o=h^1;
    double lij=len[e],lik=len[t.pv[h]>>1],ljk=len[t.nx[h]>>1],lil=len[t.nx[o]>>1],ljl=len[t.pv[o]>>1];
    double xk=(lij*lij+lik*lik-ljk*ljk)/(2*lij),yk=std::sqrt(std::max(0.0,lik*lik-xk*xk));
    double xl=(lij*lij+lil*lil-ljl*ljl)/(2*lij),yl=-std::sqrt(std::max(0.0,lil*lil-xl*xl));
    t.flip(e);
    len[e]=std::hypot(xk-xl,yk-yl);
    for(int q:{h,o}) {
      int v=t.from(q),prev=t.pv[q]^1;
      sign[q]=std::fmod(sign[prev]+corner(q),total[v]);
    }
  }
};

// Edge path from vertex a to vertex b along the shortest route through the
// mesh edges (Dijkstra), as half-edges; empty when b is unreachable.
static inline std::vector<int> dijkstra_path(const Intrinsic& s,int a,int b) {
  const Mesh& m=s.t; std::vector<double> dist(m.p.size(),std::numeric_limits<double>::infinity());
  std::vector<int> via(m.p.size(),-1);
  typedef std::pair<double,int> Item; std::priority_queue<Item,std::vector<Item>,std::greater<Item>> queue;
  dist[a]=0; queue.push({0,a});
  while(!queue.empty()) {
    Item top=queue.top(); queue.pop(); int v=top.second;
    if(top.first>dist[v]) continue;
    if(v==b) break;
    m.around(v,[&](int h) { double d=dist[v]+s.len[h>>1]; int w=m.to[h]; if(d<dist[w]) { dist[w]=d; via[w]=h; queue.push({d,w}); } });
  }
  std::vector<int> path;
  if(a==b||via[b]<0) return path;
  for(int v=b;v!=a;v=m.from(via[v])) path.push_back(via[v]);
  std::reverse(path.begin(),path.end());
  return path;
}

// FlipOut (Sharp and Crane 2020, algorithms 1 and 2): while the edge path
// has a joint a-b-c whose smaller wedge angle at b is below pi and whose
// wedge holds no other path edge, flip the wedge's interior edges at b
// whose outer-arc angle is below pi, in order, until none is left, and
// replace a-b-c by the outer arc. The joint with the smallest wedge angle
// goes first. Every step shortens the path; the result is a locally
// shortest polyhedral geodesic.
static inline std::vector<int> flip_out(Intrinsic& s,std::vector<int> path,int max_iterations,int* iterations) {
  Mesh& m=s.t; const double tolerance=1e-9;
  auto inverse=[&](int h) { return m.pv[h]^1; };
  auto wedge=[&](int qa,int qc,bool forward,std::vector<int>& spokes) {
    spokes.clear(); int q=qa,guard=0;
    while(q!=qc) { q=forward?m.rotate(q):inverse(q); if(q!=qc) spokes.push_back(q); if(++guard>100000) return false; }
    return true;
  };
  std::vector<int> on_path(m.edges(),0),spokes;
  *iterations=0;
  while(*iterations<max_iterations) {
    std::fill(on_path.begin(),on_path.end(),0);
    for(int h:path) ++on_path[h>>1];
    int best=-1; bool best_forward=true; double best_angle=pi-tolerance;
    for(size_t i=1;i<path.size();++i) {
      int qa=path[i-1]^1,qc=path[i],b=m.from(qc);
      if(qa==qc) { best=i; best_angle=-1; break; }
      double fwd=s.turn(b,s.sign[qa],s.sign[qc]),bwd=s.turn(b,s.sign[qc],s.sign[qa]);
      bool forward=fwd<=bwd; double a=std::min(fwd,bwd);
      if(!(a<best_angle)) continue;
      if(!wedge(qa,qc,forward,spokes)) continue;
      bool blocked=false;
      for(int q:spokes) if(on_path[q>>1]||m.to[q]==b) blocked=true;
      if(blocked) continue;
      best=i; best_forward=forward; best_angle=a;
    }
    if(best<0) break;
    ++*iterations;
    int qa=path[best-1]^1,qc=path[best];
    if(qa==qc) { path.erase(path.begin()+best-1,path.begin()+best+1); continue; }
    bool stuck=false;
    for(size_t k=0;;) {
      wedge(qa,qc,best_forward,spokes);
      if(k>=spokes.size()) break;
      int q=spokes[k];
      if(s.corner(m.nx[q])+s.corner(q^1)<pi) {
        if(!s.flippable(q>>1)) { stuck=true; break; }
        s.flip(q>>1); k=k>0?k-1:0;
      } else ++k;
    }
    if(stuck) break;
    std::vector<int> arc; std::vector<int> all{qa}; all.insert(all.end(),spokes.begin(),spokes.end()); all.push_back(qc);
    for(size_t k=0;k+1<all.size();++k) arc.push_back(best_forward?m.nx[all[k+1]]^1:m.nx[all[k]]);
    std::vector<int> next(path.begin(),path.begin()+best-1);
    next.insert(next.end(),arc.begin(),arc.end());
    next.insert(next.end(),path.begin()+best+1,path.end());
    path.swap(next);
    if(*iterations%1024==0) Rcpp::checkUserInterrupt();
  }
  return path;
}

// Points where intrinsic half-edge q crosses the edges of the input mesh,
// excluding its tail and including its head: from the tail, the input
// corner holding q's signpost gives the direction in that face, and the
// straight line is followed across faces, unfolding the direction about
// each crossed edge, for the intrinsic length.
static inline void trace(const Mesh& in,const std::vector<double>& in_sign,const Intrinsic& s,int q,std::vector<V>& out) {
  int v=s.t.from(q),target=s.t.to[q]; double angle=s.sign[q],remaining=s.len[q>>1];
  int g=-1; double offset=0;
  in.around(v,[&](int h) {
    if(g>=0) return;
    double a=angle-in_sign[h];
    double span=in.fc[h^1]>=0?(in_sign[in.rotate(h)]>in_sign[h]?in_sign[in.rotate(h)]-in_sign[h]:s.total[v]-in_sign[h]):0;
    if(a>=-1e-12&&a<span+1e-12) { g=h; offset=std::max(0.0,a); }
  });
  if(g<0||offset<1e-10||std::abs(offset-(in_sign[in.rotate(g)]>in_sign[g]?in_sign[in.rotate(g)]-in_sign[g]:s.total[v]-in_sign[g]))<1e-10) {
    out.push_back(in.p[target]); return;
  }
  int face=in.fc[g^1];
  V x=in.p[v],u=unit(sub(in.p[in.to[g]],x)),w=sub(in.p[in.to[in.rotate(g)]],x);
  w=unit(sub(w,mul(u,dot(w,u))));
  V d=add(mul(u,std::cos(offset)),mul(w,std::sin(offset)));
  int entry=-1; double scale=remaining;
  for(int step=0;step<static_cast<int>(4*in.fh.size()+8);++step) {
    int h0=in.fh[face],hit=-1; double best=std::numeric_limits<double>::infinity(),lambda=0;
    for(int k=0,h=h0;k<3;++k,h=in.nx[h]) {
      if(h==entry) continue;
      if(entry<0&&(in.from(h)==v||in.to[h]==v)) continue;
      V P=in.p[in.from(h)],E=sub(in.p[in.to[h]],P),r=sub(P,x);
      V n=cross(d,E); double den=dot(n,n); if(!(den>0)) continue;
      double sx=dot(cross(r,E),n)/den,lx=dot(cross(r,d),n)/den;
      if(sx>1e-14*scale&&sx<best&&lx>-1e-9&&lx<1+1e-9) { best=sx; hit=h; lambda=lx; }
    }
    if(hit<0||best>=remaining-1e-12*scale||in.fc[hit^1]<0) break;
    lambda=std::min(1-1e-9,std::max(1e-9,lambda));
    V P=in.p[in.from(hit)],E=sub(in.p[in.to[hit]],P);
    x=add(P,mul(E,lambda)); out.push_back(x); remaining-=best;
    V te=unit(E);
    auto inward=[&](int f) {
      Tri c=in.corners(f); int apex=c[0]; for(int a:c) if(a!=in.from(hit)&&a!=in.to[hit]) apex=a;
      V r=sub(in.p[apex],x); return unit(sub(r,mul(te,dot(r,te))));
    };
    double along=dot(d,te),across=dot(d,mul(inward(face),-1));
    face=in.fc[hit^1]; entry=hit^1;
    d=unit(add(mul(te,along),mul(inward(face),across)));
  }
  out.push_back(in.p[target]);
}
}
}
#endif
