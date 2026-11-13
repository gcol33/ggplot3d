#ifndef GGPLOT3D_GEOMETRY_PROCESSING_H
#define GGPLOT3D_GEOMETRY_PROCESSING_H
#include "halfedge.h"
#include "dual.h"
#include <queue>
#include <string>
namespace ggplot3d_geometry {
namespace halfedge {

constexpr double pi=3.14159265358979323846;

// Output of a mesh verb: the processed triangles, values per output corner
// and per output vertex (row-major, one column per name), and scalar
// diagnostics.
struct Result {
  Soup soup;
  std::vector<std::string> corner_names, vertex_names;
  std::vector<double> corner, vertex;
  std::vector<std::pair<std::string,double>> info;
};

static inline double angle(V u,V w) { return std::atan2(norm(cross(u,w)),dot(u,w)); }

static inline double mean_edge_length(const Mesh& m) {
  double sum=0; int n=0;
  for(int e=0;e<m.edges();++e) if(!m.edead[e]) { sum+=norm(sub(m.p[m.to[2*e]],m.p[m.to[2*e+1]])); ++n; }
  return n?sum/n:0;
}

// Unit vertex normal from the area-weighted face normals around v.
static inline V vertex_normal(const Mesh& m,int v) {
  V n{0,0,0};
  m.around(v,[&](int h) { if(m.fc[h]>=0) n=add(n,m.normal(m.fc[h])); });
  return unit(n);
}

// Angle-weighted vertex normals (Thuermer and Wuethrich 1998), one per
// corner: the faces around a vertex are split into groups at boundary edges
// and at edges whose dihedral angle exceeds crease degrees, and each corner
// takes the normal of its group.
static inline Result normals(const Soup& in,const Rcpp::List& args) {
  double limit=std::cos(Rcpp::as<double>(args["crease"])*pi/180);
  Mesh m=build(in);
  std::vector<V> fn(m.fh.size());
  for(size_t f=0;f<fn.size();++f) fn[f]=unit(m.normal(f));
  auto sharp=[&](int h) { int a=m.fc[h],b=m.fc[h^1]; return a<0||b<0||dot(fn[a],fn[b])<limit; };
  auto weight=[&](int h) { V a=m.p[m.from(h)]; return angle(sub(m.p[m.to[h]],a),sub(m.p[m.to[m.nx[h]]],a)); };
  std::vector<V> hn(m.to.size(),V{0,0,0}); std::vector<int> hs,group;
  for(size_t v=0;v<m.p.size();++v) {
    hs.clear(); m.around(v,[&](int h) { hs.push_back(h); });
    int K=hs.size(),start=0;
    while(start<K&&!sharp(hs[start])) ++start;
    V sum{0,0,0}; group.clear();
    for(int j=1;j<=K;++j) {
      int h=hs[(start+j)%K];
      if(m.fc[h]>=0) { sum=add(sum,mul(fn[m.fc[h]],weight(h))); group.push_back(h); }
      if(j==K||(start<K&&sharp(h))) { V u=unit(sum); for(int g:group) hn[g]=u; sum=V{0,0,0}; group.clear(); }
    }
  }
  Result r; std::vector<int> face_of; r.soup=m.soup(nullptr,&face_of);
  r.corner_names={"nx","ny","nz"};
  for(int f:face_of) { int h=m.fh[f]; for(int c=0;c<3;++c,h=m.nx[h]) for(int a=0;a<3;++a) r.corner.push_back(hn[h][a]); }
  return r;
}

// Discrete curvature (Meyer, Desbrun, Schroeder and Barr 2003) over the
// mixed Voronoi area of each vertex: Gaussian curvature from the angle
// defect, mean curvature from the cotangent Laplacian, signed against the
// area-weighted vertex normal so that a sphere wound outwards has mean
// curvature -1/r. Boundary vertices have no value.
static inline Result curvature(const Soup& in,const Rcpp::List&) {
  Mesh m=build(in); size_t n=m.p.size();
  std::vector<double> area(n,0),theta(n,0); std::vector<V> lap(n,V{0,0,0}),normal(n,V{0,0,0});
  for(size_t f=0;f<m.fh.size();++f) {
    Tri t=m.corners(f); V x[3]={m.p[t[0]],m.p[t[1]],m.p[t[2]]};
    V cr=cross(sub(x[1],x[0]),sub(x[2],x[0])); double twice=norm(cr);
    double a[3],cot[3]; bool obtuse=false;
    for(int i=0;i<3;++i) {
      V u=sub(x[(i+1)%3],x[i]),w=sub(x[(i+2)%3],x[i]);
      a[i]=angle(u,w); cot[i]=twice>0?dot(u,w)/twice:0; obtuse=obtuse||a[i]>pi/2;
    }
    for(int i=0;i<3;++i) {
      int j=(i+1)%3,k=(i+2)%3;
      if(!obtuse) area[t[i]]+=(dot(sub(x[j],x[i]),sub(x[j],x[i]))*cot[k]+dot(sub(x[k],x[i]),sub(x[k],x[i]))*cot[j])/8;
      else area[t[i]]+=twice/2*(a[i]>pi/2?0.5:0.25);
      theta[t[i]]+=a[i]; normal[t[i]]=add(normal[t[i]],cr);
      lap[t[j]]=add(lap[t[j]],mul(sub(x[j],x[k]),cot[i]));
      lap[t[k]]=add(lap[t[k]],mul(sub(x[k],x[j]),cot[i]));
    }
  }
  std::vector<int> vertex_of; Result r; r.soup=m.soup(&vertex_of,nullptr);
  r.vertex_names={"gaussian_curvature","mean_curvature"};
  for(int v:vertex_of) {
    double na=std::numeric_limits<double>::quiet_NaN(),g=na,h=na;
    if(!m.vb(v)&&area[v]>0) {
      V k=mul(lap[v],1/(2*area[v]));
      g=(2*pi-theta[v])/area[v];
      h=(dot(k,normal[v])>0?-1:1)*norm(k)/2;
    }
    r.vertex.push_back(g); r.vertex.push_back(h);
  }
  return r;
}

// One round of Loop (1987) subdivision, or of midpoint subdivision when
// loop is false: every triangle becomes four. Edge points take the channels
// averaged over the edge ends.
static inline Soup subdivide_once(const Mesh& m,bool loop) {
  Soup s; s.k=m.k; std::vector<int> id(m.p.size(),-1),edge_id(m.edges(),-1);
  auto push=[&](V x,int src,int a,int b) {
    s.p.push_back(x); s.source.push_back(m.source[src]);
    for(int c=0;c<m.k;++c) s.data.push_back((m.channel(a,c)+m.channel(b,c))/2);
    return static_cast<int>(s.p.size())-1;
  };
  for(size_t v=0;v<m.p.size();++v) {
    if(m.vdead[v]||m.out[v]<0) continue;
    V x=m.p[v];
    if(loop&&m.vb(v)) {
      int o=m.out[v];
      x=add(mul(x,0.75),mul(add(m.p[m.to[o]],m.p[m.from(m.pv[o])]),0.125));
    } else if(loop) {
      V sum{0,0,0}; int n=0;
      m.around(v,[&](int h) { sum=add(sum,m.p[m.to[h]]); ++n; });
      double c=0.375+0.25*std::cos(2*pi/n),beta=(0.625-c*c)/n;
      x=add(mul(x,1-n*beta),mul(sum,beta));
    }
    id[v]=push(x,v,v,v);
  }
  for(int e=0;e<m.edges();++e) {
    if(m.edead[e]) continue;
    int a=m.to[2*e+1],b=m.to[2*e];
    V x=mul(add(m.p[a],m.p[b]),0.5);
    if(loop&&!m.eb(e)) x=add(mul(add(m.p[a],m.p[b]),0.375),mul(add(m.p[m.to[m.nx[2*e]]],m.p[m.to[m.nx[2*e+1]]]),0.125));
    edge_id[e]=push(x,a,a,b);
  }
  for(size_t f=0;f<m.fh.size();++f) {
    if(m.fdead[f]) continue;
    int h=m.fh[f],g=m.nx[h],i=m.nx[g];
    int a=id[m.from(h)],b=id[m.to[h]],c=id[m.to[g]],ab=edge_id[h>>1],bc=edge_id[g>>1],ca=edge_id[i>>1];
    for(const Tri& t:{Tri{a,ab,ca},Tri{ab,b,bc},Tri{ca,bc,c},Tri{ab,bc,ca}}) { s.tri.push_back(t); s.parent.push_back(m.parent[f]); }
  }
  return s;
}

static inline Result subdivide(const Soup& in,const Rcpp::List& args) {
  int rounds=Rcpp::as<int>(args["n"]); bool loop=Rcpp::as<std::string>(args["scheme"])=="loop";
  Result r; r.soup=in;
  for(int i=0;i<rounds;++i) { Rcpp::checkUserInterrupt(); r.soup=subdivide_once(build(r.soup),loop); }
  if(rounds==0) r.soup=build(in).soup();
  return r;
}

// Taubin (1995) smoothing: each iteration moves every interior vertex by
// lambda and then by mu times its uniform umbrella Laplacian; mu < -lambda
// cancels the shrinkage of plain Laplacian smoothing. Boundary vertices are
// fixed.
static inline Result smooth(const Soup& in,const Rcpp::List& args) {
  int iterations=Rcpp::as<int>(args["iterations"]);
  double factors[2]={Rcpp::as<double>(args["lambda"]),Rcpp::as<double>(args["mu"])};
  Mesh m=build(in); std::vector<V> next(m.p.size());
  for(int it=0;it<iterations;++it) for(double factor:factors) {
    for(size_t v=0;v<m.p.size();++v) {
      next[v]=m.p[v];
      if(m.out[v]<0||m.vb(v)) continue;
      V sum{0,0,0}; int n=0;
      m.around(v,[&](int h) { sum=add(sum,m.p[m.to[h]]); ++n; });
      next[v]=add(m.p[v],mul(sub(mul(sum,1.0/n),m.p[v]),factor));
    }
    m.p.swap(next);
  }
  Result r; r.soup=m.soup(); return r;
}

// Collapsing from(h) to position x keeps every remaining face around the
// two ends non-degenerate and turns no face normal by more than 90 degrees.
static inline bool keeps_orientation(const Mesh& m,int h,V x) {
  int v0=m.from(h),v1=m.to[h],f1=m.fc[h],f2=m.fc[h^1]; bool ok=true;
  for(int v:{v0,v1}) m.around(v,[&](int g) {
    int f=m.fc[g]; if(!ok||f<0||f==f1||f==f2) return;
    Tri t=m.corners(f); V q[3];
    for(int c=0;c<3;++c) q[c]=t[c]==v0||t[c]==v1?x:m.p[t[c]];
    V after=cross(sub(q[1],q[0]),sub(q[2],q[0]));
    if(!(norm(after)>0)||dot(after,m.normal(f))<=0) ok=false;
  });
  return ok;
}

// Quadric of squared distances to weighted planes (Garland and Heckbert
// 1997). w accumulates the face area behind the quadric, so value()/w is
// an area-weighted mean squared distance.
struct Quadric {
  double a[3][3]={{0,0,0},{0,0,0},{0,0,0}}; V b{0,0,0}; double c=0,w=0;
  void plane(V n,double d,double weight) {
    for(int i=0;i<3;++i) { for(int j=0;j<3;++j) a[i][j]+=weight*n[i]*n[j]; b[i]+=weight*d*n[i]; }
    c+=weight*d*d;
  }
  void add(const Quadric& q) { for(int i=0;i<3;++i) { for(int j=0;j<3;++j) a[i][j]+=q.a[i][j]; b[i]+=q.b[i]; } c+=q.c; w+=q.w; }
  V apply(V x) const { return {a[0][0]*x[0]+a[0][1]*x[1]+a[0][2]*x[2],a[1][0]*x[0]+a[1][1]*x[1]+a[1][2]*x[2],a[2][0]*x[0]+a[2][1]*x[1]+a[2][2]*x[2]}; }
  double value(V x) const { return std::max(0.0,dot(x,apply(x))+2*dot(b,x)+c); }
  double error(V x) const { return w>0?value(x)/w:value(x); }
  // Minimiser relative to centre with eigenvalues below 1e-3 of the largest
  // truncated, so flat and creased neighbourhoods keep the vertex at the
  // centre along their flat directions (Lindstrom 2000).
  V minimiser(V centre) const {
    double s[3][3],ev[3],q[3][3];
    for(int i=0;i<3;++i) for(int j=0;j<3;++j) s[i][j]=a[i][j];
    dual::symmetric_eigen(s,ev,q);
    V r=mul(::ggplot3d_geometry::add(apply(centre),b),-1),x=centre;
    double largest=std::max(ev[0],std::max(ev[1],ev[2]));
    if(largest>0) for(int k=0;k<3;++k) if(ev[k]>=1e-3*largest) { V u{q[0][k],q[1][k],q[2][k]}; x=::ggplot3d_geometry::add(x,mul(u,dot(u,r)/ev[k])); }
    return x;
  }
};

// Quadric edge collapse decimation (Garland and Heckbert 1997). Vertex
// quadrics sum the area-weighted planes of their faces, plus planes through
// boundary edges perpendicular to their face weighted by boundary_weight
// times the squared edge length. Edges are collapsed in order of the
// area-weighted mean squared distance at the optimal position, from a heap
// with lazy invalidation, while the face count exceeds target and the RMS
// distance stays within max_error. A collapse must pass the link condition
// and the orientation check.
static inline Result decimate(const Soup& in,const Rcpp::List& args) {
  int target=Rcpp::as<int>(args["target"]);
  double max_error=Rcpp::as<double>(args["max_error"]),boundary=Rcpp::as<double>(args["boundary_weight"]);
  Mesh m=build(in); std::vector<Quadric> Q(m.p.size());
  for(size_t f=0;f<m.fh.size();++f) {
    Tri t=m.corners(f); V cr=m.normal(f); double twice=norm(cr); if(!(twice>0)) continue;
    V n=mul(cr,1/twice); double d=-dot(n,m.p[t[0]]);
    for(int v:t) { Q[v].plane(n,d,twice/2); Q[v].w+=twice/2; }
  }
  for(int h=0;h<static_cast<int>(m.to.size());++h) {
    if(!m.hb(h)||boundary<=0) continue;
    int f=m.fc[h^1]; V e=sub(m.p[m.to[h]],m.p[m.from(h)]);
    V n=unit(cross(e,unit(m.normal(f)))); if(norm(n)==0) continue;
    double d=-dot(n,m.p[m.to[h]]);
    for(int v:{m.to[h],m.from(h)}) Q[v].plane(n,d,boundary*dot(e,e));
  }
  struct Entry { double cost; int e,stamp; bool operator<(const Entry& o) const { return cost>o.cost; } };
  std::priority_queue<Entry> heap; std::vector<int> stamp(m.edges(),0); std::vector<V> place(m.edges());
  auto push=[&](int e) {
    int a=m.to[2*e+1],b=m.to[2*e]; Quadric q=Q[a]; q.add(Q[b]);
    V mid=mul(::ggplot3d_geometry::add(m.p[a],m.p[b]),0.5),x=q.minimiser(mid);
    double length=norm(sub(m.p[a],m.p[b]));
    if(!(norm(sub(x,mid))<=2*length)) x=mid;
    for(V c:{m.p[a],m.p[b]}) if(q.error(c)<q.error(x)) x=c;
    place[e]=x; heap.push({q.error(x),e,++stamp[e]});
  };
  for(int e=0;e<m.edges();++e) push(e);
  double worst=0; int steps=0;
  while(m.faces_alive>target&&!heap.empty()) {
    Entry top=heap.top(); heap.pop();
    if(m.edead[top.e]||top.stamp!=stamp[top.e]) continue;
    if(std::sqrt(top.cost)>max_error) break;
    int h=2*top.e,a=m.from(h),b=m.to[h];
    if(m.vb(a)&&!m.vb(b)) h^=1;
    if(!(m.can_collapse(h)&&keeps_orientation(m,h,place[top.e]))) {
      h^=1;
      if(!(m.can_collapse(h)&&keeps_orientation(m,h,place[top.e]))) continue;
    }
    int keep=m.to[h],gone=m.from(h);
    m.p[keep]=place[top.e]; Q[keep].add(Q[gone]);
    m.collapse(h); worst=std::max(worst,top.cost);
    m.around(keep,[&](int g) { push(g>>1); });
    if(++steps%4096==0) Rcpp::checkUserInterrupt();
  }
  Result r; r.soup=m.soup(); r.info={{"rms_error",std::sqrt(worst)},{"collapses",double(steps)}};
  return r;
}

// Closest point on triangle abc to p (Ericson 2005, 5.1.5) with its
// barycentric weights.
static inline V closest_on_triangle(V p,V a,V b,V c,double w[3]) {
  V ab=sub(b,a),ac=sub(c,a),ap=sub(p,a);
  auto set=[&](double x,double y,double z) { w[0]=x; w[1]=y; w[2]=z; return add(a,add(mul(ab,y),mul(ac,z))); };
  double d1=dot(ab,ap),d2=dot(ac,ap);
  if(d1<=0&&d2<=0) return set(1,0,0);
  V bp=sub(p,b); double d3=dot(ab,bp),d4=dot(ac,bp);
  if(d3>=0&&d4<=d3) return set(0,1,0);
  double vc=d1*d4-d3*d2;
  if(vc<=0&&d1>=0&&d3<=0) { double v=d1/(d1-d3); return set(1-v,v,0); }
  V cp=sub(p,c); double d5=dot(ab,cp),d6=dot(ac,cp);
  if(d6>=0&&d5<=d6) return set(0,0,1);
  double vb=d5*d2-d1*d6;
  if(vb<=0&&d2>=0&&d6<=0) { double v=d2/(d2-d6); return set(1-v,0,v); }
  double va=d3*d6-d5*d4;
  if(va<=0&&(d4-d3)>=0&&(d5-d6)>=0) { double v=(d4-d3)/((d4-d3)+(d5-d6)); return set(0,1-v,v); }
  double denom=va+vb+vc;
  if(!(denom>0)) return set(1,0,0);
  return set(1-(vb+vc)/denom,vb/denom,vc/denom);
}

// Uniform grid of triangle bounding boxes for closest-point queries. Cells
// are visited in shells of growing radius until no unvisited cell can hold
// a closer point.
struct TriangleGrid {
  const std::vector<V>& p; const std::vector<Tri>& tri;
  V lo{0,0,0}; double h=1; int n[3]={1,1,1}; std::vector<int> start,items; mutable std::vector<int> seen; mutable int stamp=0;
  struct Hit { int t=-1; double d2=std::numeric_limits<double>::infinity(); V x{0,0,0}; double w[3]={1,0,0}; };
  TriangleGrid(const std::vector<V>& points,const std::vector<Tri>& triangles): p(points),tri(triangles),seen(triangles.size(),0) {
    V hi=lo; bool first=true; double edge=0;
    for(const Tri& t:tri) for(int c=0;c<3;++c) {
      const V& x=p[t[c]];
      if(first) { lo=hi=x; first=false; }
      for(int a=0;a<3;++a) { lo[a]=std::min(lo[a],x[a]); hi[a]=std::max(hi[a],x[a]); }
      edge+=norm(sub(p[t[(c+1)%3]],x));
    }
    double extent=std::max(hi[0]-lo[0],std::max(hi[1]-lo[1],hi[2]-lo[2]));
    h=2*edge/std::max<size_t>(1,3*tri.size());
    if(!(h>0)) h=extent>0?extent:1;
    double budget=std::max<double>(4096,2.0*tri.size());
    for(;;) {
      for(int a=0;a<3;++a) n[a]=std::max(1,static_cast<int>(std::ceil((hi[a]-lo[a])/h)));
      if(static_cast<double>(n[0])*n[1]*n[2]<=budget) break;
      h*=1.25;
    }
    start.assign(static_cast<size_t>(n[0])*n[1]*n[2]+1,0);
    for(int pass=0;pass<2;++pass) {
      std::vector<int> fill(start.begin(),start.end()-1);
      if(pass==1) items.resize(start.back());
      for(size_t t=0;t<tri.size();++t) {
        int c0[3],c1[3]; box(t,c0,c1);
        for(int k=c0[2];k<=c1[2];++k) for(int j=c0[1];j<=c1[1];++j) for(int i=c0[0];i<=c1[0];++i) {
          size_t cell=i+static_cast<size_t>(n[0])*(j+static_cast<size_t>(n[1])*k);
          if(pass==0) ++start[cell+1]; else items[fill[cell]++]=t;
        }
      }
      if(pass==0) for(size_t c=1;c<start.size();++c) start[c]+=start[c-1];
    }
  }
  int cell_of(double x,int a) const { return std::min(n[a]-1,std::max(0,static_cast<int>(std::floor((x-lo[a])/h)))); }
  void box(size_t t,int c0[3],int c1[3]) const {
    for(int a=0;a<3;++a) {
      double l=std::min(p[tri[t][0]][a],std::min(p[tri[t][1]][a],p[tri[t][2]][a]));
      double u=std::max(p[tri[t][0]][a],std::max(p[tri[t][1]][a],p[tri[t][2]][a]));
      c0[a]=cell_of(l,a); c1[a]=cell_of(u,a);
    }
  }
  Hit closest(V x) const {
    Hit best; int c[3]; for(int a=0;a<3;++a) c[a]=cell_of(x[a],a);
    ++stamp;
    for(int r=0;;++r) {
      for(int k=c[2]-r;k<=c[2]+r;++k) for(int j=c[1]-r;j<=c[1]+r;++j) for(int i=c[0]-r;i<=c[0]+r;++i) {
        if(std::max(std::abs(i-c[0]),std::max(std::abs(j-c[1]),std::abs(k-c[2])))!=r) continue;
        if(i<0||j<0||k<0||i>=n[0]||j>=n[1]||k>=n[2]) continue;
        size_t cell=i+static_cast<size_t>(n[0])*(j+static_cast<size_t>(n[1])*k);
        for(int q=start[cell];q<start[cell+1];++q) {
          int t=items[q]; if(seen[t]==stamp) continue; seen[t]=stamp;
          double w[3]; V y=closest_on_triangle(x,p[tri[t][0]],p[tri[t][1]],p[tri[t][2]],w);
          double d2=dot(sub(x,y),sub(x,y));
          if(d2<best.d2) { best.t=t; best.d2=d2; best.x=y; for(int a=0;a<3;++a) best.w[a]=w[a]; }
        }
      }
      double gap=std::numeric_limits<double>::infinity(); bool all=true;
      for(int a=0;a<3;++a) {
        if(c[a]-r>0) { all=false; gap=std::min(gap,x[a]-(lo[a]+(c[a]-r)*h)); }
        if(c[a]+r<n[a]-1) { all=false; gap=std::min(gap,lo[a]+(c[a]+r+1)*h-x[a]); }
      }
      if(all||(best.t>=0&&gap>0&&best.d2<=gap*gap)) return best;
    }
  }
};

// Isotropic remeshing (Botsch and Kobbelt 2004) towards edge length L:
// each iteration splits edges longer than 4/3 L at their midpoints,
// collapses edges shorter than 4/5 L where no edge longer than 4/3 L
// results, flips edges that bring the valences of the four vertices closer
// to 6 (4 on the boundary), moves interior vertices to the centroid of
// their neighbours within the tangent plane, and projects all vertices
// onto the input surface. Boundary vertices are never removed or relaxed.
// Channels and sources are finally resampled from the closest input
// triangle, and each face takes the input face closest to its centroid.
static inline Result remesh(const Soup& in,const Rcpp::List& args) {
  double L=Rcpp::as<double>(args["edge"]); int iterations=Rcpp::as<int>(args["iterations"]);
  Mesh m=build(in);
  std::vector<Tri> ref; std::vector<int> ref_parent;
  for(size_t f=0;f<m.fh.size();++f) { ref.push_back(m.corners(f)); ref_parent.push_back(m.parent[f]); }
  std::vector<V> ref_p=m.p; std::vector<double> ref_data=m.data; std::vector<int> ref_source=m.source;
  TriangleGrid grid(ref_p,ref);
  double high=4.0/3*L,low=4.0/5*L;
  auto length=[&](int e) { return norm(sub(m.p[m.to[2*e]],m.p[m.to[2*e+1]])); };
  for(int it=0;it<iterations;++it) {
    Rcpp::checkUserInterrupt();
    for(bool again=true;again;) {
      again=false;
      for(int e=0;e<m.edges();++e) if(!m.edead[e]&&length(e)>high) {
        m.split(e,mul(add(m.p[m.to[2*e]],m.p[m.to[2*e+1]]),0.5)); again=true;
      }
    }
    for(int e=0;e<m.edges();++e) {
      if(m.edead[e]||length(e)>=low) continue;
      int a=m.to[2*e+1],b=m.to[2*e]; bool ba=m.vb(a),bb=m.vb(b);
      if(ba&&bb) continue;
      int h=ba?2*e+1:2*e; V x=ba?m.p[a]:bb?m.p[b]:mul(add(m.p[a],m.p[b]),0.5);
      if(!m.can_collapse(h)||!keeps_orientation(m,h,x)) continue;
      bool short_enough=true;
      for(int v:{a,b}) m.around(v,[&](int g) { if(norm(sub(m.p[m.to[g]],x))>high&&m.to[g]!=a&&m.to[g]!=b) short_enough=false; });
      if(!short_enough) continue;
      m.p[m.to[h]]=x; m.collapse(h);
    }
    for(int e=0;e<m.edges();++e) {
      if(m.edead[e]||m.eb(e)) continue;
      int a=m.to[2*e+1],b=m.to[2*e],c=m.to[m.nx[2*e]],d=m.to[m.nx[2*e+1]];
      auto deviation=[&](int v,int change) { return std::abs(m.valence(v)+change-(m.vb(v)?4:6)); };
      int before=deviation(a,0)+deviation(b,0)+deviation(c,0)+deviation(d,0);
      int after=deviation(a,-1)+deviation(b,-1)+deviation(c,1)+deviation(d,1);
      if(after>=before||!m.can_flip(e)) continue;
      V n=add(m.normal(m.fc[2*e]),m.normal(m.fc[2*e+1]));
      V na=cross(sub(m.p[c],m.p[d]),sub(m.p[a],m.p[d])),nb=cross(sub(m.p[d],m.p[c]),sub(m.p[b],m.p[c]));
      if(dot(na,n)>0&&dot(nb,n)>0) m.flip(e);
    }
    std::vector<V> next=m.p;
    for(size_t v=0;v<m.p.size();++v) {
      if(m.vdead[v]||m.out[v]<0||m.vb(v)) continue;
      V sum{0,0,0}; int k=0;
      m.around(v,[&](int g) { sum=add(sum,m.p[m.to[g]]); ++k; });
      V c=mul(sum,1.0/k),n=vertex_normal(m,v);
      next[v]=add(c,mul(n,dot(n,sub(m.p[v],c))));
    }
    for(size_t v=0;v<m.p.size();++v) if(!m.vdead[v]&&m.out[v]>=0) m.p[v]=grid.closest(next[v]).x;
  }
  for(size_t v=0;v<m.p.size();++v) {
    if(m.vdead[v]||m.out[v]<0) continue;
    auto hit=grid.closest(m.p[v]); const Tri& t=ref[hit.t];
    int best=0; for(int c=1;c<3;++c) if(hit.w[c]>hit.w[best]) best=c;
    m.source[v]=ref_source[t[best]];
    for(int q=0;q<m.k;++q) {
      double value=0; for(int c=0;c<3;++c) value+=hit.w[c]*ref_data[static_cast<size_t>(t[c])*m.k+q];
      m.data[static_cast<size_t>(v)*m.k+q]=value;
    }
  }
  for(size_t f=0;f<m.fh.size();++f) {
    if(m.fdead[f]) continue;
    Tri t=m.corners(f);
    m.parent[f]=ref_parent[grid.closest(mul(add(m.p[t[0]],add(m.p[t[1]],m.p[t[2]])),1.0/3)).t];
  }
  Result r; r.soup=m.soup(); r.info={{"mean_edge",mean_edge_length(build(r.soup))}};
  return r;
}

using Verb=Result(*)(const Soup&,const Rcpp::List&);
static inline const std::map<std::string,Verb>& verbs() {
  static const std::map<std::string,Verb> table{
    {"normals",normals},{"curvature",curvature},{"subdivide",subdivide},
    {"smooth",smooth},{"decimate",decimate},{"remesh",remesh}
  };
  return table;
}
}
}
#endif
