#ifndef GGPLOT3D_GEOMETRY_POISSON_H
#define GGPLOT3D_GEOMETRY_POISSON_H
#include "octree.h"
#include "normals.h"
#include <chrono>
namespace ggplot3d_geometry {
namespace poisson {

// Screened Poisson surface reconstruction (Kazhdan, Bolitho and Hoppe 2006;
// Kazhdan and Hoppe 2013). The points are mapped into the unit cube, an
// octree is adapted to their density, and the indicator function chi is
// sought in the span of the quadratic B-splines of all octree nodes, as the
// minimiser of
//   int |grad chi - V|^2 + alpha Area/|P| sum_p chi(p)^2,
// where V splats the oriented normals. The system is solved by a cascadic
// pass from coarse to fine depths, each depth solving for its own
// coefficients with the coarser solution fixed, and with the screening of
// depth d weighted by 2^d (scale-independent screening). The B-splines are
// reflected at the faces of the cube (Neumann boundary condition): the
// function of a node is the sum of the B-splines of the node and of its
// mirror images, so every depth holds the constants and the spaces of
// successive depths are nested.

using I3=std::array<int,3>;
using Key=long long;

static inline Key pack(const I3& i) { return (Key(i[0])<<42)|(Key(i[1])<<21)|Key(i[2]); }
static inline I3 unpack(Key k) { const Key m=(Key(1)<<21)-1; return {int((k>>42)&m),int((k>>21)&m),int(k&m)}; }
static inline int floor_half(int a) { return a>=0?a/2:-((1-a)/2); }
// Node index of the mirror image of node j in [0, N) under reflections at 0
// and N.
static inline int fold(int j,int N) { while(j<0||j>=N) j=j<0?-1-j:2*N-1-j; return j; }
static inline bool inside(const I3& k,int N) { return k[0]>=0&&k[1]>=0&&k[2]>=0&&k[0]<N&&k[1]<N&&k[2]<N; }

// Open-addressing hash table (linear probing) from keys to dense ids in
// order of insertion.
struct KeyIndex {
  std::vector<Key> slot_key,order; std::vector<int> slot_id; size_t mask=0;
  explicit KeyIndex(size_t expected=8) { reset(expected); }
  void reset(size_t expected) {
    size_t c=16; while(c<2*expected) c<<=1;
    slot_key.assign(c,-1); slot_id.assign(c,-1); mask=c-1; order.clear();
  }
  static size_t hash(Key k) {
    unsigned long long x=k; x^=x>>33; x*=0xff51afd7ed558ccdULL; x^=x>>33; x*=0xc4ceb9fe1a85ec53ULL; x^=x>>33;
    return static_cast<size_t>(x);
  }
  int size() const { return order.size(); }
  int find(Key k) const {
    size_t h=hash(k)&mask;
    while(slot_key[h]!=-1) { if(slot_key[h]==k) return slot_id[h]; h=(h+1)&mask; }
    return -1;
  }
  int insert(Key k) {
    if(2*(order.size()+1)>slot_key.size()) {
      std::vector<Key> keep=order; reset(2*keep.size()+2);
      for(Key x:keep) place(x);
    }
    return place(k);
  }
  int place(Key k) {
    size_t h=hash(k)&mask;
    while(slot_key[h]!=-1) { if(slot_key[h]==k) return slot_id[h]; h=(h+1)&mask; }
    slot_key[h]=k; slot_id[h]=order.size(); order.push_back(k);
    return slot_id[h];
  }
};

// Quadratic B-spline on its support [0, 3] (the box filter convolved with
// itself three times) and its derivative. The B-spline of node j at a depth
// with lattice coordinate u is bspline(u - j + 1): it is centred on the cell
// [j, j + 1] and spans the cells j - 1 to j + 1.
static inline double bspline(double t) {
  if(t<=0||t>=3) return 0;
  if(t<1) return 0.5*t*t;
  if(t<2) return 0.5*(-2*t*t+6*t-3);
  return 0.5*(3-t)*(3-t);
}
static inline double bspline_slope(double t) {
  if(t<=0||t>=3) return 0;
  if(t<1) return t;
  if(t<2) return 3-2*t;
  return t-3;
}
// Two-scale relation: the B-spline of node j at depth d - 1 is the sum over m
// of refine_weight[m + 1] times the B-spline of node 2j + m at depth d.
static constexpr double refine_weight[4]={0.25,0.75,0.75,0.25};
// Parents of node j: the two nodes p (before folding) of the coarser depth
// whose two-scale relation involves j, with weights.
static inline void parents(int j,int p[2],double w[2]) {
  p[0]=floor_half(j+1)-1; p[1]=p[0]+1;
  for(int a=0;a<2;++a) w[a]=refine_weight[j-2*p[a]+1];
}

// Integrals over [0, N] (lattice units) of products of the reflected node
// functions k and l = k + o at a depth with N cells per axis, for k in
// [0, N) and o in [-2, 2]: value-value (m), slope-slope (d) and value of k
// times slope of l (g). Each unit interval holds polynomials of degree at
// most four, which three-point Gauss-Legendre quadrature integrates exactly.
struct Integrals {
  int N; std::vector<double> m,d,g;
  explicit Integrals(int N): N(N),m(5*N,0),d(5*N,0),g(5*N,0) {
    std::vector<std::vector<int>> image(N);
    for(int j=-2;j<=N+1;++j) image[fold(j,N)].push_back(j);
    auto value=[&](int k,double u,bool slope) { double s=0; for(int j:image[k]) s+=slope?bspline_slope(u-j+1):bspline(u-j+1); return s; };
    const double node[3]={0.5-0.5*std::sqrt(0.6),0.5,0.5+0.5*std::sqrt(0.6)},weight[3]={5.0/18,8.0/18,5.0/18};
    for(int k=0;k<N;++k) for(int o=-2;o<=2;++o) {
      int l=k+o; if(l<0||l>=N) continue;
      double sm=0,sd=0,sg=0;
      for(int c=std::max(0,std::min(k,l)-2);c<std::min(N,std::max(k,l)+3);++c) {
        for(int q=0;q<3;++q) {
          double u=c+node[q],vk=value(k,u,false),vl=value(l,u,false);
          sm+=weight[q]*vk*vl; sd+=weight[q]*value(k,u,true)*value(l,u,true); sg+=weight[q]*vk*value(l,u,true);
        }
      }
      m[5*k+o+2]=sm; d[5*k+o+2]=sd; g[5*k+o+2]=sg;
    }
  }
  int at(int k,int l) const { return (k<0||k>=N||l<0||l>=N||l-k<-2||l-k>2)?-1:5*k+l-k+2; }
  double M(int k,int l) const { int a=at(k,l); return a<0?0:m[a]; }
  double D(int k,int l) const { int a=at(k,l); return a<0?0:d[a]; }
  double G(int k,int l) const { int a=at(k,l); return a<0?0:g[a]; }
  // int grad B_a . grad B_b over the unit cube, for cell width h.
  double laplacian(const I3& a,const I3& b,double h) const {
    double m0=M(a[0],b[0]),m1=M(a[1],b[1]),m2=M(a[2],b[2]);
    return h*(D(a[0],b[0])*m1*m2+m0*D(a[1],b[1])*m2+m0*m1*D(a[2],b[2]));
  }
};

static inline I3 offset(int s) { return {s/25-2,(s/5)%5-2,s%5-2}; }

// Reflected node functions of one depth that are nonzero at the point u
// (unit cube coordinates), with their values and, when gradient is given,
// gradients: at most 27 nodes of the octree index. Returns their number.
static inline int support(const V& u,int N,const KeyIndex& index,int* id,double* value,V* gradient) {
  int base[3]; double b[3][3],s[3][3]; int node[3][3];
  for(int a=0;a<3;++a) {
    double x=std::min(std::max(u[a],0.0),1.0)*N; base[a]=static_cast<int>(std::floor(x))-1;
    for(int t=0;t<3;++t) {
      double r=x-(base[a]+t)+1; b[a][t]=bspline(r); s[a][t]=gradient?bspline_slope(r)*N:0;
      node[a][t]=fold(base[a]+t,N);
    }
  }
  int count=0;
  for(int t=0;t<27;++t) {
    int i=t/9,j=(t/3)%3,k=t%3;
    double v=b[0][i]*b[1][j]*b[2][k];
    if(v==0&&!gradient) continue;
    int found=index.find(pack(I3{node[0][i],node[1][j],node[2][k]}));
    if(found<0) continue;
    int c=0; while(c<count&&id[c]!=found) ++c;
    if(c==count) { id[c]=found; value[c]=0; if(gradient) gradient[c]=V{0,0,0}; ++count; }
    value[c]+=v;
    if(gradient) gradient[c]=add(gradient[c],V{s[0][i]*b[1][j]*b[2][k],b[0][i]*s[1][j]*b[2][k],b[0][i]*b[1][j]*s[2][k]});
  }
  return count;
}

// Octree as the set of refined nodes per depth; the nodes of depth d are the
// children of the refined nodes of depth d - 1 (the root at depth 0).
struct Octree {
  int D; std::vector<KeyIndex> refined;
  explicit Octree(int D): D(D),refined(D) {}
  void refine(int d,I3 c) {
    for(;d>=0;--d) {
      if(refined[d].find(pack(c))>=0) return;
      refined[d].insert(pack(c));
      for(int a=0;a<3;++a) c[a]=floor_half(c[a]);
    }
  }
  bool is_refined(int d,const I3& c) const { return d<D&&refined[d].find(pack(c))>=0; }
};

// The linear system of one depth over its nodes: Laplacian entries from the
// one-dimensional integrals, plus screening alpha 2^d Area/|P| times the
// products of node functions at the samples (27 slots per sample). Along
// each axis the Laplacian rows of nodes at least three cells from both
// faces coincide up to translation, so rows are grouped into classes by
// their per-axis distance to the faces and each class keeps one stencil.
struct Level {
  int n=0,N=1; double h=1,screen=0;
  std::vector<I3> node; std::vector<int> nbr,row_class; std::vector<double> stencil;
  std::vector<int> sid; std::vector<double> sval;
  Level(int N,const Integrals& table,const std::vector<I3>& nodes): n(nodes.size()),N(N),h(1.0/N),node(nodes),row_class(nodes.size()) {
    std::vector<int> axis_class(N),representative;
    for(int k=0;k<N;++k) {
      bool far=N>=8&&k>=3&&k<=N-4;
      if(far&&k>3) { axis_class[k]=axis_class[3]; continue; }
      axis_class[k]=representative.size(); representative.push_back(far?N/2:k);
    }
    int c=representative.size();
    stencil.assign(125*static_cast<size_t>(c*c*c),0);
    for(int a=0;a<c;++a) for(int b=0;b<c;++b) for(int e=0;e<c;++e) {
      I3 k{representative[a],representative[b],representative[e]};
      for(int s=0;s<125;++s) {
        I3 o=offset(s),j{k[0]+o[0],k[1]+o[1],k[2]+o[2]};
        if(inside(j,N)) stencil[125*static_cast<size_t>((a*c+b)*c+e)+s]=table.laplacian(k,j,h);
      }
    }
    for(int i=0;i<n;++i) row_class[i]=(axis_class[node[i][0]]*c+axis_class[node[i][1]])*c+axis_class[node[i][2]];
  }
  const double* row(int i) const { return &stencil[125*static_cast<size_t>(row_class[i])]; }
  void apply(const std::vector<double>& x,std::vector<double>& y) const {
    for(int i=0;i<n;++i) {
      double sum=0; const int* adj=&nbr[125*static_cast<size_t>(i)]; const double* w=row(i);
      for(int s=0;s<125;++s) if(adj[s]>=0) sum+=w[s]*x[adj[s]];
      y[i]=sum;
    }
    size_t np=sid.size()/27;
    for(size_t p=0;p<np;++p) {
      double t=0;
      for(int q=0;q<27;++q) { int j=sid[27*p+q]; if(j>=0) t+=sval[27*p+q]*x[j]; }
      t*=screen;
      for(int q=0;q<27;++q) { int j=sid[27*p+q]; if(j>=0) y[j]+=sval[27*p+q]*t; }
    }
  }
  std::vector<double> diagonal() const {
    std::vector<double> out(n);
    for(int i=0;i<n;++i) out[i]=row(i)[62];
    for(size_t q=0;q<sid.size();++q) if(sid[q]>=0) out[sid[q]]+=screen*sval[q]*sval[q];
    return out;
  }
};

// Preconditioned conjugate gradients with the Jacobi preconditioner, from
// x = 0 until the residual falls below tol times the right-hand side.
static inline std::vector<double> conjugate_gradients(const Level& A,const std::vector<double>& b,double tol,int max_iter,int& iterations) {
  int n=A.n; std::vector<double> x(n,0),r=b,z(n),p(n),q(n),inv=A.diagonal();
  for(double& v:inv) v=v>0?1/v:1;
  double bb=0; for(double v:b) bb+=v*v;
  iterations=0;
  if(bb==0) return x;
  for(int i=0;i<n;++i) z[i]=inv[i]*r[i];
  p=z; double rz=0; for(int i=0;i<n;++i) rz+=r[i]*z[i];
  for(int it=0;it<max_iter;++it) {
    double rr=0; for(double v:r) rr+=v*v;
    if(rr<=tol*tol*bb) break;
    if(it%16==0) Rcpp::checkUserInterrupt();
    A.apply(p,q);
    double pq=0; for(int i=0;i<n;++i) pq+=p[i]*q[i];
    if(!(pq>0)) break;
    double alpha=rz/pq;
    for(int i=0;i<n;++i) { x[i]+=alpha*p[i]; r[i]-=alpha*q[i]; }
    for(int i=0;i<n;++i) z[i]=inv[i]*r[i];
    double next=0; for(int i=0;i<n;++i) next+=r[i]*z[i];
    double beta=next/rz; rz=next;
    for(int i=0;i<n;++i) p[i]=z[i]+beta*p[i];
    ++iterations;
  }
  return x;
}

struct Options { int depth=8,neighbours=10; double screening=4,samples_per_node=1.5,scale=1.1; int full_depth=5; double tol=1e-6; int max_iter=2000; };

// The fitted indicator function: coefficients of the nodes of every depth.
// ring and acc hold, per depth, the function of that depth and all coarser
// ones as coefficients of the depth's node functions on the two-ring of its
// nodes; finer holds for every node and every finer depth the largest
// coefficient magnitude among its descendants there.
struct Fit {
  int D=0; V lo{0,0,0}; double width=1,iso=0;
  std::vector<KeyIndex> index,ring; std::vector<std::vector<double>> x,acc,finer;
  std::vector<std::vector<I3>> node; Octree tree{0};
  std::vector<double> seconds; std::vector<int> nodes,iterations;
  V unit_point(const V& p) const { V u; for(int a=0;a<3;++a) u[a]=(p[a]-lo[a])/width; return u; }
  // chi and, when gradient is given, its gradient in data units at p. Node
  // supports nest across depths, so the sum stops at the first depth with
  // no node around p.
  double value(const V& p,V* gradient) const {
    V u=unit_point(p),g{0,0,0}; double f=0;
    int id[27]; double v[27]; V grad[27];
    for(int d=0;d<=D;++d) {
      int count=support(u,1<<d,index[d],id,v,gradient?grad:nullptr);
      if(!count) break;
      for(int c=0;c<count;++c) { f+=x[d][id[c]]*v[c]; if(gradient) g=add(g,mul(grad[c],x[d][id[c]])); }
    }
    if(gradient) *gradient=mul(g,1/width);
    return f;
  }
  // Whether chi may reach level on the closed cell c of depth d: the
  // functions of depths up to d lie within the range of the coefficients
  // around the cell (the node functions are nonnegative and sum to one),
  // and each finer depth adds at most its largest coefficient magnitude
  // among the descendants of the nodes around the cell.
  bool may_meet(int d,const I3& c,double level) const {
    int N=1<<d; double low=INFINITY,high=-INFINITY; std::vector<double> slack(D+1,0);
    for(int t=0;t<27;++t) {
      I3 k{fold(c[0]+t/9-1,N),fold(c[1]+(t/3)%3-1,N),fold(c[2]+t%3-1,N)};
      int r=ring[d].find(pack(k));
      double v=r>=0?acc[d][r]:0; low=std::min(low,v); high=std::max(high,v);
      int i=index[d].find(pack(k));
      if(i>=0) for(int e=d+1;e<=D;++e) slack[e]=std::max(slack[e],finer[d][i*static_cast<size_t>(D+1)+e]);
    }
    double extra=0; for(double v:slack) extra+=v;
    return low-extra<=level&&high+extra>=level;
  }
};

static inline double seconds_since(std::chrono::steady_clock::time_point& t) {
  auto now=std::chrono::steady_clock::now();
  double s=std::chrono::duration<double>(now-t).count(); t=now; return s;
}

static inline Fit fit(const std::vector<V>& points,const std::vector<V>& normals,const Options& opt) {
  auto clock=std::chrono::steady_clock::now();
  int n=points.size(),D=opt.depth;
  Fit out; out.D=D;
  V a=points[0],b=a;
  for(const V& p:points) for(int c=0;c<3;++c) { a[c]=std::min(a[c],p[c]); b[c]=std::max(b[c],p[c]); }
  double extent=std::max(b[0]-a[0],std::max(b[1]-a[1],b[2]-a[2]));
  if(!(extent>0)) Rcpp::stop("The points must span a positive extent.");
  // Surface area per sample from the disc reaching its k-th nearest
  // neighbour, pi r^2 / k. A node of width h meets about h^2 / area
  // samples, so the sample is splatted at the fractional depth where that
  // number equals samples_per_node, shared between the two nearest whole
  // depths (Kazhdan, Bolitho and Hoppe 2006, section 5).
  std::vector<double> area(n),depth(n),width(n);
  {
    normals::KdTree kd(points); std::vector<int> near;
    int k=std::max(1,std::min(opt.neighbours,n-1));
    for(int i=0;i<n;++i) {
      if(n>1) {
        kd.nearest(points[i],k+1,near);
        V far=sub(points[near.back()],points[i]);
        area[i]=3.141592653589793*dot(far,far)/(near.size()-1);
      } else area[i]=extent*extent;
      width[i]=std::max(std::sqrt(opt.samples_per_node*area[i]),opt.scale*extent/(1<<D));
    }
  }
  // The cube is scale times the largest extent, enlarged so that it reaches
  // two splat cells beyond every sample (cells capped at the 90th percentile
  // of their widths): the normal field of a sample meeting a face would be
  // cancelled by its reflection.
  std::vector<double> sorted=width;
  std::nth_element(sorted.begin(),sorted.begin()+(9*(n-1))/10,sorted.end());
  double cap=sorted[(9*(n-1))/10],half=0.5*opt.scale*extent;
  V centre; for(int c=0;c<3;++c) centre[c]=0.5*(a[c]+b[c]);
  for(int i=0;i<n;++i) for(int c=0;c<3;++c) half=std::max(half,std::abs(points[i][c]-centre[c])+2*std::min(width[i],cap));
  out.width=2*half;
  for(int c=0;c<3;++c) out.lo[c]=centre[c]-half;
  std::vector<V> u(n);
  for(int i=0;i<n;++i) u[i]=out.unit_point(points[i]);
  auto cell=[&](const V& p,int d) { int N=1<<d; I3 c; for(int k=0;k<3;++k) c[k]=std::min(std::max(static_cast<int>(std::floor(p[k]*N)),0),N-1); return c; };
  double total_area=0;
  for(int i=0;i<n;++i) {
    area[i]/=out.width*out.width; total_area+=area[i];
    double target=-0.5*std::log2(opt.samples_per_node*area[i]);
    depth[i]=std::isnan(target)?0:std::min(std::max(target,0.0),double(D));
  }

  // Normals splatted trilinearly onto the eight nearest nodes of the splat
  // depths as coefficients of a unit-integral kernel.
  Octree tree(D);
  std::vector<KeyIndex> splat(D+1); std::vector<std::vector<V>> field(D+1);
  for(int i=0;i<n;++i) {
    int coarse_depth=std::min(static_cast<int>(std::floor(depth[i])),D);
    double share=depth[i]-coarse_depth;
    for(int level=0;level<2;++level) {
      int d=coarse_depth+level; double part=level?share:1-share;
      if(d>D||part<=0) continue;
      int N=1<<d; double h=1.0/N;
      int base[3]; double frac[3];
      for(int k=0;k<3;++k) { double t=u[i][k]*N-0.5; base[k]=static_cast<int>(std::floor(t)); frac[k]=t-base[k]; }
      double w[8],sum=0; I3 c[8];
      for(int q=0;q<8;++q) {
        w[q]=1;
        for(int k=0;k<3;++k) { int bit=(q>>k)&1; c[q][k]=base[k]+bit; w[q]*=bit?frac[k]:1-frac[k]; if(c[q][k]<0||c[q][k]>=N) w[q]=0; }
        sum+=w[q];
      }
      V v=mul(normals[i],part*area[i]/(h*h*h*sum));
      for(int q=0;q<8;++q) if(w[q]>0) {
        int id=splat[d].insert(pack(c[q]));
        if(id>=static_cast<int>(field[d].size())) field[d].push_back(V{0,0,0});
        field[d][id]=add(field[d][id],mul(v,w[q]));
        if(d>0) tree.refine(d-1,I3{floor_half(c[q][0]),floor_half(c[q][1]),floor_half(c[q][2])});
      }
      if(d>0) tree.refine(d-1,cell(u[i],d-1));
    }
  }
  int full=std::min(opt.full_depth,D);
  for(int d=0;d<full;++d) { int N=1<<d; for(int x=0;x<N;++x) for(int y=0;y<N;++y) for(int z=0;z<N;++z) tree.refine(d,I3{x,y,z}); }
  // Conforming octree: the children of a refined node see the B-splines of
  // the nodes within its two-ring, which must therefore exist.
  for(int d=D-1;d>=1;--d) {
    int N=1<<d; std::vector<Key> keys=tree.refined[d].order;
    for(Key key:keys) {
      I3 r=unpack(key);
      for(int s=0;s<125;++s) {
        I3 o=offset(s),q{r[0]+o[0],r[1]+o[1],r[2]+o[2]};
        if(inside(q,N)) tree.refine(d-1,I3{q[0]/2,q[1]/2,q[2]/2});
      }
    }
  }
  out.index.resize(D+1); out.node.resize(D+1); out.x.resize(D+1);
  out.node[0].push_back(I3{0,0,0});
  for(int d=1;d<=D;++d) for(Key key:tree.refined[d-1].order) {
    I3 r=unpack(key);
    for(int q=0;q<8;++q) out.node[d].push_back(I3{2*r[0]+(q&1),2*r[1]+((q>>1)&1),2*r[2]+((q>>2)&1)});
  }
  for(int d=0;d<=D;++d) {
    out.index[d].reset(out.node[d].size());
    for(const I3& c:out.node[d]) out.index[d].insert(pack(c));
    out.nodes.push_back(out.node[d].size());
  }
  out.seconds.push_back(seconds_since(clock));

  // Right-hand sides int V . grad B_j, from the finest depth to the root:
  // the coarser node functions are combinations of finer ones, so their
  // right-hand sides are the same combinations of the finer ones.
  std::vector<Integrals> table; for(int d=0;d<=D;++d) table.emplace_back(1<<d);
  std::vector<std::vector<double>> rhs(D+1);
  {
    KeyIndex finer; std::vector<double> finer_value;
    for(int d=D;d>=0;--d) {
      int N=1<<d; double h=1.0/N;
      KeyIndex here(finer.size()/4+splat[d].size()*27+8); std::vector<double> value;
      auto add_to=[&](const I3& k,double v) { int id=here.insert(pack(k)); if(id>=static_cast<int>(value.size())) value.push_back(0); value[id]+=v; };
      for(int e=0;e<finer.size();++e) {
        I3 j=unpack(finer.order[e]); int p[3][2]; double w[3][2];
        for(int c=0;c<3;++c) parents(j[c],p[c],w[c]);
        for(int q=0;q<8;++q) {
          int x=q&1,y=(q>>1)&1,z=(q>>2)&1;
          add_to(I3{fold(p[0][x],N),fold(p[1][y],N),fold(p[2][z],N)},w[0][x]*w[1][y]*w[2][z]*finer_value[e]);
        }
      }
      const Integrals& t=table[d];
      for(int e=0;e<splat[d].size();++e) {
        I3 o=unpack(splat[d].order[e]); const V& v=field[d][e];
        for(int s=0;s<125;++s) {
          I3 off=offset(s),j{o[0]+off[0],o[1]+off[1],o[2]+off[2]};
          if(!inside(j,N)) continue;
          double mx=t.M(o[0],j[0]),my=t.M(o[1],j[1]),mz=t.M(o[2],j[2]);
          double sum=h*h*(v[0]*t.G(o[0],j[0])*my*mz+v[1]*mx*t.G(o[1],j[1])*mz+v[2]*mx*my*t.G(o[2],j[2]));
          if(sum!=0) add_to(j,sum);
        }
      }
      rhs[d].assign(out.node[d].size(),0);
      for(size_t i=0;i<out.node[d].size();++i) { int id=here.find(pack(out.node[d][i])); if(id>=0) rhs[d][i]=value[id]; }
      finer=std::move(here); finer_value=std::move(value);
    }
  }
  splat.clear(); field.clear();
  out.seconds.push_back(seconds_since(clock));

  // Cascadic solve from the root down. prolonged holds the coarser solution
  // as coefficients of this depth's node functions on the two-ring of its
  // nodes (including nodes outside the octree); coarse holds it at the samples.
  std::vector<double> coarse(n,0);
  out.ring.resize(D+1); out.acc.resize(D+1);
  for(int d=0;d<=D;++d) {
    int N=1<<d; const Integrals& t=table[d];
    Level L(N,t,out.node[d]);
    L.screen=opt.screening*std::pow(2.0,d)*total_area/n;
    // nbr first holds the ring ids of the two-ring of every node, then the
    // node ids (or -1 for ring members outside the octree).
    L.nbr.assign(125*static_cast<size_t>(L.n),-1);
    KeyIndex ring(4*static_cast<size_t>(L.n));
    for(int i=0;i<L.n;++i) {
      const I3& k=L.node[i];
      for(int s=0;s<125;++s) { I3 o=offset(s),j{k[0]+o[0],k[1]+o[1],k[2]+o[2]}; if(inside(j,N)) L.nbr[125*static_cast<size_t>(i)+s]=ring.insert(pack(j)); }
    }
    std::vector<int> own(L.n),node_of(ring.size(),-1);
    for(int i=0;i<L.n;++i) { own[i]=L.nbr[125*static_cast<size_t>(i)+62]; node_of[own[i]]=i; }
    L.sid.assign(27*static_cast<size_t>(n),-1); L.sval.assign(27*static_cast<size_t>(n),0);
    for(int p=0;p<n;++p) support(u[p],N,out.index[d],&L.sid[27*static_cast<size_t>(p)],&L.sval[27*static_cast<size_t>(p)],nullptr);
    std::vector<double> prolonged(ring.size(),0);
    if(d>0) for(int e=0;e<ring.size();++e) {
      I3 j=unpack(ring.order[e]); int p[3][2]; double w[3][2]; double sum=0;
      for(int c=0;c<3;++c) parents(j[c],p[c],w[c]);
      for(int q=0;q<8;++q) {
        int x=q&1,y=(q>>1)&1,z=(q>>2)&1;
        int id=out.ring[d-1].find(pack(I3{fold(p[0][x],N/2),fold(p[1][y],N/2),fold(p[2][z],N/2)}));
        if(id>=0) sum+=w[0][x]*w[1][y]*w[2][z]*out.acc[d-1][id];
      }
      prolonged[e]=sum;
    }
    std::vector<double> b=rhs[d];
    if(d>0) {
      for(int i=0;i<L.n;++i) {
        double sum=0; const int* adj=&L.nbr[125*static_cast<size_t>(i)]; const double* w=L.row(i);
        for(int s=0;s<125;++s) if(adj[s]>=0) sum+=w[s]*prolonged[adj[s]];
        b[i]-=sum;
      }
      for(int p=0;p<n;++p) for(int q=0;q<27;++q) { int j=L.sid[27*p+q]; if(j>=0) b[j]-=L.screen*L.sval[27*p+q]*coarse[p]; }
    }
    for(int& e:L.nbr) if(e>=0) e=node_of[e];
    int iterations=0;
    out.x[d]=conjugate_gradients(L,b,opt.tol,opt.max_iter,iterations);
    out.iterations.push_back(iterations);
    for(int i=0;i<L.n;++i) prolonged[own[i]]+=out.x[d][i];
    for(int p=0;p<n;++p) for(int q=0;q<27;++q) { int j=L.sid[27*p+q]; if(j>=0) coarse[p]+=L.sval[27*p+q]*out.x[d][j]; }
    out.ring[d]=std::move(ring); out.acc[d]=std::move(prolonged);
  }
  out.finer.resize(D+1);
  for(int d=0;d<=D;++d) out.finer[d].assign(out.node[d].size()*static_cast<size_t>(D+1),0);
  for(int d=D;d>=1;--d) for(size_t i=0;i<out.node[d].size();++i) {
    double m=std::abs(out.x[d][i]); I3 c=out.node[d][i];
    for(int e=d-1;e>=0;--e) {
      for(int a=0;a<3;++a) c[a]/=2;
      double& slot=out.finer[e][out.index[e].find(pack(c))*static_cast<size_t>(D+1)+d];
      if(slot>=m) break;
      slot=m;
    }
  }
  out.seconds.push_back(seconds_since(clock));
  double iso=0; for(double v:coarse) iso+=v;
  out.iso=iso/n;
  out.tree=std::move(tree);
  return out;
}

// Positions of the level on segments a-b after `steps` Illinois regula falsi
// steps of the fitted function.
static inline void crossings(const Fit& f,const std::vector<V>& a,const std::vector<V>& b,const std::vector<double>& fa,
                             const std::vector<double>& fb,double level,int steps,std::vector<V>& out) {
  out.resize(a.size());
  for(size_t e=0;e<a.size();++e) {
    double ta=0,tb=1,va=fa[e]-level,vb=fb[e]-level; int last=0;
    auto secant=[&]() { return vb!=va?(ta*vb-tb*va)/(vb-va):0.5*(ta+tb); };
    for(int s=0;s<steps;++s) {
      double t=secant(); double v=f.value(add(a[e],mul(sub(b[e],a[e]),t)),nullptr)-level;
      if(!std::isfinite(v)) break;
      if((v>=0)==(va>=0)) { if(last==1) vb*=0.5; ta=t; va=v; last=1; }
      else { if(last==-1) va*=0.5; tb=t; vb=v; last=-1; }
    }
    out[e]=add(a[e],mul(sub(b[e],a[e]),secant()));
  }
}

// Level surface of the fit at its iso-value (the mean of chi at the
// samples), by dual contouring on the leaves of the solve octree.
template<bool hermite>
static inline Mesh extract(const Fit& f,int refine) {
  int D=f.D,lattice=1<<D; std::vector<octree::P3> origin; std::vector<int> size;
  for(int d=0;d<=D;++d) for(const I3& c:f.node[d]) if(!f.tree.is_refined(d,c)) {
    int s=lattice>>d; origin.push_back(octree::P3{c[0]*s,c[1]*s,c[2]*s}); size.push_back(s);
  }
  V hi; for(int a=0;a<3;++a) hi[a]=f.lo[a]+f.width;
  octree::Tree tree(D,f.lo,hi,false,origin,size);
  std::vector<char> empty(origin.size(),0);
  size_t leaf=0;
  for(int d=0;d<=D;++d) for(const I3& c:f.node[d]) if(!f.tree.is_refined(d,c)) empty[leaf++]=!f.may_meet(d,c,f.iso);
  octree::ValueFunction value=[&f](const std::vector<V>& p,std::vector<double>& out) {
    out.resize(p.size());
    for(size_t i=0;i<p.size();++i) { if(i%65536==0) Rcpp::checkUserInterrupt(); out[i]=f.value(p[i],nullptr); }
  };
  octree::CrossingFunction place;
  if(refine>0) place=[&f,refine](const std::vector<V>& a,const std::vector<V>& b,const std::vector<double>& fa,const std::vector<double>& fb,double level,std::vector<V>& out) {
    crossings(f,a,b,fa,fb,level,refine,out);
  };
  dual::GradientFunction gradient=[&f](const std::vector<V>& p,std::vector<V>& out) {
    out.resize(p.size());
    for(size_t i=0;i<p.size();++i) f.value(p[i],&out[i]);
  };
  return octree::extract<hermite>(tree,empty,std::vector<double>{f.iso},value,place,gradient);
}
}
}
#endif
