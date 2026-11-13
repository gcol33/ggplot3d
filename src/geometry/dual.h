#ifndef GGPLOT3D_GEOMETRY_DUAL_H
#define GGPLOT3D_GEOMETRY_DUAL_H
#include "isosurface.h"
#include <functional>
namespace ggplot3d_geometry {
namespace dual {

// Eigen-decomposition of a symmetric 3x3 matrix by cyclic Jacobi rotations;
// a is destroyed, w receives the eigenvalues and the columns of q the vectors.
static inline void symmetric_eigen(double a[3][3],double w[3],double q[3][3]) {
  for(int i=0;i<3;++i) for(int j=0;j<3;++j) q[i][j]=i==j;
  for(int sweep=0;sweep<50;++sweep) {
    double off=a[0][1]*a[0][1]+a[0][2]*a[0][2]+a[1][2]*a[1][2];
    double diagonal=a[0][0]*a[0][0]+a[1][1]*a[1][1]+a[2][2]*a[2][2];
    if(off<=1e-30*diagonal||off==0) break;
    for(int p=0;p<2;++p) for(int r=p+1;r<3;++r) {
      if(a[p][r]==0) continue;
      double theta=(a[r][r]-a[p][p])/(2*a[p][r]);
      double t=(theta>=0?1:-1)/(std::abs(theta)+std::sqrt(theta*theta+1));
      double c=1/std::sqrt(t*t+1),s=t*c;
      for(int k=0;k<3;++k) { double kp=a[k][p],kr=a[k][r]; a[k][p]=c*kp-s*kr; a[k][r]=s*kp+c*kr; }
      for(int k=0;k<3;++k) { double pk=a[p][k],rk=a[r][k]; a[p][k]=c*pk-s*rk; a[r][k]=s*pk+c*rk; }
      for(int k=0;k<3;++k) { double kp=q[k][p],kr=q[k][r]; q[k][p]=c*kp-s*kr; q[k][r]=s*kp+c*kr; }
    }
  }
  for(int i=0;i<3;++i) w[i]=a[i][i];
}

// Dual contouring vertex (Ju, Losasso, Schaefer and Warren 2002): minimiser
// of sum (n_i . (x - p_i))^2 relative to the mass point, with eigenvalues
// below 1 % of the largest (singular values below 0.1 of the largest)
// truncated so flat and creased data fall back towards the mass point,
// clamped to the cell.
static inline V qef_vertex(const std::vector<V>& points,const std::vector<V>& normals,V lo,V hi) {
  V mass{0,0,0};
  for(const V& p:points) mass=add(mass,p);
  mass=mul(mass,1.0/points.size());
  double a[3][3]={{0,0,0},{0,0,0},{0,0,0}}; V r{0,0,0};
  for(size_t i=0;i<points.size();++i) {
    const V& n=normals[i]; double d=dot(n,sub(points[i],mass));
    for(int x=0;x<3;++x) { for(int y=0;y<3;++y) a[x][y]+=n[x]*n[y]; r[x]+=n[x]*d; }
  }
  double w[3],q[3][3]; symmetric_eigen(a,w,q);
  double largest=std::max(w[0],std::max(w[1],w[2]));
  V x=mass;
  if(largest>0) for(int k=0;k<3;++k) {
    if(w[k]<0.01*largest) continue;
    V u{q[0][k],q[1][k],q[2][k]};
    x=add(x,mul(u,dot(u,r)/w[k]));
  }
  for(int c=0;c<3;++c) x[c]=std::min(std::max(x[c],lo[c]),hi[c]);
  return x;
}

// The grid, optionally surrounded by one layer of points above every level
// at the box coordinates, so the sublevel region closes on the grid box.
struct Padded {
  const ScalarGrid& grid; int pad; int n[3];
  Padded(const ScalarGrid& grid,bool caps): grid(grid),pad(caps),n{grid.nx,grid.ny,grid.nz} {}
  bool inside(int i,int j,int k) const { return i>=0&&j>=0&&k>=0&&i<n[0]&&j<n[1]&&k<n[2]; }
  int id(int i,int j,int k) const { return grid.index(i,j,k); }
  V point(int i,int j,int k) const {
    int q[3]={i,j,k}; V p;
    for(int a=0;a<3;++a) p[a]=grid.axes[a][std::min(std::max(q[a],0),n[a]-1)];
    return p;
  }
  bool finite(int i,int j,int k) const { return !inside(i,j,k)||grid.finite(id(i,j,k)); }
  double value(int i,int j,int k,double level) const { return inside(i,j,k)?grid.sample(id(i,j,k),level)-level:1; }
  V outward(int i,int j,int k) const {
    int q[3]={i,j,k}; V u{0,0,0};
    for(int a=0;a<3;++a) u[a]=q[a]<0?-1:(q[a]>=n[a]?1:0);
    return u;
  }
  V gradient(int i,int j,int k) const { return inside(i,j,k)?grid.gradients[id(i,j,k)]:outward(i,j,k); }
  int first_cell() const { return -pad; }
  int cells(int axis) const { return n[axis]-1+2*pad; }
};

struct LevelSlots {
  double level; std::array<std::vector<int>,2> slots;
  LevelSlots(double level,int size): level(level) { for(auto& s:slots) s.assign(size,-1); }
};

// Hermite data of one layer of cells: every boundary loop of the level set on
// the cell faces, with faces resolved by the asymptotic decider as in marching
// cubes, becomes one cell vertex (manifold dual contouring, Schaefer, Ju and
// Warren 2007). A tunnel through a single cell cannot be carried by cell
// vertices; its two loops close as separate sheets. Crossing normals are the
// interpolated grid gradients, or the box normal on a padding edge, unless a
// gradient function replaces those of interior edges.
struct Hermite {
  struct Loop { int* slot; std::vector<int> edges; size_t first; V lo, hi; };
  std::vector<Loop> loops; std::vector<V> points, normals; std::vector<char> interior;
  void clear() { loops.clear(); points.clear(); normals.clear(); interior.clear(); }
};

static inline void collect(Hermite& h,const Padded& g,int i,int j,int k,double level,int* slot) {
  std::fill(slot,slot+12,-1);
  int q[8][3]; double v[8]; int mask=0;
  for(int c=0;c<8;++c) {
    q[c][0]=i+(c&1); q[c][1]=j+((c>>1)&1); q[c][2]=k+((c>>2)&1);
    if(!g.finite(q[c][0],q[c][1],q[c][2])) return;
    v[c]=g.value(q[c][0],q[c][1],q[c][2],level); mask|=(v[c]>=0)<<c;
  }
  if(mask==0||mask==255) return;
  for(const auto& loop:cubes::analyse(v).loops) {
    h.loops.push_back({slot,loop,h.points.size(),g.point(q[0][0],q[0][1],q[0][2]),g.point(q[7][0],q[7][1],q[7][2])});
    for(int e:loop) {
      const int* a=q[cubes::edges[e][0]]; const int* b=q[cubes::edges[e][1]];
      double va=v[cubes::edges[e][0]],vb=v[cubes::edges[e][1]],t=va/(va-vb);
      V pa=g.point(a[0],a[1],a[2]),pb=g.point(b[0],b[1],b[2]),n;
      bool inside=g.inside(a[0],a[1],a[2])&&g.inside(b[0],b[1],b[2]);
      if(!inside) n=unit(add(g.outward(a[0],a[1],a[2]),g.outward(b[0],b[1],b[2])));
      else { V ga=g.gradient(a[0],a[1],a[2]),gb=g.gradient(b[0],b[1],b[2]); n=unit(add(ga,mul(sub(gb,ga),t))); }
      h.points.push_back(add(pa,mul(sub(pb,pa),t))); h.normals.push_back(n); h.interior.push_back(inside);
    }
  }
}

template<bool hermite>
static inline void resolve(Mesh& mesh,const Hermite& h) {
  for(const auto& loop:h.loops) {
    size_t n=loop.edges.size();
    std::vector<V> points(h.points.begin()+loop.first,h.points.begin()+loop.first+n);
    std::vector<V> normals(h.normals.begin()+loop.first,h.normals.begin()+loop.first+n);
    Vertex x; x.n=V{0,0,0};
    for(const V& m:normals) x.n=add(x.n,m);
    x.n=unit(x.n);
    if(hermite) x.p=qef_vertex(points,normals,loop.lo,loop.hi);
    else { x.p=V{0,0,0}; for(const V& p:points) x.p=add(x.p,p); x.p=mul(x.p,1.0/n); }
    int id=mesh.add(x);
    for(int e:loop.edges) loop.slot[e]=id;
  }
}

using GradientFunction=std::function<void(const std::vector<V>&,std::vector<V>&)>;

// One polygon per sign-changing grid edge, joining the vertices of the four
// cells around it in cyclic order, facing increasing field value. A plain
// quad is split along its shorter diagonal; a polygon with split arc vertices
// is fanned around its mean. An edge with one endpoint in the padding layer
// gives a cap polygon with the box normal.
static inline void polygon(Mesh& mesh,std::vector<int> ids,bool reverse,double level,bool cap,V normal) {
  if(reverse) std::reverse(ids.begin(),ids.end());
  if(ids.size()==4) {
    V p[4]; for(int a=0;a<4;++a) p[a]=mesh.vertices[ids[a]].p;
    if(norm(sub(p[2],p[0]))<=norm(sub(p[3],p[1]))) {
      mesh.oriented(ids[0],ids[1],ids[2],level,cap,normal); mesh.oriented(ids[0],ids[2],ids[3],level,cap,normal);
    } else {
      mesh.oriented(ids[0],ids[1],ids[3],level,cap,normal); mesh.oriented(ids[1],ids[2],ids[3],level,cap,normal);
    }
    return;
  }
  Vertex centre; centre.p=V{0,0,0}; centre.n=V{0,0,0};
  for(int id:ids) { centre.p=add(centre.p,mesh.vertices[id].p); centre.n=add(centre.n,mesh.vertices[id].n); }
  centre.p=mul(centre.p,1.0/ids.size()); centre.n=unit(centre.n);
  int c=mesh.add(centre),n=ids.size();
  for(int a=0;a<n;++a) mesh.oriented(ids[a],ids[(a+1)%n],c,level,cap,normal);
}

// Two cells sharing a face meet along the arcs of the level curve on that
// face. A face whose corners alternate in sign carries two arcs; when both
// join the same two cell vertices, the second arc gets a vertex of its own,
// at the mean of the two, so the surface stays a 2-manifold.
struct Arcs {
  std::map<std::pair<int,int>,long long> first;
  std::map<long long,int> split;
};

// Dual extraction of every level in one sweep over cell layers; two layers
// of per-cell edge slots per level are enough to build every quad.
template<bool hermite>
static inline Mesh extract(const ScalarGrid& grid,const std::vector<double>& levels,bool caps,const GradientFunction& gradient) {
  Padded g(grid,caps);
  int c0=g.first_cell(),cx=g.cells(0),cy=g.cells(1),cz=g.cells(2),nl=levels.size();
  std::vector<Mesh> meshes(nl);
  std::vector<LevelSlots> states; states.reserve(nl);
  for(int l=0;l<nl;++l) states.emplace_back(levels[l],cx*cy*12);
  auto slot=[&](LevelSlots& s,int i,int j,int k) { return &s.slots[(k-c0)&1][12*((i-c0)+cx*(j-c0))]; };
  auto high=[&](int i,int j,int k,double level) { return g.value(i,j,k,level)>=0; };
  std::vector<Arcs> arcs(nl); Hermite layer;
  long long width=std::max(g.n[0],std::max(g.n[1],g.n[2]))+3;
  // Vertex of the level-curve arc between consecutive cells P and Q around the
  // grid edge a-a+axis, or -1 when the arc needs no vertex of its own.
  auto arc_vertex=[&](Mesh& mesh,Arcs& seen,double level,const int a[3],int axis,const int* P,const int* Q,int A,int B) {
    int d=0; while(P[d]==Q[d]) ++d;
    int t=3-d-axis,plane=std::max(P[d],Q[d]),t0=P[t];
    int f[4][3]; double v[4]; bool h[4];
    for(int c=0;c<4;++c) {
      f[c][d]=plane; f[c][axis]=a[axis]+(c==1||c==2); f[c][t]=t0+(c>=2);
      v[c]=g.value(f[c][0],f[c][1],f[c][2],level); h[c]=v[c]>=0;
    }
    if(!(h[0]==h[2]&&h[1]==h[3]&&h[0]!=h[1])) return -1;
    bool joined=cubes::high_joined(v[0],v[1],v[2],v[3],h[0]);
    int cut[3]={a[0],a[1],a[2]};
    if((g.value(a[0],a[1],a[2],level)>=0)==joined) cut[axis]+=1;
    int corner=(cut[0]-f[0][0])|((cut[1]-f[0][1])<<1)|((cut[2]-f[0][2])<<2);
    long long key=((((long long)d*width+f[0][0]+1)*width+f[0][1]+1)*width+f[0][2]+1)*8+corner;
    auto pair=std::minmax(A,B);
    auto found=seen.first.find(pair);
    if(found==seen.first.end()) { seen.first[pair]=key; return -1; }
    if(found->second==key) return -1;
    auto made=seen.split.find(key);
    if(made!=seen.split.end()) return made->second;
    Vertex m; m.p=mul(add(mesh.vertices[A].p,mesh.vertices[B].p),0.5); m.n=unit(add(mesh.vertices[A].n,mesh.vertices[B].n));
    int id=mesh.add(m); seen.split[key]=id; return id;
  };
  auto edge=[&](int l,const int a[3],const int b[3],int axis,const int cells[4][4]) {
    LevelSlots& s=states[l]; Mesh& mesh=meshes[l];
    bool ha=high(a[0],a[1],a[2],s.level),hb=high(b[0],b[1],b[2],s.level);
    if(ha==hb||!g.finite(a[0],a[1],a[2])||!g.finite(b[0],b[1],b[2])) return;
    int ids[4];
    for(int c=0;c<4;++c) { ids[c]=slot(s,cells[c][0],cells[c][1],cells[c][2])[cells[c][3]]; if(ids[c]<0) return; }
    std::vector<int> poly;
    for(int c=0;c<4;++c) {
      poly.push_back(ids[c]);
      int m=arc_vertex(mesh,arcs[l],s.level,a,axis,cells[c],cells[(c+1)%4],ids[c],ids[(c+1)%4]);
      if(m>=0) poly.push_back(m);
    }
    // The cell lists run counterclockwise around +x and +z and clockwise around +y.
    double facing=(hb?1:-1)*(axis==1?-1:1);
    V normal{0,0,0}; normal[axis]=hb?1:-1;
    bool cap=!g.inside(a[0],a[1],a[2])||!g.inside(b[0],b[1],b[2]);
    polygon(mesh,poly,facing<0,s.level,cap,normal);
  };
  for(int k=c0;k<c0+cz;++k) {
    Rcpp::checkUserInterrupt();
    for(int l=0;l<nl;++l) {
      LevelSlots& s=states[l]; Mesh& mesh=meshes[l];
      layer.clear();
      for(int j=c0;j<c0+cy;++j) for(int i=c0;i<c0+cx;++i) collect(layer,g,i,j,k,s.level,slot(s,i,j,k));
      if(hermite&&gradient&&!layer.points.empty()) {
        std::vector<V> points,normals;
        for(size_t p=0;p<layer.points.size();++p) if(layer.interior[p]) points.push_back(layer.points[p]);
        gradient(points,normals);
        for(size_t p=0,r=0;p<layer.points.size();++p) if(layer.interior[p]) {
          V n=unit(normals[r++]);
          if(norm(n)>0) layer.normals[p]=n;
        }
      }
      resolve<hermite>(mesh,layer);
      for(int j=c0+1;j<c0+cy;++j) for(int i=c0+1;i<c0+cx;++i) {
        int a[3]={i,j,k},b[3]={i,j,k+1};
        const int cells[4][4]={{i-1,j-1,k,11},{i,j-1,k,10},{i,j,k,8},{i-1,j,k,9}};
        edge(l,a,b,2,cells);
      }
      if(k==c0) continue;
      for(int j=c0+1;j<c0+cy;++j) for(int i=c0;i<c0+cx;++i) {
        int a[3]={i,j,k},b[3]={i+1,j,k};
        const int cells[4][4]={{i,j-1,k-1,3},{i,j,k-1,2},{i,j,k,0},{i,j-1,k,1}};
        edge(l,a,b,0,cells);
      }
      for(int j=c0;j<c0+cy;++j) for(int i=c0+1;i<c0+cx;++i) {
        int a[3]={i,j,k},b[3]={i,j+1,k};
        const int cells[4][4]={{i-1,j,k-1,7},{i,j,k-1,6},{i,j,k,4},{i-1,j,k,5}};
        edge(l,a,b,1,cells);
      }
    }
  }
  Mesh mesh;
  for(auto& m:meshes) mesh.append(m);
  return mesh;
}
}
}
#endif
