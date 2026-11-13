#ifndef GGPLOT3D_GEOMETRY_ISOSURFACE_H
#define GGPLOT3D_GEOMETRY_ISOSURFACE_H
#include "field.h"
#include <string>
namespace ggplot3d_geometry {

// One level and its welded vertices.
struct LevelState {
  double level; VertexSlab slab;
  LevelState(double level,int nx,int ny): level(level),slab(nx,ny) {}
};

// Cell position and the grid ids of its eight corners.
struct Cell { int i,j,k; int ids[8]; };

namespace tetra {
constexpr int tets[6][4]={{0,1,3,7},{0,3,2,7},{0,2,6,7},{0,6,4,7},{0,4,5,7},{0,5,1,7}};
constexpr int faces[4][3]={{0,1,2},{0,1,3},{0,2,3},{1,2,3}};

static inline void tetrahedron(Mesh& mesh,LevelState& s,const ScalarGrid& grid,const std::array<int,4>& t) {
  double level=s.level; int hi[4],lo[4],nh=0,nl=0;
  for(int a:t) { if(grid.high(a,level)) hi[nh++]=a; else lo[nl++]=a; }
  if(!nh||!nl) return;
  V direction=sub(grid.point(hi[0]),grid.point(lo[0]));
  auto e=[&](int a,int b) { return edge_vertex(mesh,s.slab,grid,a,b,level); };
  if(nh==1||nl==1) {
    int a=nh==1?hi[0]:lo[0]; const int* others=nh==1?lo:hi;
    mesh.triangle(e(a,others[0]),e(a,others[1]),e(a,others[2]),direction,level);
  } else {
    int a=e(hi[0],lo[0]),b=e(hi[0],lo[1]),c=e(hi[1],lo[0]),d=e(hi[1],lo[1]);
    mesh.triangle(a,b,d,direction,level); mesh.triangle(a,d,c,direction,level);
  }
}

static inline void cell(Mesh& mesh,LevelState& s,const ScalarGrid& grid,const Cell& c,bool caps) {
  for(auto& tet:tets) {
    std::array<int,4> t{c.ids[tet[0]],c.ids[tet[1]],c.ids[tet[2]],c.ids[tet[3]]};
    tetrahedron(mesh,s,grid,t);
    if(!caps) continue;
    for(auto& f:faces) for(int axis=0;axis<3;++axis) {
      V a=grid.point(t[f[0]]),b=grid.point(t[f[1]]),cc=grid.point(t[f[2]]);
      if(a[axis]!=b[axis]||a[axis]!=cc[axis]) continue;
      std::vector<int> tri{t[f[0]],t[f[1]],t[f[2]]};
      if(a[axis]==grid.axes[axis].front()) cap_face(mesh,s.slab,grid,tri,s.level,axis,-1,false);
      else if(a[axis]==grid.axes[axis].back()) cap_face(mesh,s.slab,grid,tri,s.level,axis,1,false);
    }
  }
}

// Whether moving u and v to target keeps every triangle around them that does
// not contain both non-degenerate and facing the same side.
static inline bool keeps_shape(const Mesh& mesh,const std::vector<int>& faces,int u,int v,V target) {
  for(int f:faces) {
    const auto& t=mesh.triangles[f];
    if(t[0]<0) continue;
    bool hu=t[0]==u||t[1]==u||t[2]==u,hv=t[0]==v||t[1]==v||t[2]==v;
    if(hu&&hv) continue;
    V p[3],q[3];
    for(int k=0;k<3;++k) { p[k]=mesh.vertices[t[k]].p; q[k]=t[k]==u||t[k]==v?target:p[k]; }
    V before=cross(sub(p[1],p[0]),sub(p[2],p[0])),after=cross(sub(q[1],q[0]),sub(q[2],q[0]));
    double scale=std::max(dot(sub(q[1],q[0]),sub(q[1],q[0])),dot(sub(q[2],q[0]),sub(q[2],q[0])));
    if(dot(before,after)<=0||norm(after)<=1024*std::numeric_limits<double>::epsilon()*scale) return false;
  }
  return true;
}

// Regularised marching tetrahedra (Treece, Prager and Gee 1999): every edge
// crossing belongs to the cluster of the grid point it lies closest to, and
// surface edges joining two crossings of one cluster are collapsed, merging
// the cluster into one vertex at the mean position of its crossings. A
// collapse is skipped when it would break the 2-manifold: the endpoints'
// common neighbours must be exactly the apexes of the edge's triangles, and
// two boundary vertices are only merged along a boundary edge. Crossings are
// only merged with crossings on the same faces of the grid box, so caps stay
// planar and every merged vertex is a mean of points on the same box planes.
static inline void regularise(Mesh& mesh,const ScalarGrid& grid) {
  int n=mesh.vertices.size(),nt=mesh.triangles.size();
  if(n<=0) return;
  std::vector<int> cluster(n);
  for(int v=0;v<n;++v) { const Vertex& x=mesh.vertices[v]; cluster[v]=x.ea==x.eb?-1:(x.t<0.5?x.ea:x.eb); }
  std::vector<std::vector<int>> incident(n);
  for(int f=0;f<nt;++f) for(int v:mesh.triangles[f]) if(v>=0) incident[v].push_back(f);
  auto alive=[&](int f) { return mesh.triangles[f][0]>=0; };
  auto neighbours=[&](int v) {
    std::vector<int> out;
    for(int f:incident[v]) if(alive(f)) for(int w:mesh.triangles[f]) if(w!=v) out.push_back(w);
    std::sort(out.begin(),out.end()); out.erase(std::unique(out.begin(),out.end()),out.end());
    return out;
  };
  auto edge_faces=[&](int u,int v) {
    std::vector<int> out;
    for(int f:incident[u]) if(alive(f)) { auto& t=mesh.triangles[f]; if(t[0]==v||t[1]==v||t[2]==v) out.push_back(f); }
    return out;
  };
  auto on_boundary=[&](int v) {
    for(int w:neighbours(v)) if(edge_faces(v,w).size()==1) return true;
    return false;
  };
  std::vector<V> sum_p(n),sum_n(n); std::vector<int> count(n,1);
  std::vector<std::array<int,3>> box(n,std::array<int,3>{0,0,0});
  for(int v=0;v<n;++v) {
    const Vertex& x=mesh.vertices[v]; sum_p[v]=x.p; sum_n[v]=x.n;
    for(int a=0;a<3;++a) if(x.ea!=x.eb&&grid.on_boundary(x.ea,a)&&grid.ijk(x.ea)[a]==grid.ijk(x.eb)[a])
      box[v][a]=grid.ijk(x.ea)[a]==0?-1:1;
  }
  bool changed=true;
  while(changed) {
    changed=false;
    for(int f=0;f<nt;++f) for(int e=0;e<3&&alive(f);++e) {
      int u=mesh.triangles[f][e],v=mesh.triangles[f][(e+1)%3],cu=cluster[u];
      if(cu<0||cu!=cluster[v]) continue;
      std::vector<int> faces=edge_faces(u,v),apex,common;
      for(int g:faces) for(int w:mesh.triangles[g]) if(w!=u&&w!=v) apex.push_back(w);
      std::sort(apex.begin(),apex.end());
      auto nu=neighbours(u),nv=neighbours(v);
      std::set_intersection(nu.begin(),nu.end(),nv.begin(),nv.end(),std::back_inserter(common));
      common.erase(std::remove(common.begin(),common.end(),u),common.end());
      common.erase(std::remove(common.begin(),common.end(),v),common.end());
      if(common!=apex) continue;
      if(faces.size()==2&&on_boundary(u)&&on_boundary(v)) continue;
      if(box[u]!=box[v]) continue;
      V target=mul(add(sum_p[u],sum_p[v]),1.0/(count[u]+count[v]));
      if(!keeps_shape(mesh,incident[u],u,v,target)||!keeps_shape(mesh,incident[v],u,v,target)) continue;
      for(int g:incident[v]) if(alive(g)) {
        auto& t=mesh.triangles[g];
        for(int& w:t) if(w==v) w=u;
        if(t[0]==t[1]||t[1]==t[2]||t[0]==t[2]) t={-1,-1,-1}; else incident[u].push_back(g);
      }
      incident[v].clear();
      sum_p[u]=add(sum_p[u],sum_p[v]); sum_n[u]=add(sum_n[u],sum_n[v]); count[u]+=count[v];
      mesh.vertices[u].p=target; mesh.vertices[u].n=unit(sum_n[u]); mesh.vertices[u].ea=mesh.vertices[u].eb=-1;
      changed=true;
    }
  }
}
}

namespace cubes {
constexpr int edges[12][2]={{0,1},{2,3},{4,5},{6,7},{0,2},{1,3},{4,6},{5,7},{0,4},{1,5},{2,6},{3,7}};
// Faces as cyclic corner lists with their axis and outward side.
constexpr int faces[6][4]={{0,2,6,4},{1,3,7,5},{0,1,5,4},{2,3,7,6},{0,1,3,2},{4,5,7,6}};
constexpr int face_axis[6]={0,0,1,1,2,2};
constexpr int face_side[6]={-1,1,-1,1,-1,1};
// Slices perpendicular to each axis: the four corners with that bit clear, in cyclic order.
constexpr int slices[3][4]={{0,2,6,4},{0,1,5,4},{0,1,3,2}};

static inline int edge_of(int a,int b) {
  for(int e=0;e<12;++e) if((edges[e][0]==a&&edges[e][1]==b)||(edges[e][0]==b&&edges[e][1]==a)) return e;
  return -1;
}
static inline int edge_faces(int e) {
  int mask=0;
  for(int f=0;f<6;++f) for(int j=0;j<4;++j) {
    int a=faces[f][j],b=faces[f][(j+1)%4];
    if((edges[e][0]==a&&edges[e][1]==b)||(edges[e][0]==b&&edges[e][1]==a)) mask|=1<<f;
  }
  return mask;
}
static inline V unit_corner(int c) { return {double(c&1),double((c>>1)&1),double((c>>2)&1)}; }
static inline V unit_midpoint(int e) { return mul(add(unit_corner(edges[e][0]),unit_corner(edges[e][1])),0.5); }

// Asymptotic decider (Nielson and Hamann 1991): on a bilinear face whose
// corners alternate in sign, the high diagonal is connected exactly when the
// saddle value is not below the level, i.e. when the product of the high pair
// is at least the product of the low pair (values relative to the level).
static inline bool high_joined(double a,double b,double c,double d,bool a_high) {
  return a_high ? a*c>=b*d : b*d>=a*c;
}

struct Analysis {
  int next[12];
  int loops=0, loop_edges[12][12], loop_size[12];
  int split_low[6];
};

// Topology of the trilinear interpolant in one cube, following the case
// analysis of Marching Cubes 33 (Chernyaev 1995; Lewiner et al. 2003): face
// ambiguities are resolved by the asymptotic decider and interior ambiguities
// by the bilinear slice test, here applied along all three axes. The result is
// the set of closed boundary loops on the cube surface, oriented with the high
// region on their left seen from outside, and the pairs of loops that bound a
// tunnel through the cube interior.
static inline void cell(Mesh& mesh,LevelState& s,const ScalarGrid& grid,const Cell& c,bool caps) {
  double level=s.level,v[8]; bool hi[8]; int mask=0;
  for(int a=0;a<8;++a) { v[a]=grid.sample(c.ids[a],level)-level; hi[a]=v[a]>=0; mask|=hi[a]<<a; }
  bool split_low[6]={false,false,false,false,false,false};
  int next[12]; std::fill(next,next+12,-1);
  UnionFind surface(8);
  for(auto& e:edges) if(hi[e[0]]==hi[e[1]]) surface.unite(e[0],e[1]);
  if(mask!=0&&mask!=255) for(int f=0;f<6;++f) {
    const int* q=faces[f]; int crossing[4],n=0;
    for(int j=0;j<4;++j) if(hi[q[j]]!=hi[q[(j+1)%4]]) crossing[n++]=j;
    std::vector<std::array<int,3>> segments;
    if(n==2) {
      int ja=crossing[0],jb=crossing[1];
      int ea=edge_of(q[ja],q[(ja+1)%4]),eb=edge_of(q[jb],q[(jb+1)%4]),ref=-1;
      if((ja+1)%4==jb||(jb+1)%4==ja) {
        int shared=(ja+1)%4==jb?q[jb]:q[ja];
        ref=hi[shared]?shared:(edges[ea][0]==shared?edges[ea][1]:edges[ea][0]);
      } else for(int j=0;j<4;++j) if(hi[q[j]]) { ref=q[j]; break; }
      segments.push_back({ea,eb,ref});
    } else if(n==4) {
      bool joined=high_joined(v[q[0]],v[q[1]],v[q[2]],v[q[3]],hi[q[0]]);
      split_low[f]=joined;
      if(joined) surface.unite(hi[q[0]]?q[0]:q[1],hi[q[0]]?q[2]:q[3]);
      else surface.unite(hi[q[0]]?q[1]:q[0],hi[q[0]]?q[3]:q[2]);
      for(int j=0;j<4;++j) {
        int corner=q[j];
        if(hi[corner]==joined) continue;
        int ea=edge_of(q[(j+3)%4],corner),eb=edge_of(corner,q[(j+1)%4]);
        segments.push_back({ea,eb,hi[corner]?corner:q[(j+1)%4]});
      }
    }
    V outward{0,0,0}; outward[face_axis[f]]=face_side[f];
    for(auto& sg:segments) {
      V ma=unit_midpoint(sg[0]),mb=unit_midpoint(sg[1]);
      bool forward=dot(cross(sub(mb,ma),mul(outward,-1)),sub(unit_corner(sg[2]),ma))>0;
      if(forward) next[sg[0]]=sg[1]; else next[sg[1]]=sg[0];
    }
  }
  if(caps) for(int f=0;f<6;++f) {
    int axis=face_axis[f],q=f%2?(axis==0?c.i+1:axis==1?c.j+1:c.k+1):(axis==0?c.i:axis==1?c.j:c.k);
    int n=axis==0?grid.nx:axis==1?grid.ny:grid.nz;
    if((face_side[f]<0&&q!=0)||(face_side[f]>0&&q!=n-1)) continue;
    std::vector<int> corners; for(int j=0;j<4;++j) corners.push_back(c.ids[faces[f][j]]);
    cap_face(mesh,s.slab,grid,corners,level,axis,face_side[f],split_low[f]);
  }
  if(mask==0||mask==255) return;

  // Boundary loops.
  std::vector<std::vector<int>> loops; bool used[12]={false};
  for(int e=0;e<12;++e) {
    if(next[e]<0||used[e]) continue;
    std::vector<int> loop; int x=e;
    while(x>=0&&!used[x]) { used[x]=true; loop.push_back(x); x=next[x]; }
    if(x==e&&loop.size()>=3) loops.push_back(loop);
  }

  // Interior connections: a bilinear slice perpendicular to an axis whose
  // corners alternate in sign connects one diagonal pair through the interior.
  UnionFind inner=surface;
  for(int axis=0;axis<3;++axis) {
    const int* q=slices[axis]; int bit=1<<axis; double alpha[4],beta[4];
    for(int j=0;j<4;++j) { alpha[j]=v[q[j]]; beta[j]=v[q[j]|bit]-v[q[j]]; }
    for(int pattern=0;pattern<2;++pattern) {
      double lo=0,up=1;
      for(int j=0;j<4;++j) {
        bool want=(j%2==0)==(pattern==0);
        if(beta[j]==0) { if((alpha[j]>=0)!=want) up=-1; continue; }
        double root=-alpha[j]/beta[j];
        if((beta[j]>0)==want) lo=std::max(lo,root); else up=std::min(up,root);
      }
      if(lo>=up) continue;
      int h0=pattern==0?0:1,h1=h0+2,l0=1-h0,l1=l0+2;
      auto w=[&](int j,double t) { return alpha[j]+beta[j]*t; };
      auto excess=[&](double t) { return w(h0,t)*w(h1,t)-w(l0,t)*w(l1,t); };
      double c2=beta[h0]*beta[h1]-beta[l0]*beta[l1];
      std::vector<double> candidates{(lo+up)/2};
      if(c2!=0) {
        double c1=alpha[h0]*beta[h1]+alpha[h1]*beta[h0]-alpha[l0]*beta[l1]-alpha[l1]*beta[l0],t=-c1/(2*c2);
        if(t>lo&&t<up) candidates.push_back(t);
      }
      double best_t=candidates[0],worst_t=candidates[0];
      for(double t:candidates) { if(excess(t)>excess(best_t)) best_t=t; if(excess(t)<excess(worst_t)) worst_t=t; }
      auto end=[&](int j,double t) { bool sign=w(j,t)>=0; return hi[q[j]]==sign?q[j]:(q[j]|bit); };
      if(excess(best_t)>0) inner.unite(end(h0,best_t),end(h1,best_t));
      if(excess(worst_t)<0) inner.unite(end(l0,worst_t),end(l1,worst_t));
    }
  }

  auto vid=[&](int e) { return edge_vertex(mesh,s.slab,grid,c.ids[edges[e][0]],c.ids[edges[e][1]],level); };
  auto position=[&](int e) { return mesh.vertices[vid(e)].p; };
  auto apart=[&](int a,int b) { return !(edge_faces(a)&edge_faces(b)); };
  int nl=loops.size();
  std::vector<int> high_side(nl),low_side(nl),partner(nl,-1);
  for(int l=0;l<nl;++l) {
    int e=loops[l][0],a=edges[e][0],b=edges[e][1];
    high_side[l]=hi[a]?a:b; low_side[l]=hi[a]?b:a;
  }
  for(int pass=0;pass<2;++pass) for(int a=0;a<nl;++a) for(int b=a+1;b<nl;++b) {
    if(partner[a]>=0||partner[b]>=0) continue;
    bool high_tunnel=surface.find(high_side[a])!=surface.find(high_side[b])&&inner.find(high_side[a])==inner.find(high_side[b]);
    bool low_tunnel=surface.find(low_side[a])!=surface.find(low_side[b])&&inner.find(low_side[a])==inner.find(low_side[b]);
    bool shared=high_tunnel?surface.find(low_side[a])==surface.find(low_side[b]):surface.find(high_side[a])==surface.find(high_side[b]);
    if((high_tunnel||low_tunnel)&&(shared||pass==1)) { partner[a]=b; partner[b]=a; }
  }

  for(int l=0;l<nl;++l) {
    if(partner[l]>=0) {
      if(partner[l]<l) continue;
      // Tunnel: a triangle strip between the two loops, the second traversed
      // backwards so both loops keep their orientation as surface boundaries.
      // The strip is a monotone path over rung pairs that never repeats a
      // rung before closing; rungs joining two crossings of one cube face
      // are avoided where possible.
      const auto& p=loops[l]; const auto& r=loops[partner[l]]; int m=p.size(),n=r.size();
      auto wrap=[](int a,int size) { return (a%size+size)%size; };
      double scale=norm(sub(grid.point(c.ids[7]),grid.point(c.ids[0])));
      auto rung=[&](int i,int j) {
        int a=p[wrap(i,m)],b=r[wrap(j,n)]; double d=norm(sub(position(a),position(b)));
        return apart(a,b)?d:d+16*scale;
      };
      int i0=0,j0=0;
      for(int i=0;i<m;++i) for(int j=0;j<n;++j) if(rung(i,j)<rung(i0,j0)) { i0=i; j0=j; }
      std::vector<char> visited(m*n,0),steps; visited[i0*n+j0]=1;
      int di=0,dj=0;
      while(di<m||dj<n) {
        bool last=di+dj+1==m+n;
        int ip=wrap(i0+di+1,m),jp=wrap(j0-dj,n),ir=wrap(i0+di,m),jr=wrap(j0-dj-1,n);
        bool can_p=di<m&&(last||!visited[ip*n+jp]),can_r=dj<n&&(last||!visited[ir*n+jr]);
        if(!can_p&&!can_r) break;
        bool take_p=can_p&&(!can_r||rung(i0+di+1,j0-dj)<=rung(i0+di,j0-dj-1));
        steps.push_back(take_p);
        if(take_p) { ++di; visited[ip*n+jp]=1; } else { ++dj; visited[ir*n+jr]=1; }
      }
      if(di<m||dj<n) {
        steps.clear(); di=dj=0;
        while(di<m||dj<n) { bool take_p=di<m&&(dj==n||(2*di+1)*n<(2*dj+1)*m); steps.push_back(take_p); take_p?++di:++dj; }
      }
      int i=i0,j=j0;
      for(char take_p:steps) {
        if(take_p) { mesh.oriented(vid(p[wrap(i,m)]),vid(p[wrap(i+1,m)]),vid(r[wrap(j,n)]),level); ++i; }
        else { mesh.oriented(vid(p[wrap(i,m)]),vid(r[wrap(j-1,n)]),vid(r[wrap(j,n)]),level); --j; }
      }
      continue;
    }
    // Disk: ears cut along the shortest diagonal that does not join two
    // crossings of one cube face (a neighbouring cell could cut the same
    // diagonal); otherwise a fan around the mean of the loop.
    std::vector<int> loop=loops[l];
    while(loop.size()>3) {
      int k=loop.size(),best=-1; double length=0;
      for(int a=0;a<k;++a) {
        int before=loop[(a+k-1)%k],after=loop[(a+1)%k];
        if(!apart(before,after)) continue;
        double d=norm(sub(position(before),position(after)));
        if(best<0||d<length) { best=a; length=d; }
      }
      if(best<0) break;
      mesh.oriented(vid(loop[(best+k-1)%k]),vid(loop[best]),vid(loop[(best+1)%k]),level);
      loop.erase(loop.begin()+best);
    }
    if(loop.size()==3) { mesh.oriented(vid(loop[0]),vid(loop[1]),vid(loop[2]),level); continue; }
    Vertex centre; centre.p=V{0,0,0}; centre.n=V{0,0,0};
    for(int e:loop) { const Vertex& x=mesh.vertices[vid(e)]; centre.p=add(centre.p,x.p); centre.n=add(centre.n,x.n); }
    centre.p=mul(centre.p,1.0/loop.size()); centre.n=unit(centre.n);
    int id=mesh.add(centre),k=loop.size();
    for(int a=0;a<k;++a) mesh.oriented(vid(loop[a]),vid(loop[(a+1)%k]),id,level);
  }
}
}

using CellFunction=void(*)(Mesh&,LevelState&,const ScalarGrid&,const Cell&,bool);
using Finish=void(*)(Mesh&,const ScalarGrid&);
struct Method { const char* name; CellFunction cell; Finish finish; };

static inline const std::vector<Method>& isosurface_methods() {
  static const std::vector<Method> methods{
    {"tetra",tetra::cell,tetra::regularise},
    {"cubes",cubes::cell,nullptr}
  };
  return methods;
}

// Extracts every level in one sweep over the cells. Each level keeps its own
// two-layer vertex slab; levels are emitted in the order given.
static inline Rcpp::DataFrame extract_isosurface(ScalarGrid& grid,Rcpp::NumericVector levels,bool caps,const std::string& name) {
  const Method* method=nullptr;
  for(const auto& m:isosurface_methods()) if(name==m.name) method=&m;
  if(!method) Rcpp::stop("Unknown isosurface method.");
  for(double level:levels) if(!std::isfinite(level)) Rcpp::stop("Levels must be finite.");
  int nl=levels.size();
  std::vector<Mesh> meshes(nl);
  std::vector<LevelState> states; states.reserve(nl);
  for(int l=0;l<nl;++l) states.emplace_back(levels[l],grid.nx,grid.ny);
  for(int k=0;k<grid.nz-1;++k) {
    Rcpp::checkUserInterrupt();
    for(auto& s:states) s.slab.begin_layer(k);
    for(int j=0;j<grid.ny-1;++j) for(int i=0;i<grid.nx-1;++i) {
      Cell c{i,j,k,{}}; bool valid=true;
      for(int a=0;a<8;++a) { c.ids[a]=grid.corner(i,j,k,a); valid=valid&&grid.finite(c.ids[a]); }
      if(!valid) continue;
      for(int l=0;l<nl;++l) method->cell(meshes[l],states[l],grid,c,caps);
    }
  }
  Mesh mesh;
  for(int l=0;l<nl;++l) {
    if(method->finish) method->finish(meshes[l],grid);
    int offset=mesh.vertices.size();
    mesh.vertices.insert(mesh.vertices.end(),meshes[l].vertices.begin(),meshes[l].vertices.end());
    for(auto t:meshes[l].triangles) { if(t[0]>=0) for(int& x:t) x+=offset; mesh.triangles.push_back(t); }
    mesh.levels.insert(mesh.levels.end(),meshes[l].levels.begin(),meshes[l].levels.end());
    mesh.caps.insert(mesh.caps.end(),meshes[l].caps.begin(),meshes[l].caps.end());
    mesh.cap_normals.insert(mesh.cap_normals.end(),meshes[l].cap_normals.begin(),meshes[l].cap_normals.end());
  }
  return mesh.output();
}
}
#endif
