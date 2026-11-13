#ifndef GGPLOT3D_GEOMETRY_HEAT_H
#define GGPLOT3D_GEOMETRY_HEAT_H
#include "processing.h"
#include "sparse.h"
namespace ggplot3d_geometry {
namespace halfedge {

using sparse::Complex;

// Largest Cholesky factor the solvers may allocate.
constexpr double factor_budget_bytes=2.0*1024*1024*1024;

static inline bool live_vertex(const Mesh& m,int v) { return !m.vdead[v]&&m.out[v]>=0; }

// Connected component of every vertex through live edges, -1 for vertices
// without faces.
static inline std::vector<int> components(const Mesh& m,int* count) {
  int n=m.p.size(); UnionFind u(n);
  for(int e=0;e<m.edges();++e) if(!m.edead[e]) u.unite(m.to[2*e],m.to[2*e+1]);
  std::vector<int> id(n,-1),root(n,-1); *count=0;
  for(int v=0;v<n;++v) if(live_vertex(m,v)) { int r=u.find(v); if(root[r]<0) root[r]=(*count)++; id[v]=root[r]; }
  return id;
}

// Unknown index of every vertex: live vertices not excluded, in order.
static inline std::vector<int> unknowns(const Mesh& m,const std::vector<char>& excluded,int* count) {
  std::vector<int> index(m.p.size(),-1); *count=0;
  for(size_t v=0;v<m.p.size();++v) if(live_vertex(m,v)&&!excluded[v]) index[v]=(*count)++;
  return index;
}

// Factor of alpha M + beta K over the unknowns, where M is the diagonal of
// mass and K the cotangent stiffness matrix, positive semidefinite, whose
// entry for half-edge h, from(h) to to(h), is -w(h) times transport(h)
// (one for scalar functions). Excluded vertices are held at zero.
template<class T,class Transport>
static inline sparse::Cholesky<T> stiffness_factor(const Mesh& m,const std::vector<int>& index,int count,
    const std::vector<double>& mass,double alpha,const std::vector<double>& w,double beta,Transport transport) {
  sparse::Matrix<T> A(count);
  for(size_t v=0;v<m.p.size();++v) if(index[v]>=0) { A.diag[index[v]]+=alpha*mass[v]; A.at[index[v]]=m.p[v]; }
  for(int e=0;e<m.edges();++e) {
    if(m.edead[e]) continue;
    int h=2*e,a=m.from(h),b=m.to[h];
    if(index[a]>=0) A.diag[index[a]]+=beta*w[e];
    if(index[b]>=0) A.diag[index[b]]+=beta*w[e];
    if(index[a]>=0&&index[b]>=0) A.add(index[a],index[b],-beta*w[e]*transport(h));
  }
  return sparse::Cholesky<T>(A,factor_budget_bytes);
}

template<class T>
static inline std::vector<T> solve_on(const sparse::Cholesky<T>& L,const std::vector<int>& index,const std::vector<T>& b) {
  std::vector<T> rhs(L.n);
  for(size_t v=0;v<index.size();++v) if(index[v]>=0) rhs[index[v]]=b[v];
  std::vector<T> y=L.solve(rhs),out(index.size(),T(0));
  for(size_t v=0;v<index.size();++v) if(index[v]>=0) out[v]=y[index[v]];
  return out;
}

// Solution of K x = b on each component with one vertex held at zero, for
// right-hand sides that sum to zero on each component.
static inline std::vector<double> poisson(const Mesh& m,const std::vector<double>& w,const std::vector<double>& b) {
  int count=0; std::vector<int> comp=components(m,&count);
  std::vector<char> pinned(m.p.size(),0),seen(count,0);
  for(size_t v=0;v<m.p.size();++v) if(comp[v]>=0&&!seen[comp[v]]) { seen[comp[v]]=1; pinned[v]=1; }
  int n=0; std::vector<int> index=unknowns(m,pinned,&n);
  if(!n) return std::vector<double>(m.p.size(),0);
  auto L=stiffness_factor<double>(m,index,n,std::vector<double>(m.p.size(),0),0,w,1,[](int) { return 1.0; });
  return solve_on(L,index,b);
}

// A point on the mesh: the corners of the closest live face and the
// barycentric weights of the closest point in it.
struct SurfacePoint { Tri t; double w[3]; };

static inline std::vector<SurfacePoint> locate(const Mesh& m,const Rcpp::NumericMatrix& points) {
  std::vector<Tri> tri; for(size_t f=0;f<m.fh.size();++f) if(!m.fdead[f]) tri.push_back(m.corners(f));
  std::vector<SurfacePoint> out;
  if(tri.empty()) return out;
  TriangleGrid grid(m.p,tri);
  for(int i=0;i<points.nrow();++i) {
    auto hit=grid.closest(V{points(i,0),points(i,1),points(i,2)});
    SurfacePoint s; s.t=tri[hit.t]; for(int c=0;c<3;++c) s.w[c]=hit.w[c]; out.push_back(s);
  }
  return out;
}

// Geodesic distance by the heat method (Crane, Weischedel and Wardetzky
// 2013): heat diffuses from the sources for time t = time * h^2 (h the mean
// edge length) by one backward Euler step (M + t K) u = u0, with M the mixed
// Voronoi areas and K the cotangent stiffness matrix; the normalised field
// X = -grad u / |grad u| on each face is integrated against the hat
// functions, and K phi = div X recovers the distance up to a constant,
// which is fixed by the mean of phi over the sources of each component.
// Sources are points on the surface whose unit heat is spread over the
// corners of their face by barycentric weight. With a boundary, heat is
// held at zero there ("dirichlet"), reflected ("neumann") or the mean of
// both ("average", as the paper recommends). Vertices of components without
// a source have no value.
static inline Result geodesic_distance(const Soup& in,const Rcpp::List& args) {
  Mesh m=build(in); int n=m.p.size();
  Rcpp::NumericMatrix points=args["source"];
  double time=Rcpp::as<double>(args["time"]); std::string boundary=Rcpp::as<std::string>(args["boundary"]);
  std::vector<double> w=cotan_weights(m),area=mixed_areas(m);
  double h=mean_edge_length(m),t=time*h*h;
  std::vector<SurfacePoint> sources=locate(m,points);
  if(sources.empty()) {
    Result r; r.soup=m.soup(); r.vertex_names={"distance"};
    r.vertex.assign(r.soup.p.size(),std::numeric_limits<double>::quiet_NaN());
    return r;
  }
  std::vector<double> u0(n,0);
  for(const auto& s:sources) for(int c=0;c<3;++c) u0[s.t[c]]+=s.w[c];
  std::vector<char> none(n,0),rim(n,0); bool has_rim=false;
  for(int v=0;v<n;++v) if(live_vertex(m,v)&&m.vb(v)) { rim[v]=1; has_rim=true; }
  auto heat=[&](const std::vector<char>& excluded) {
    int count=0; std::vector<int> index=unknowns(m,excluded,&count);
    if(!count) return std::vector<double>(n,0);
    auto L=stiffness_factor<double>(m,index,count,area,1,w,t,[](int) { return 1.0; });
    return solve_on(L,index,u0);
  };
  std::vector<double> u;
  if(!has_rim||boundary=="neumann") u=heat(none);
  else if(boundary=="dirichlet") u=heat(rim);
  else { u=heat(none); std::vector<double> d=heat(rim); for(int v=0;v<n;++v) u[v]=(u[v]+d[v])/2; }

  std::vector<double> b(n,0);
  for(size_t f=0;f<m.fh.size();++f) {
    if(m.fdead[f]) continue;
    FaceGeometry g=face_geometry(m,f); if(!(g.twice>0)) continue;
    V N=mul(g.cr,1/g.twice),grad{0,0,0},side[3];
    for(int i=0;i<3;++i) { side[i]=cross(N,sub(g.x[(i+2)%3],g.x[(i+1)%3])); grad=add(grad,mul(side[i],u[g.t[i]])); }
    double size=norm(grad); if(!(size>0)) continue;
    V X=mul(grad,-1/size);
    for(int i=0;i<3;++i) b[g.t[i]]+=dot(X,side[i])/2;
  }
  std::vector<double> phi=poisson(m,w,b);

  int count=0; std::vector<int> comp=components(m,&count);
  std::vector<double> shift(count,0); std::vector<int> hits(count,0);
  for(const auto& s:sources) {
    int k=comp[s.t[0]]; double value=0;
    for(int c=0;c<3;++c) value+=s.w[c]*phi[s.t[c]];
    shift[k]+=value; ++hits[k];
  }
  std::vector<int> vertex_of; Result r; r.soup=m.soup(&vertex_of,nullptr);
  r.vertex_names={"distance"};
  double na=std::numeric_limits<double>::quiet_NaN();
  for(int v:vertex_of) { int k=comp[v]; r.vertex.push_back(k>=0&&hits[k]?phi[v]-shift[k]/hits[k]:na); }
  r.info={{"time",t}};
  return r;
}

// Polar angle at vertex v of the tangent direction d, measured in the
// rescaled polar coordinates of polar_angles(): the direction projected
// into the plane of the vertex normal is placed between the projections of
// two consecutive outgoing edges and the angle interpolated between theirs.
static inline double direction_angle(const Mesh& m,const std::vector<double>& phi,int v,V d) {
  V n=vertex_normal(m,v); V dt=sub(d,mul(n,dot(d,n)));
  if(!(norm(dt)>0)) return 0;
  std::vector<int> hs; m.around(v,[&](int h) { hs.push_back(h); });
  V ref=sub(m.p[m.to[hs[0]]],m.p[v]); ref=unit(sub(ref,mul(n,dot(ref,n)))); V up=cross(n,ref);
  auto at=[&](V x) { double a=std::atan2(dot(x,up),dot(x,ref)); return a<0?a+2*pi:a; };
  std::vector<double> geo; for(int h:hs) geo.push_back(at(sub(m.p[m.to[h]],m.p[v])));
  double target=at(dt);
  // Outgoing edges run clockwise, so their counterclockwise angles fall.
  for(size_t k=0;k<hs.size();++k) {
    size_t j=(k+1)%hs.size();
    if(m.fc[hs[k]^1]<0) continue;
    double span=std::fmod(geo[k]-geo[j]+4*pi,2*pi),off=std::fmod(geo[k]-target+4*pi,2*pi);
    if(off<=span) {
      double gap=std::fmod(phi[hs[k]]-phi[hs[j]]+4*pi,2*pi);
      return std::fmod(phi[hs[k]]-(span>0?off/span:0)*gap+4*pi,2*pi);
    }
  }
  return phi[hs[0]];
}

// Logarithmic map by the vector heat method (Sharp, Soliman and Crane
// 2019, section 8.2 and appendix A). Tangent vectors are complex numbers in
// the rescaled polar coordinates of each vertex; the connection Laplacian
// carries the cotangent weights times the rotation between the coordinates
// of the two ends of each edge. One factor of M + t K (t = h^2) diffuses
// the unit vector at the source along `direction` (the horizontal field H)
// and the finite-element projection of unit outward normals on a small
// circle around the source (the radial field R); both are normalised. The
// angle is arg R - arg H, and the radius solves K r = -div R with the edge
// values of R from the Whitney average and r = 0 at the source. The source
// snaps to the vertex closest to the given point.
static inline Result logmap(const Soup& in,const Rcpp::List& args) {
  Mesh m=build(in); int n=m.p.size();
  Rcpp::NumericVector point=args["source"],dir=args["direction"]; double time=Rcpp::as<double>(args["time"]);
  if(point.size()!=3) {
    Result r; r.soup=m.soup(); r.vertex_names={"logmap_u","logmap_v"};
    r.vertex.assign(2*r.soup.p.size(),std::numeric_limits<double>::quiet_NaN());
    return r;
  }
  int src=-1; double best=std::numeric_limits<double>::infinity();
  for(int v=0;v<n;++v) if(live_vertex(m,v)) {
    V d=sub(m.p[v],V{point[0],point[1],point[2]}); if(dot(d,d)<best) { best=dot(d,d); src=v; }
  }
  if(src<0) Rcpp::stop("The mesh has no faces.");
  std::vector<double> w=cotan_weights(m),area=mixed_areas(m),phi=polar_angles(m);
  double h=mean_edge_length(m),t=time*h*h;
  int count=0; std::vector<int> index=unknowns(m,std::vector<char>(n,0),&count);
  auto L=stiffness_factor<Complex>(m,index,count,area,1,w,t,[&](int g) { return std::polar(1.0,phi[g]+pi-phi[g^1]); });

  double start=dir.size()==3?direction_angle(m,phi,src,V{dir[0],dir[1],dir[2]}):0;
  std::vector<Complex> y0(n,Complex(0)); y0[src]=std::polar(1.0,start);
  std::vector<Complex> H=solve_on(L,index,y0);

  auto length=[&](int g) { return norm(sub(m.p[m.to[g]],m.p[m.from(g)])); };
  auto face_area=[&](int f) { return norm(m.normal(f))/2; };
  std::vector<Complex> x(n,Complex(0));
  m.around(src,[&](int g) {
    Complex xt(0);
    if(m.fc[g]>=0) {
      double a=tail_angle(m,g),lik=length(m.pv[g]),A=face_area(m.fc[g]);
      xt+=Complex(a*std::sin(a),std::sin(a)-a*std::cos(a))*(lik/(4*A));
      double lij=length(g);
      x[src]+=std::polar(1.0,phi[g])*Complex(-std::sin(a)*(lik*a+lij*std::sin(a)),lij*(std::cos(a)*std::sin(a)-a)+lik*(a*std::cos(a)-std::sin(a)))/(4*A);
    }
    if(m.fc[g^1]>=0) {
      int o=m.nx[g^1]; double b=tail_angle(m,o),lil=length(o),A=face_area(m.fc[g^1]);
      xt+=Complex(b*std::sin(b),b*std::cos(b)-std::sin(b))*(lil/(4*A));
    }
    x[m.to[g]]+=-std::polar(1.0,phi[g^1])*xt;
  });
  std::vector<Complex> R=solve_on(L,index,x);
  for(auto* field:{&H,&R}) for(auto& z:*field) { double s=std::abs(z); z=s>0?z/s:Complex(0); }
  R[src]=Complex(0);

  std::vector<double> b(n,0);
  for(int e=0;e<m.edges();++e) {
    if(m.edead[e]) continue;
    int g=2*e,i=m.from(g),j=m.to[g]; double l=length(g);
    double rij=(std::real(std::polar(l,phi[g])*std::conj(R[i]))-std::real(std::polar(l,phi[g^1])*std::conj(R[j])))/2;
    b[i]-=w[e]*rij; b[j]+=w[e]*rij;
  }
  std::vector<double> radius=poisson(m,w,b);
  int ncomp=0; std::vector<int> comp=components(m,&ncomp);
  std::vector<int> vertex_of; Result r; r.soup=m.soup(&vertex_of,nullptr);
  r.vertex_names={"logmap_u","logmap_v"};
  double na=std::numeric_limits<double>::quiet_NaN();
  for(int v:vertex_of) {
    if(comp[v]!=comp[src]) { r.vertex.push_back(na); r.vertex.push_back(na); continue; }
    double rv=radius[v]-radius[src],a=v==src?0:std::arg(R[v])-std::arg(H[v]);
    r.vertex.push_back(rv*std::cos(a)); r.vertex.push_back(rv*std::sin(a));
  }
  r.info={{"time",t}};
  return r;
}
}
}
#endif
