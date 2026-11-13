#ifndef GGPLOT3D_GEOMETRY_PREDICATES_H
#define GGPLOT3D_GEOMETRY_PREDICATES_H
#include <vector>
#include <cmath>
#include <cstddef>
namespace ggplot3d_geometry {
namespace predicates {

// Geometric predicates with exact signs (Shewchuk 1997). Each predicate is
// evaluated in floating point first and returned when its magnitude exceeds
// Shewchuk's static error bound for that expression; otherwise the sign is
// computed exactly in floating-point expansion arithmetic: a value is held
// as a sum of non-overlapping doubles in increasing magnitude, built from
// error-free sums (Knuth's two-sum) and products (Dekker's split), so the
// sign is that of the largest component. Inputs must be finite and the
// products must not overflow or underflow.

using Expansion=std::vector<double>;
static constexpr double epsilon=1.1102230246251565e-16;
static constexpr double splitter=134217729.0;

static inline void two_sum(double a,double b,double& x,double& y) {
  x=a+b; double bv=x-a,av=x-bv; y=(a-av)+(b-bv);
}
static inline void split(double a,double& hi,double& lo) {
  double c=splitter*a,big=c-a; hi=c-big; lo=a-hi;
}
static inline void two_product(double a,double b,double& x,double& y) {
  x=a*b; double ah,al,bh,bl; split(a,ah,al); split(b,bh,bl);
  y=al*bl-(((x-ah*bh)-al*bh)-ah*bl);
}
static inline Expansion grow(const Expansion& e,double b) {
  Expansion h; double q=b;
  for(double x:e) { double s,r; two_sum(q,x,s,r); if(r!=0) h.push_back(r); q=s; }
  if(q!=0||h.empty()) h.push_back(q);
  return h;
}
static inline Expansion sum(const Expansion& e,const Expansion& f) {
  Expansion h=e;
  for(double x:f) h=grow(h,x);
  return h;
}
static inline Expansion negate(Expansion e) { for(double& x:e) x=-x; return e; }
static inline Expansion difference(const Expansion& e,const Expansion& f) { return sum(e,negate(f)); }
static inline Expansion scale(const Expansion& e,double b) {
  Expansion h; double q=0; bool first=true;
  for(double x:e) {
    double p,t; two_product(x,b,p,t);
    if(first) { if(t!=0) h.push_back(t); q=p; first=false; continue; }
    double s,r; two_sum(q,t,s,r); if(r!=0) h.push_back(r);
    double u,w; two_sum(p,s,u,w); if(w!=0) h.push_back(w); q=u;
  }
  if(q!=0||h.empty()) h.push_back(q);
  return h;
}
static inline Expansion product(const Expansion& e,const Expansion& f) {
  Expansion h{0};
  for(double x:f) h=sum(h,scale(e,x));
  return h;
}
static inline Expansion exact_difference(double a,double b) { double x,y; two_sum(a,-b,x,y); return y!=0?Expansion{y,x}:Expansion{x}; }
static inline int sign(const Expansion& e) {
  for(size_t i=e.size();i-->0;) if(e[i]!=0) return e[i]>0?1:-1;
  return 0;
}
// a d - b c for expansions.
static inline Expansion minor2(const Expansion& a,const Expansion& b,const Expansion& c,const Expansion& d) {
  return difference(product(a,d),product(b,c));
}

// Sign of the determinant |a-c, b-c|: positive when a, b, c run counterclockwise.
static inline int orient2d(const double* a,const double* b,const double* c) {
  double left=(a[0]-c[0])*(b[1]-c[1]),right=(a[1]-c[1])*(b[0]-c[0]),det=left-right;
  double bound=(3+16*epsilon)*epsilon*(std::abs(left)+std::abs(right));
  if(det>bound||-det>bound) return det>0?1:-1;
  Expansion ax=exact_difference(a[0],c[0]),ay=exact_difference(a[1],c[1]),bx=exact_difference(b[0],c[0]),by=exact_difference(b[1],c[1]);
  return sign(minor2(ax,ay,bx,by));
}

// Positive when d lies inside the circle through the counterclockwise a, b, c.
static inline int incircle(const double* a,const double* b,const double* c,const double* d) {
  double adx=a[0]-d[0],ady=a[1]-d[1],bdx=b[0]-d[0],bdy=b[1]-d[1],cdx=c[0]-d[0],cdy=c[1]-d[1];
  double bc=bdx*cdy,cb=cdx*bdy,ca=cdx*ady,ac=adx*cdy,ab=adx*bdy,ba=bdx*ady;
  double al=adx*adx+ady*ady,bl=bdx*bdx+bdy*bdy,cl=cdx*cdx+cdy*cdy;
  double det=al*(bc-cb)+bl*(ca-ac)+cl*(ab-ba);
  double permanent=(std::abs(bc)+std::abs(cb))*al+(std::abs(ca)+std::abs(ac))*bl+(std::abs(ab)+std::abs(ba))*cl;
  double bound=(10+96*epsilon)*epsilon*permanent;
  if(det>bound||-det>bound) return det>0?1:-1;
  Expansion ax=exact_difference(a[0],d[0]),ay=exact_difference(a[1],d[1]),bx=exact_difference(b[0],d[0]),
            by=exact_difference(b[1],d[1]),cx=exact_difference(c[0],d[0]),cy=exact_difference(c[1],d[1]);
  auto lift=[](const Expansion& x,const Expansion& y) { return sum(product(x,x),product(y,y)); };
  Expansion det_e=sum(sum(product(lift(ax,ay),minor2(bx,by,cx,cy)),product(lift(bx,by),minor2(cx,cy,ax,ay))),product(lift(cx,cy),minor2(ax,ay,bx,by)));
  return sign(det_e);
}

// Sign of the determinant |a-d, b-d, c-d|: positive when d lies below the
// plane through a, b, c, which appear counterclockwise seen from above.
static inline int orient3d(const double* a,const double* b,const double* c,const double* d) {
  double adx=a[0]-d[0],ady=a[1]-d[1],adz=a[2]-d[2],bdx=b[0]-d[0],bdy=b[1]-d[1],bdz=b[2]-d[2],cdx=c[0]-d[0],cdy=c[1]-d[1],cdz=c[2]-d[2];
  double bc=bdx*cdy,cb=cdx*bdy,ca=cdx*ady,ac=adx*cdy,ab=adx*bdy,ba=bdx*ady;
  double det=adz*(bc-cb)+bdz*(ca-ac)+cdz*(ab-ba);
  double permanent=(std::abs(bc)+std::abs(cb))*std::abs(adz)+(std::abs(ca)+std::abs(ac))*std::abs(bdz)+(std::abs(ab)+std::abs(ba))*std::abs(cdz);
  double bound=(7+56*epsilon)*epsilon*permanent;
  if(det>bound||-det>bound) return det>0?1:-1;
  Expansion e[3][3];
  const double* p[3]={a,b,c};
  for(int i=0;i<3;++i) for(int k=0;k<3;++k) e[i][k]=exact_difference(p[i][k],d[k]);
  Expansion det_e=sum(sum(product(e[0][2],minor2(e[1][0],e[1][1],e[2][0],e[2][1])),product(e[1][2],minor2(e[2][0],e[2][1],e[0][0],e[0][1]))),
                      product(e[2][2],minor2(e[0][0],e[0][1],e[1][0],e[1][1])));
  return sign(det_e);
}

// Positive when e lies inside the sphere through a, b, c, d, which must be
// positively oriented (orient3d(a, b, c, d) > 0).
static inline int insphere(const double* a,const double* b,const double* c,const double* d,const double* e) {
  double r[4][3]; const double* p[4]={a,b,c,d};
  for(int i=0;i<4;++i) for(int k=0;k<3;++k) r[i][k]=p[i][k]-e[k];
  auto m2=[&](int i,int j) { return r[i][0]*r[j][1]-r[j][0]*r[i][1]; };
  auto m2abs=[&](int i,int j) { return std::abs(r[i][0]*r[j][1])+std::abs(r[j][0]*r[i][1]); };
  // 3x3 minors of rows i, j, k over columns x, y, z, and their permanents.
  auto m3=[&](int i,int j,int k) { return r[i][2]*m2(j,k)-r[j][2]*m2(i,k)+r[k][2]*m2(i,j); };
  auto m3abs=[&](int i,int j,int k) { return std::abs(r[i][2])*m2abs(j,k)+std::abs(r[j][2])*m2abs(i,k)+std::abs(r[k][2])*m2abs(i,j); };
  double lift[4]; for(int i=0;i<4;++i) lift[i]=r[i][0]*r[i][0]+r[i][1]*r[i][1]+r[i][2]*r[i][2];
  double det=-lift[0]*m3(1,2,3)+lift[1]*m3(0,2,3)-lift[2]*m3(0,1,3)+lift[3]*m3(0,1,2);
  double permanent=lift[0]*m3abs(1,2,3)+lift[1]*m3abs(0,2,3)+lift[2]*m3abs(0,1,3)+lift[3]*m3abs(0,1,2);
  double bound=(16+224*epsilon)*epsilon*permanent;
  if(det>bound||-det>bound) return det>0?1:-1;
  Expansion x[4][3];
  for(int i=0;i<4;++i) for(int k=0;k<3;++k) x[i][k]=exact_difference(p[i][k],e[k]);
  auto em2=[&](int i,int j) { return minor2(x[i][0],x[i][1],x[j][0],x[j][1]); };
  auto em3=[&](int i,int j,int k) { return sum(difference(product(x[i][2],em2(j,k)),product(x[j][2],em2(i,k))),product(x[k][2],em2(i,j))); };
  Expansion el[4];
  for(int i=0;i<4;++i) el[i]=sum(sum(product(x[i][0],x[i][0]),product(x[i][1],x[i][1])),product(x[i][2],x[i][2]));
  Expansion det_e=sum(difference(product(el[1],em3(0,2,3)),product(el[0],em3(1,2,3))),
                      difference(product(el[3],em3(0,1,2)),product(el[2],em3(0,1,3))));
  return sign(det_e);
}
}
}
#endif
