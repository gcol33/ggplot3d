#ifndef GGPLOT3D_GEOMETRY_STREAMLINE_H
#define GGPLOT3D_GEOMETRY_STREAMLINE_H
#include "vector.h"
#include <tuple>
namespace ggplot3d_geometry { namespace flow {

struct Field {
  std::array<std::vector<double>,3> axis;
  std::vector<V> value;
  int slice;
  bool sample(V p,V& v) const {
    std::array<int,3> i; V t;
    for(int a=0;a<3;++a) {
      const auto& x=axis[a];
      if(!std::isfinite(p[a])||p[a]<x.front()||p[a]>x.back()) return false;
      i[a]=std::min<int>(std::upper_bound(x.begin(),x.end(),p[a])-x.begin()-1,x.size()-2);
      t[a]=(p[a]-x[i[a]])/(x[i[a]+1]-x[i[a]]);
    }
    v={0,0,0};
    for(int bits=0;bits<8;++bits) {
      int x=i[0]+(bits&1),y=i[1]+((bits>>1)&1),z=i[2]+((bits>>2)&1);
      V q=value[x+axis[0].size()*(y+axis[1].size()*z)];
      double w=1;
      for(int a=0;a<3;++a) w*=((bits>>a)&1)?t[a]:1-t[a];
      if(w==0) continue;
      for(int a=0;a<3;++a) { if(!std::isfinite(q[a])) return false; v[a]+=w*q[a]; }
    }
    if(slice>=0) v[slice]=0;
    return true;
  }
};

struct Tableau { int n; double a[7][7]{},b[7]{},e[7]{}; };
static inline Tableau tableau(bool adaptive) {
  Tableau t{};
  if(!adaptive) {
    t.n=4;t.a[1][0]=.5;t.a[2][1]=.5;t.a[3][2]=1;
    t.b[0]=t.b[3]=1./6;t.b[1]=t.b[2]=1./3;
    return t;
  }
  // Dormand-Prince 5(4), with the embedded error including the endpoint stage.
  t.n=7;
  t.a[1][0]=1./5;
  t.a[2][0]=3./40;t.a[2][1]=9./40;
  t.a[3][0]=44./45;t.a[3][1]=-56./15;t.a[3][2]=32./9;
  t.a[4][0]=19372./6561;t.a[4][1]=-25360./2187;t.a[4][2]=64448./6561;t.a[4][3]=-212./729;
  t.a[5][0]=9017./3168;t.a[5][1]=-355./33;t.a[5][2]=46732./5247;t.a[5][3]=49./176;t.a[5][4]=-5103./18656;
  const double b[]={35./384,0,500./1113,125./192,-2187./6784,11./84,0};
  const double e[]={-71./57600,0,71./16695,-71./1920,17253./339200,-22./525,1./40};
  for(int i=0;i<7;++i) { t.b[i]=b[i];t.e[i]=e[i];t.a[6][i]=b[i]; }
  return t;
}

static inline bool advance(const Field& f,const Tableau& t,V p,double h,V& next,V& error) {
  V k[7];
  for(int i=0;i<t.n;++i) {
    V q=p;
    for(int j=0;j<i;++j) q=add(q,mul(k[j],h*t.a[i][j]));
    if(!f.sample(q,k[i])) return false;
  }
  next=p;error={0,0,0};
  for(int i=0;i<t.n;++i) { next=add(next,mul(k[i],h*t.b[i]));error=add(error,mul(k[i],h*t.e[i])); }
  V v;return f.sample(next,v);
}

// Cartesian bins of accepted samples; the spacing bound during tracing keeps
// the samples closer than the distance used by the separation test.
struct Proximity {
  double width;
  std::map<std::tuple<long long,long long,long long>,std::vector<V>> bins;
  std::array<long long,3> cell(V p) const {
    return {static_cast<long long>(std::floor(p[0]/width)),static_cast<long long>(std::floor(p[1]/width)),static_cast<long long>(std::floor(p[2]/width))};
  }
  bool close(V p,double radius) const {
    if(!(width>0)) return false;
    auto c=cell(p);
    for(int x=-1;x<=1;++x) for(int y=-1;y<=1;++y) for(int z=-1;z<=1;++z) {
      auto it=bins.find({c[0]+x,c[1]+y,c[2]+z});
      if(it!=bins.end()) for(V q:it->second) if(norm(sub(p,q))<radius) return true;
    }
    return false;
  }
  void insert(V p) { auto c=cell(p);bins[{c[0],c[1],c[2]}].push_back(p); }
};

struct Sample { V p;double time,speed; };
using Line=std::vector<Sample>;
static inline Line trace(const Field& field,V seed,double step,int max_steps,bool adaptive,
                         int sign,double tolerance,const Proximity& existing,double dtest) {
  V v;if(!field.sample(seed,v)||!(norm(v)>0)) return {};
  Line out{{seed,0,norm(v)}};Tableau method=tableau(adaptive);
  double h=step,time=0,arc=0;
  while(static_cast<int>(out.size())<=max_steps) {
    if((out.size()&255)==0) Rcpp::checkUserInterrupt();
    V p=out.back().p;field.sample(p,v);double speed=norm(v);
    if(!(speed>0)) break;
    if(dtest>0) h=std::min(h,dtest/(2*speed));
    double minimum=32*std::numeric_limits<double>::epsilon()*std::max(1.,norm(p))/speed;
    if(h<=minimum) break;
    V next,error;
    if(!advance(field,method,p,sign*h,next,error)) { h*=.5;continue; }
    double distance=norm(sub(next,p));
    if(dtest>0&&distance>dtest/2) { h*=.5;continue; }
    double relative=0;
    for(int a=0;a<3;++a) relative=std::max(relative,std::abs(error[a])/(tolerance*(field.axis[a].back()-field.axis[a].front())));
    if(adaptive&&relative>1) { h*=std::max(.1,.9*std::pow(relative,-.2));continue; }
    if(distance<=32*std::numeric_limits<double>::epsilon()*std::max(1.,norm(p))) break;
    if(dtest>0&&existing.close(next,dtest)) break;
    arc+=distance;time+=sign*h;
    field.sample(next,v);
    out.push_back({next,time,norm(v)});
    if(arc>8*distance&&norm(sub(next,seed))<distance*.75) break;
    if(adaptive) h=std::min(step,h*(relative>0?std::min(5.,.9*std::pow(relative,-.2)):5.));
  }
  return out;
}

static inline std::vector<Line> integrate(const Field& f,std::vector<V> seeds,double step,int max_steps,
                                         bool adaptive,int direction,double tolerance,double separation,
                                         double dtest,int max_lines) {
  Proximity accepted{separation,{}};std::vector<Line> lines;
  for(size_t at=0;at<seeds.size()&&static_cast<int>(lines.size())<max_lines;++at) {
    V seed=seeds[at],velocity;
    if(!f.sample(seed,velocity)||accepted.close(seed,separation)) continue;
    Line forward,backward;
    if(direction>=0) forward=trace(f,seed,step,max_steps,adaptive,1,tolerance,accepted,dtest);
    if(direction<=0) backward=trace(f,seed,step,max_steps,adaptive,-1,tolerance,accepted,dtest);
    Line line;
    if(!backward.empty()) { std::reverse(backward.begin(),backward.end());line=std::move(backward); }
    if(!forward.empty()) line.insert(line.end(),forward.begin()+(!line.empty()),forward.end());
    if(line.size()<2) continue;
    lines.push_back(line);
    if(separation>0) {
      for(auto sample:line) accepted.insert(sample.p);
      // Jobard-Lefer candidate queue in a slice: candidates lie on either
      // side of the projected tangent at the requested separation.
      V normal{0,0,0};normal[f.slice]=1;
      for(auto sample:line) {
        f.sample(sample.p,velocity);V offset=mul(unit(cross(normal,velocity)),separation);
        seeds.push_back(add(sample.p,offset));seeds.push_back(sub(sample.p,offset));
      }
    }
  }
  return lines;
}
}}
#endif
