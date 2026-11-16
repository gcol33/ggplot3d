#pragma once
#include "kernel.h"
#include <cstdint>

namespace ggplot3d { namespace raymarch {
inline std::uint64_t mix_bits(std::uint64_t x) {
  x^=x>>30;x*=0xbf58476d1ce4e5b9ULL;x^=x>>27;x*=0x94d049bb133111ebULL;return x^(x>>31);
}
inline std::uint32_t reverse_bits(std::uint32_t x) {
  x=(x>>16)|(x<<16);x=((x&0x00ff00ffu)<<8)|((x&0xff00ff00u)>>8);
  x=((x&0x0f0f0f0fu)<<4)|((x&0xf0f0f0f0u)>>4);
  x=((x&0x33333333u)<<2)|((x&0xccccccccu)>>2);return ((x&0x55555555u)<<1)|((x&0xaaaaaaaau)>>1);
}
inline std::uint32_t owen(std::uint32_t x,std::uint32_t seed) {
  x=reverse_bits(x);x+=seed;x^=x*0x6c50b47cu;x^=x*0xb82f1e52u;x^=x*0xc7afe638u;x^=x*0x8d22f6e6u;return reverse_bits(x);
}
inline const std::array<std::array<std::uint32_t,32>,4>& sobol_directions() {
  static const auto table=[](){std::array<std::array<std::uint32_t,32>,4> v{};
    for(int j=0;j<32;++j)v[0][j]=1u<<(31-j);
    int degree[3]={1,2,3},polynomial[3]={0,1,1};int initial[3][3]={{1,0,0},{1,3,0},{1,3,1}};
    for(int d=1;d<4;++d){int s=degree[d-1];for(int j=0;j<s;++j)v[d][j]=std::uint32_t(initial[d-1][j])<<(31-j);
      for(int j=s;j<32;++j){v[d][j]=v[d][j-s]^(v[d][j-s]>>s);
        for(int k=1;k<s;++k)if((polynomial[d-1]>>(s-1-k))&1)v[d][j]^=v[d][j-k];}}
    return v;}();return table;
}
struct Sampler {
  std::uint64_t key,dimension=0,scope=0;std::uint32_t sample;
  Sampler(std::uint64_t seed,std::uint64_t pixel,std::uint64_t index):
    key(mix_bits(seed)^mix_bits(pixel+1)^mix_bits(index%2+0x8917)),sample(std::uint32_t(index/2)){}
  void stream(std::uint64_t bounce,std::uint64_t component){scope=mix_bits(bounce+1)^mix_bits(component+0x734912);dimension=0;}
  double next() {
    std::uint64_t d=dimension++,group=d/4;std::uint32_t index=owen(sample,std::uint32_t(mix_bits(key^scope^group))),value=0;
    const auto& directions=sobol_directions()[d%4];for(int bit=0;index;++bit,index>>=1)if(index&1)value^=directions[bit];
    return (double(owen(value,std::uint32_t(mix_bits(key^scope^mix_bits(d+1)))))+.5)*0x1.0p-32;
  }
};
inline Vec3 scaled(const Vec3& a,double s){return {a[0]*s,a[1]*s,a[2]*s};}
inline Vec3 product(const Vec3& a,const Vec3& b){return {a[0]*b[0],a[1]*b[1],a[2]*b[2]};}
inline Vec3 plus(const Vec3& a,const Vec3& b){return {a[0]+b[0],a[1]+b[1],a[2]+b[2]};}
inline double luminance(const Vec3& a){return .2126*a[0]+.7152*a[1]+.0722*a[2];}
inline double power_weight(double a,double b){if(a==0)return 0;double r=b/a;return 1/(1+r*r);}
struct Frame {
  Vec3 x,y,z;
  explicit Frame(const Vec3& n):z(n){x=unit(engine::cross(std::abs(n[2])<.9 ? Vec3{0,0,1}:Vec3{1,0,0},n));y=engine::cross(n,x);}
  Vec3 world(const Vec3& v)const{return plus(plus(scaled(x,v[0]),scaled(y,v[1])),scaled(z,v[2]));}
  Vec3 local(const Vec3& v)const{return {engine::dot(v,x),engine::dot(v,y),engine::dot(v,z)};}
};
inline Vec3 cosine_sample(const Vec3& normal,Sampler& rng) {
  double u=rng.next(),phi=2*engine::pi*rng.next(),r=std::sqrt(u);
  return Frame(normal).world({r*std::cos(phi),r*std::sin(phi),std::sqrt(1-u)});
}
inline Vec3 cone_sample(const Vec3& axis,double cosine,Sampler& rng) {
  double z=1-rng.next()*(1-cosine),phi=2*engine::pi*rng.next(),r=std::sqrt(std::max(0.0,1-z*z));
  return Frame(axis).world({r*std::cos(phi),r*std::sin(phi),z});
}
} }
