#pragma once
#include "scene.h"
#include <cstdint>

namespace ggplot3d { namespace raymarch {
inline std::uint64_t mix_bits(std::uint64_t x) {
  x^=x>>30;x*=0xbf58476d1ce4e5b9ULL;x^=x>>27;x*=0x94d049bb133111ebULL;return x^(x>>31);
}
struct Sampler {
  std::uint64_t key,dimension=0;
  Sampler(std::uint64_t seed,std::uint64_t pixel,std::uint64_t sample):key(mix_bits(seed)^mix_bits(pixel+1)^mix_bits(sample+0x9e3779b97f4a7c15ULL)){}
  double next() {return (double(mix_bits(key+0x9e3779b97f4a7c15ULL*(++dimension))>>11)+0.5)*0x1.0p-53;}
};
inline Vec3 cosine_sample(const Vec3& normal,Sampler& rng) {
  double u=rng.next(),phi=2*engine::pi*rng.next(),r=std::sqrt(u);
  return basis_sample(normal,r*std::cos(phi),r*std::sin(phi),std::sqrt(1-u));
}
inline Vec3 cone_sample(const Vec3& axis,double cosine,Sampler& rng) {
  double z=1-rng.next()*(1-cosine),phi=2*engine::pi*rng.next(),r=std::sqrt(std::max(0.0,1-z*z));
  return basis_sample(axis,r*std::cos(phi),r*std::sin(phi),z);
}
inline Ray lens_ray(const engine::Projection& p,double x,double y,double aperture,double focus,Sampler& rng) {
  Ray r=camera_ray(p,x,y);const auto& c=p.resolved;
  if(aperture>0) {
    Vec3 target=point(r.origin,r.direction,focus/engine::dot(r.direction,c.forward));
    double radius=aperture*std::sqrt(rng.next()),phi=2*engine::pi*rng.next();
    for(int k=0;k<3;++k)r.origin[k]+=radius*(std::cos(phi)*c.right[k]+std::sin(phi)*c.up[k]);
    r.direction=unit(engine::subtract(target,r.origin));
    double forward=engine::dot(r.direction,c.forward),offset=engine::dot(engine::subtract(r.origin,c.eye),c.forward);
    r.near=(c.near-offset)/forward;r.far=(c.far-offset)/forward;
  }
  return r;
}
struct PathLight {Vec3 direction;double sky,sun,cosine;};
inline std::array<double,4> path_sample(const TraceScene& scene,Ray ray,const PathLight& light,int bounces,Sampler& rng,double epsilon) {
  Vec3 radiance{0,0,0},throughput{1,1,1};bool covered=false;std::vector<Jet> scratch;
  for(int bounce=0;bounce<bounces;++bounce) {
    SurfaceHit hit;
    while(true){hit=scene.intersect(ray,scratch);if(!hit.found || hit.alpha>=1 || rng.next()<hit.alpha)break;
      ray.origin=point(ray.origin,ray.direction,hit.t+epsilon);ray.far-=hit.t+epsilon;ray.near=0;}
    if(!hit.found){if(covered)for(int k=0;k<3;++k)radiance[k]+=throughput[k]*light.sky;break;}
    covered=true;Vec3 n=hit.normal;if(engine::dot(n,ray.direction)>0)for(double& v:n)v=-v;
    Vec3 p=point(ray.origin,ray.direction,hit.t),origin=point(p,n,epsilon);
    if(hit.illustrative>0) {
      shade::vec3 colour=shade::shade(shade::to_vec3(hit.normal),shade::to_vec3(p),-shade::to_vec3(ray.direction),
        shade::to_vec3(hit.colour),scene.materials[hit.illustrative],scene.lights);
      for(int k=0;k<3;++k)radiance[k]+=throughput[k]*colour[k];
      break;
    }
    Vec3 l=cone_sample(light.direction,light.cosine,rng);double cosine=std::max(0.0,engine::dot(n,l));
    if(cosine>0 && light.sun>0){double visible=scene.visibility({origin,l,0,INFINITY},scratch,epsilon);
      for(int k=0;k<3;++k)radiance[k]+=throughput[k]*hit.colour[k]*light.sun*cosine*visible;}
    for(int k=0;k<3;++k)throughput[k]*=hit.colour[k];
    if(bounce>=3){double survive=std::min(.95,*std::max_element(throughput.begin(),throughput.end()));if(survive<=0 || rng.next()>=survive)break;for(double& v:throughput)v/=survive;}
    ray={origin,cosine_sample(n,rng),0,INFINITY};
    if(bounce+1==bounces)for(int k=0;k<3;++k)radiance[k]+=throughput[k]*light.sky*scene.visibility(ray,scratch,epsilon);
  }
  return {radiance[0],radiance[1],radiance[2],covered ? 1.0:0.0};
}
} }
