#pragma once
#include "bsdf.h"
#include "path-light.h"

namespace ggplot3d { namespace raymarch {
inline Ray lens_ray(const engine::Projection& p,double x,double y,double aperture,double focus,Sampler& rng) {
  Ray r=camera_ray(p,x,y);const auto& c=p.resolved;
  if(aperture>0) {
    Vec3 target=point(r.origin,r.direction,focus/engine::dot(r.direction,c.forward));
    double radius=aperture*std::sqrt(rng.next()),phi=2*engine::pi*rng.next();
    for(int k=0;k<3;++k)r.origin[k]+=radius*(std::cos(phi)*c.right[k]+std::sin(phi)*c.up[k]);
    r.direction=unit(engine::sub(target,r.origin));
    double forward=engine::dot(r.direction,c.forward),offset=engine::dot(engine::sub(r.origin,c.eye),c.forward);
    r.near=(c.near-offset)/forward;r.far=(c.far-offset)/forward;
  }return r;
}
inline double phase_hg(double cosine,double g) {
  double d=1+g*g-2*g*cosine;return (1-g*g)/(4*engine::pi*d*std::sqrt(d));
}
inline Vec3 phase_sample(const Vec3& forward,double g,Sampler& rng) {
  double u=rng.next(),c;if(std::abs(g)<1e-3)c=1-2*u;else{double a=(1-g*g)/(1-g+2*g*u);c=(1+g*g-a*a)/(2*g);}
  c=std::clamp(c,-1.0,1.0);double phi=2*engine::pi*rng.next(),s=std::sqrt(1-c*c);return Frame(forward).world({s*std::cos(phi),s*std::sin(phi),c});
}
inline double medium_transmittance(const TraceScene& scene,const Ray& ray,Sampler& rng) {
  double tr=1;for(const auto& v:scene.volumes)if(v.volume.mode==0)tr*=v.track(ray,rng,false);return tr;
}
struct PathValue {std::array<double,4> rgba{};std::array<double,7> feature{};};
inline PathValue path_sample(const TraceScene& scene,Ray ray,const PathLights& lights,int bounces,Sampler& rng,double epsilon,int estimator=0) {
  Vec3 radiance{0,0,0},throughput{1,1,1},previous_normal{0,0,0},previous_point=ray.origin;
  bool covered=false,previous_delta=true;double previous_pdf=0,eta_scale=1;std::vector<Jet> scratch;PathValue result;
  for(int bounce=0;bounce<=bounces;++bounce) {
    rng.stream(bounce,0);SurfaceHit hit;Ray segment=ray;double travelled=0;std::vector<int> excluded;
    while(true){hit=scene.intersect(segment,scratch,&excluded);if(!hit.found || hit.alpha>=1 || rng.next()<hit.alpha)break;
      if(hit.gaussian>=0){excluded.push_back(hit.gaussian);continue;}
      double step=hit.t+epsilon;travelled+=step;segment.origin=point(segment.origin,segment.direction,step);segment.far-=step;segment.near=0;
      if(segment.far<=0){hit.found=false;break;}}
    if(hit.found)hit.t+=travelled;
    Ray flight=ray;if(hit.found)flight.far=hit.t;int medium=-1;double distance=flight.far;
    if(bounce<bounces)for(std::size_t j=0;j<scene.volumes.size();++j)if(scene.volumes[j].volume.mode==0){rng.stream(bounce,1000+j);
      double t=scene.volumes[j].track(flight,rng,true);if(t<distance){distance=t;medium=j;flight.far=t;}}
    if(medium>=0) {
      covered=true;const auto& v=scene.volumes[medium];Vec3 p=point(ray.origin,ray.direction,distance);double length;Ray local=v.local(ray,length);
      Vec3 albedo=v.volume.transfer(v.volume.evaluate(point(local.origin,local.direction,distance*length)).v).first;
      throughput=product(throughput,albedo);if(bounce==0){for(int k=0;k<3;++k)result.feature[k]=albedo[k];result.feature[6]=distance;}
      if(estimator!=2)for(int id=0;id<lights.count();++id){rng.stream(bounce,10+id);auto l=lights.sample(id,p,{0,0,0},scene,rng);if(l.pdf<=0)continue;
        Ray shadow{point(p,l.direction,epsilon),l.direction,0,l.distance-epsilon};double phase=phase_hg(engine::dot(ray.direction,l.direction),v.g);
        double tr=scene.visibility(shadow,scratch,epsilon)*medium_transmittance(scene,shadow,rng),w=l.delta || estimator==1 ? 1:power_weight(l.pdf,phase);
        radiance=plus(radiance,scaled(product(throughput,l.radiance),phase*tr*w/l.pdf));}
      rng.stream(bounce,2);Vec3 direction=phase_sample(ray.direction,v.g,rng);previous_pdf=phase_hg(engine::dot(ray.direction,direction),v.g);
      previous_delta=false;previous_normal={0,0,0};previous_point=p;ray={point(p,direction,epsilon),direction,0,INFINITY};
    }else {
      if(bounce==bounces && !scene.volumes.empty()){rng.stream(bounce,1000);throughput=scaled(throughput,medium_transmittance(scene,flight,rng));}
      if(!hit.found){if(covered)radiance=plus(radiance,product(throughput,lights.environment(ray.direction,previous_point,previous_normal,previous_pdf,previous_delta,estimator)));break;}
      if(bounce==bounces){const auto& m=scene.materials[hit.material];double pdf=lights.emission_pdf(hit,previous_point,ray.direction,scene);
        double w=previous_delta || estimator==2 || pdf==0 ? 1:estimator==1 ? 0:power_weight(previous_pdf,pdf);
        radiance=plus(radiance,scaled(product(throughput,Vec3{m.emission.x,m.emission.y,m.emission.z}),w));break;}
      covered=true;Vec3 n=hit.normal,p=point(ray.origin,ray.direction,hit.t),view=scaled(ray.direction,-1);bool entering=engine::dot(hit.geometric,view)>0;
      if(engine::dot(n,view)<0)n=scaled(n,-1);
      const auto& material=scene.materials[hit.material];
      if(!entering && material.has_back)hit.colour={material.back.x,material.back.y,material.back.z};
      if(bounce==0){for(int k=0;k<3;++k){result.feature[k]=hit.colour[k];result.feature[k+3]=n[k];}result.feature[6]=hit.t;}
      if(hit.illustrative>0){auto c=shade::shade(shade::to_vec3(hit.normal),shade::to_vec3(p),shade::to_vec3(view),shade::to_vec3(hit.colour),material,scene.lights);
        radiance=plus(radiance,product(throughput,Vec3{c.x,c.y,c.z}));break;}
      double epdf=lights.emission_pdf(hit,previous_point,ray.direction,scene),ew=previous_delta || estimator==2 || epdf==0 ? 1:estimator==1 ? 0:power_weight(previous_pdf,epdf);
      radiance=plus(radiance,scaled(product(throughput,Vec3{material.emission.x,material.emission.y,material.emission.z}),ew));
      Bsdf bsdf(material,hit.colour,n,view,entering);
      if(estimator!=2)for(int id=0;id<lights.count();++id){rng.stream(bounce,10+id);auto l=lights.sample(id,p,n,scene,rng);if(l.pdf<=0)continue;
        auto value=bsdf.evaluate(l.direction);double cosine=std::abs(engine::dot(n,l.direction));if(luminance(value.f)<=0)continue;
        Vec3 ng=engine::dot(hit.geometric,l.direction)>=0 ? hit.geometric:scaled(hit.geometric,-1);
        Ray shadow{point(p,ng,epsilon),l.direction,0,l.distance-epsilon*2};double tr=scene.visibility(shadow,scratch,epsilon)*medium_transmittance(scene,shadow,rng);
        double w=l.delta || estimator==1 ? 1:power_weight(l.pdf,value.pdf);
        radiance=plus(radiance,scaled(product(product(throughput,value.f),l.radiance),cosine*tr*w/l.pdf));}
      rng.stream(bounce,2);auto sample=bsdf.sample(rng);if(sample.pdf<=0 || luminance(sample.weight)<=0)break;
      throughput=product(throughput,sample.weight);eta_scale*=sample.eta*sample.eta;
      previous_pdf=sample.pdf;previous_delta=sample.delta;previous_normal=n;previous_point=p;
      Vec3 ng=engine::dot(hit.geometric,sample.direction)>=0 ? hit.geometric:scaled(hit.geometric,-1);
      ray={point(p,ng,epsilon),sample.direction,0,INFINITY};
    }
    if(bounce>=3){rng.stream(bounce,3);double survive=std::min(.95,*std::max_element(throughput.begin(),throughput.end())*eta_scale);
      if(survive<=0 || rng.next()>=survive)break;
      throughput=scaled(throughput,1/survive);}
  }
  result.rgba={radiance[0],radiance[1],radiance[2],covered ? 1.0:0.0};return result;
}
} }
