#pragma once
#include "scene.h"

namespace ggplot3d { namespace raymarch {
struct Environment {
  int height,width;std::vector<Vec3> pixels;std::vector<double> rows,columns;std::array<Vec3,3> axes;double power=0;
  explicit Environment(const Rcpp::List& input) {
    Rcpp::NumericVector image=input["image"];Rcpp::IntegerVector dim=image.attr("dim");Rcpp::NumericMatrix frame=input["frame"];
    require(dim.size()==3 && dim[2]==3 && dim[0]>0 && dim[1]>0,"Invalid environment dimensions");
    height=dim[0];width=dim[1];pixels.resize(std::size_t(width)*height);rows.resize(height+1);columns.resize(std::size_t(height)*(width+1));
    for(int k=0;k<3;++k)axes[k]={frame(0,k),frame(1,k),frame(2,k)};
    for(int y=0;y<height;++y){double area=2*engine::pi/width*(std::cos(engine::pi*y/height)-std::cos(engine::pi*(y+1)/height));
      for(int x=0;x<width;++x){Vec3 c;for(int k=0;k<3;++k){c[k]=image[y+height*(x+width*k)];require(std::isfinite(c[k]) && c[k]>=0,"Invalid environment radiance");}
        pixels[x+width*y]=c;columns[y*(width+1)+x+1]=columns[y*(width+1)+x]+luminance(c);}
      rows[y+1]=rows[y]+columns[y*(width+1)+width]*area;}power=rows.back();
  }
  Vec3 local(const Vec3& d)const{return {engine::dot(d,axes[0]),engine::dot(d,axes[1]),engine::dot(d,axes[2])};}
  Vec3 world(const Vec3& d)const{return plus(plus(scaled(axes[0],d[0]),scaled(axes[1],d[1])),scaled(axes[2],d[2]));}
  int cell(const Vec3& d)const {Vec3 p=local(d);double phi=std::atan2(p[1],p[0]);if(phi<0)phi+=2*engine::pi;
    return std::min(width-1,int(phi*width/(2*engine::pi)))+width*std::min(height-1,int(std::acos(std::clamp(p[2],-1.0,1.0))*height/engine::pi));}
  Vec3 evaluate(const Vec3& d)const{return pixels[cell(d)];}
  double pdf(const Vec3& d)const{return power>0 ? luminance(evaluate(d))/power:0;}
  Vec3 sample(Sampler& rng)const {
    double u=rng.next()*power;int y=std::clamp(int(std::upper_bound(rows.begin(),rows.end(),u)-rows.begin())-1,0,height-1);
    double t=(u-rows[y])/(rows[y+1]-rows[y]);auto begin=columns.begin()+y*(width+1);double v=rng.next()*begin[width];
    int x=std::clamp(int(std::upper_bound(begin,begin+width+1,v)-begin)-1,0,width-1);double f=(v-begin[x])/(begin[x+1]-begin[x]);
    double z=(1-t)*std::cos(engine::pi*y/height)+t*std::cos(engine::pi*(y+1)/height),phi=2*engine::pi*(x+f)/width,r=std::sqrt(std::max(0.0,1-z*z));
    return world({r*std::cos(phi),r*std::sin(phi),z});
  }
};
struct LightSample {Vec3 direction{0,0,1},radiance{0,0,0};double distance=INFINITY,pdf=0;bool delta=false;};
struct PathLights {
  shade::Lights lights;std::vector<Environment> environments;std::vector<int> emitters;std::vector<double> cdf{0},areas;
  Vec3 eye,toward;bool perspective;double cosine,omega;
  PathLights(const TraceScene& scene,const Rcpp::List& input,const engine::Projection& projection,double radius):
    lights(shade::read_lights(input)),eye(projection.resolved.eye),toward(scaled(projection.resolved.forward,-1)),perspective(projection.camera.perspective),
    cosine(std::cos(radius*engine::pi/180)),omega(2*engine::pi*(1-cosine)) {
    if(input.containsElementNamed("environment")){Rcpp::List env=input["environment"];for(SEXP e:env)environments.emplace_back(Rcpp::List(e));}
    areas.resize(scene.triangles.size());
    for(std::size_t i=0;i<scene.triangles.size();++i){const auto& t=scene.triangles[i];const auto& m=scene.materials[t.material];
      double area=.5*norm(engine::cross(engine::sub(t.p[1],t.p[0]),engine::sub(t.p[2],t.p[0])));areas[i]=area;
      double power=area*luminance({m.emission.x,m.emission.y,m.emission.z})*(t.alpha[0]+t.alpha[1]+t.alpha[2])/3;
      if(power>0){emitters.push_back(i);cdf.push_back(cdf.back()+power);}}
  }
  int count()const{return 1+lights.list.size()+environments.size()+(!emitters.empty());}
  Vec3 axis(const shade::Light& l,const Vec3& p)const {
    if(l.kind==3)return perspective ? unit(engine::sub(eye,p)):toward;
    return {l.vector.x,l.vector.y,l.vector.z};
  }
  double sky_pdf(const Vec3& n,const Vec3& d)const{return norm(n)>0 ? std::max(0.0,engine::dot(n,d))/engine::pi:1/(4*engine::pi);}
  LightSample sample(int id,const Vec3& p,const Vec3& n,const TraceScene& scene,Sampler& rng)const {
    LightSample s;
    if(id==0){s.radiance={lights.ambient.x,lights.ambient.y,lights.ambient.z};if(luminance(s.radiance)==0)return s;
      s.direction=norm(n)>0 ? cosine_sample(n,rng):cone_sample({0,0,1},-1,rng);s.pdf=sky_pdf(n,s.direction);return s;}
    --id;
    if(id<int(lights.list.size())){const auto& l=lights.list[id];Vec3 c{l.colour.x,l.colour.y,l.colour.z};
      if(l.kind==2){Vec3 d=engine::sub(axis(l,p),p);s.distance=norm(d);if(s.distance==0)return s;s.direction=scaled(d,1/s.distance);
        s.radiance=scaled(c,engine::pi/std::pow(std::max(1e-3,s.distance),l.falloff));s.pdf=1;s.delta=true;
      }else if(l.kind==4){s.direction=cone_sample({0,0,1},-1,rng);double t=.5*(1+engine::dot(s.direction,axis(l,p)));
        s.radiance=plus(scaled(c,t),scaled(Vec3{l.ground.x,l.ground.y,l.ground.z},1-t));s.pdf=1/(4*engine::pi);
      }else{s.direction=omega>0 ? cone_sample(axis(l,p),cosine,rng):axis(l,p);s.delta=omega==0;s.pdf=s.delta ? 1:1/omega;
        s.radiance=scaled(c,engine::pi*s.pdf);}return s;}
    id-=lights.list.size();
    if(id<int(environments.size())){const auto& e=environments[id];if(e.power==0)return s;s.direction=e.sample(rng);s.radiance=e.evaluate(s.direction);s.pdf=e.pdf(s.direction);return s;}
    if(emitters.empty())return s;
    double u=rng.next()*cdf.back();int j=std::clamp(int(std::upper_bound(cdf.begin(),cdf.end(),u)-cdf.begin())-1,0,int(emitters.size())-1),index=emitters[j];
    const auto& t=scene.triangles[index];double a=std::sqrt(rng.next()),b=rng.next(),weights[3]={1-a,a*(1-b),a*b};Vec3 q{0,0,0};double alpha=0;
    for(int k=0;k<3;++k){q=plus(q,scaled(t.p[k],weights[k]));alpha+=weights[k]*t.alpha[k];}
    Vec3 d=engine::sub(q,p);s.distance=norm(d);if(s.distance==0)return s;s.direction=scaled(d,1/s.distance);
    double facing=std::abs(engine::dot(unit(engine::cross(engine::sub(t.p[1],t.p[0]),engine::sub(t.p[2],t.p[0]))),s.direction));
    if(facing==0)return s;
    s.pdf=(cdf[j+1]-cdf[j])/cdf.back()*s.distance*s.distance/(areas[index]*facing);
    if(!scene.texture_faces.empty() && scene.texture_faces[index].index>0){const auto& texture=scene.texture_faces[index];std::array<double,2> uv{};
      for(int j=0;j<3;++j)for(int k=0;k<2;++k)uv[k]+=weights[j]*texture.uv[j][k];
      double ta=scene.textures[texture.index-1].sample(uv[0],uv[1],1)[3];alpha*=ta+(1-ta)*texture.fill_alpha;}
    const auto& m=scene.materials[t.material];s.radiance=scaled(Vec3{m.emission.x,m.emission.y,m.emission.z},alpha);return s;
  }
  Vec3 environment(const Vec3& d,const Vec3& p,const Vec3& n,double bsdf_pdf,bool delta,int estimator)const {
    Vec3 out{0,0,0};auto add=[&](const Vec3& c,double pdf){double w=delta || estimator==2 || pdf==0 ? 1:estimator==1 ? 0:power_weight(bsdf_pdf,pdf);out=plus(out,scaled(c,w));};
    add({lights.ambient.x,lights.ambient.y,lights.ambient.z},sky_pdf(n,d));
    for(const auto& l:lights.list){Vec3 c{l.colour.x,l.colour.y,l.colour.z};
      if(l.kind==4){double t=.5*(1+engine::dot(d,axis(l,p)));add(plus(scaled(c,t),scaled(Vec3{l.ground.x,l.ground.y,l.ground.z},1-t)),1/(4*engine::pi));}
      else if(l.kind!=2 && omega>0 && engine::dot(d,axis(l,p))>=cosine)add(scaled(c,engine::pi/omega),1/omega);}
    for(const auto& e:environments)add(e.evaluate(d),e.pdf(d));
    return out;
  }
  double emission_pdf(const SurfaceHit& hit,const Vec3& p,const Vec3& direction,const TraceScene& scene)const {
    if(hit.triangle<0 || emitters.empty())return 0;
    auto it=std::lower_bound(emitters.begin(),emitters.end(),hit.triangle);if(it==emitters.end() || *it!=hit.triangle)return 0;
    int j=it-emitters.begin();double c=std::abs(engine::dot(hit.geometric,direction));
    if(c==0)return 0;
    return (cdf[j+1]-cdf[j])/cdf.back()*hit.t*hit.t/(areas[hit.triangle]*c);
  }
};
} }
