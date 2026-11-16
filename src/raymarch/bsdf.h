#pragma once
#include "sampler.h"
#include "../shade/shade.h"

namespace ggplot3d { namespace raymarch {
inline double fresnel(double cosine,double eta) {
  cosine=std::clamp(std::abs(cosine),0.0,1.0);double sin2=(1-cosine*cosine)/(eta*eta);
  if(sin2>=1)return 1;
  double ct=std::sqrt(1-sin2);
  double a=(eta*cosine-ct)/(eta*cosine+ct),b=(cosine-eta*ct)/(cosine+eta*ct);return (a*a+b*b)*.5;
}
inline Vec3 schlick(const Vec3& f0,double cosine) {
  double f=std::pow(1-std::clamp(std::abs(cosine),0.0,1.0),5);return plus(scaled(f0,1-f),Vec3{f,f,f});
}
inline double ggx_lambda(double cosine,double alpha) {
  double c2=cosine*cosine;if(c2==0)return INFINITY;
  double x=alpha*alpha*(1-c2)/c2;return x/(2*(std::sqrt(1+x)+1));
}
inline double ggx_D(double cosine,double alpha) {
  if(cosine<=0)return 0;
  double c2=cosine*cosine,d=(1-c2)+c2*alpha*alpha;return alpha*alpha/(engine::pi*d*d);
}
inline Vec3 ggx_visible(const Vec3& wo,double alpha,double u,double v) {
  Vec3 h=unit({alpha*wo[0],alpha*wo[1],wo[2]});double len=h[0]*h[0]+h[1]*h[1];
  Vec3 t=len>0 ? scaled(Vec3{-h[1],h[0],0},1/std::sqrt(len)):Vec3{1,0,0},b=engine::cross(h,t);
  double r=std::sqrt(u),phi=2*engine::pi*v,x=r*std::cos(phi),y=r*std::sin(phi),s=.5*(1+h[2]);
  y=(1-s)*std::sqrt(1-x*x)+s*y;Vec3 n=plus(plus(scaled(t,x),scaled(b,y)),scaled(h,std::sqrt(std::max(0.0,1-x*x-y*y))));
  return unit({alpha*n[0],alpha*n[1],std::max(0.0,n[2])});
}
inline Vec3 reflected(const Vec3& wo,const Vec3& h){return engine::sub(scaled(h,2*engine::dot(wo,h)),wo);}
inline Vec3 refracted(const Vec3& wo,const Vec3& h,double eta) {
  double c=engine::dot(wo,h),s2=(1-c*c)/(eta*eta);if(s2>=1)return {0,0,0};
  return plus(scaled(wo,-1/eta),scaled(h,c/eta-std::sqrt(1-s2)));
}

// Directional hemispherical GGX albedo for the reciprocal multiple-scattering lobe.
struct GgxEnergy {
  std::array<std::array<double,65>,33> energy{};std::array<double,33> average{};
  GgxEnergy() {
    for(int a=0;a<=32;++a){double alpha=std::max(1e-4,std::pow(a/32.0,2));
      for(int m=0;m<=64;++m){double mu=std::max(1e-5,m/64.0);Vec3 wo{std::sqrt(1-mu*mu),0,mu};double sum=0;
        for(int j=0;j<1024;++j){Vec3 h=ggx_visible(wo,alpha,(j+.5)/1024,(reverse_bits(j)+.5)*0x1.0p-32),wi=reflected(wo,h);
          if(wi[2]>0)sum+=(1+ggx_lambda(mu,alpha))/(1+ggx_lambda(mu,alpha)+ggx_lambda(wi[2],alpha));}
        energy[a][m]=sum/1024;}
      for(int m=0;m<64;++m)average[a]+=(energy[a][m]*m+energy[a][m+1]*(m+1))/(64.0*64);}
  }
  double at(double roughness,double mu)const {
    double a=roughness*32,m=std::clamp(std::abs(mu),0.0,1.0)*64;int i=std::min(31,int(a)),j=std::min(63,int(m));a-=i;m-=j;
    return (1-a)*((1-m)*energy[i][j]+m*energy[i][j+1])+a*((1-m)*energy[i+1][j]+m*energy[i+1][j+1]);
  }
  double mean(double r)const{double a=r*32;int i=std::min(31,int(a));return average[i]*(1-a+i)+average[i+1]*(a-i);}
};
inline const GgxEnergy& ggx_energy(){static const GgxEnergy e;return e;}
struct BsdfValue {Vec3 f{0,0,0};double pdf=0;};
struct BsdfSample {Vec3 direction{0,0,0},weight{0,0,0};double pdf=0,eta=1;bool delta=false;};
struct Bsdf {
  shade::Material m;Vec3 colour,wo;Frame frame;double eta,alpha,metal,diffuse,glass,specular;
  Bsdf(const shade::Material& material,const Vec3& base,const Vec3& n,const Vec3& view,bool entering):
    m(material),colour(base),wo(Frame(n).local(view)),frame(n),eta(entering ? m.ior:1/m.ior),alpha(m.roughness*m.roughness),
    metal(m.metallic),diffuse((1-metal)*(1-m.transmission)),glass((1-metal)*m.transmission),specular(std::clamp(m.specular,0.0,1.0)){}
  double reflection_probability()const{return .5*specular;}
  BsdfValue evaluate(const Vec3& direction)const {
    Vec3 wi=frame.local(direction);BsdfValue out;double co=wo[2],ci=wi[2];if(co<=0 || ci==0)return out;
    double rp=reflection_probability();
    if(ci>0) {
      double fo=specular*fresnel(co,eta),fi=specular*fresnel(ci,eta);
      out.f=scaled(colour,diffuse*(1-fo)*(1-fi)/engine::pi);
      out.pdf=diffuse*(1-rp)*ci/engine::pi;
      if(alpha>0 && metal>0) {
        double eo=ggx_energy().at(m.roughness,co),ei=ggx_energy().at(m.roughness,ci),ea=ggx_energy().mean(m.roughness);
        out.f=plus(out.f,scaled(colour,metal*(1-eo)*(1-ei)/(engine::pi*std::max(1e-8,1-ea))));
        out.pdf+=metal*.5*ci/engine::pi;
      }
    }
    if(alpha==0)return out;
    bool reflection=ci>0;Vec3 h=unit(plus(wo,scaled(wi,reflection ? 1:eta)));if(h[2]<0)h=scaled(h,-1);
    double oh=engine::dot(wo,h),ih=engine::dot(wi,h);if(oh<=0 || ih*ci<=0)return out;
    double D=ggx_D(h[2],alpha),G1=1/(1+ggx_lambda(co,alpha)),G=1/(1+ggx_lambda(co,alpha)+ggx_lambda(ci,alpha));
    double F=fresnel(oh,eta),ph=D*G1*oh/co;
    if(reflection) {
      Vec3 reflectance=plus(scaled(schlick(colour,oh),metal),Vec3{(glass+diffuse*specular)*F,(glass+diffuse*specular)*F,(glass+diffuse*specular)*F});
      out.f=plus(out.f,scaled(reflectance,D*G/(4*co*ci)));
      out.pdf+=(metal*.5+glass*F+diffuse*rp)*ph/(4*oh);
    }else if(glass>0){double denominator=ih+oh/eta;denominator*=denominator;
      out.f=scaled(colour,glass*(1-F)*D*G*std::abs(ih*oh/(ci*co*denominator))/(eta*eta));
      out.pdf=glass*(1-F)*ph*std::abs(ih)/denominator;}
    return out;
  }
  BsdfSample sample(Sampler& rng)const {
    BsdfSample out;double choice=rng.next();Vec3 wi;bool dielectric=false;
    if(choice<metal){if(alpha>0 && rng.next()<.5)wi=frame.local(cosine_sample(frame.z,rng));
      else wi=reflected(wo,alpha==0 ? Vec3{0,0,1}:ggx_visible(wo,alpha,rng.next(),rng.next()));
      if(wi[2]<=0)return out;}
    else if(choice<metal+glass)dielectric=true;
    else if(rng.next()>=reflection_probability())wi=frame.local(cosine_sample(frame.z,rng));
    else {wi=reflected(wo,alpha==0 ? Vec3{0,0,1}:ggx_visible(wo,alpha,rng.next(),rng.next()));if(wi[2]<=0)return out;}
    if(dielectric && eta==1){out.direction=frame.world(scaled(wo,-1));out.delta=true;out.pdf=glass;out.weight=colour;return out;}
    if(dielectric){Vec3 h=alpha==0 ? Vec3{0,0,1}:ggx_visible(wo,alpha,rng.next(),rng.next());double F=fresnel(engine::dot(wo,h),eta);
      bool reflect=rng.next()<F;wi=reflect ? reflected(wo,h):refracted(wo,h,eta);if((wi[2]>0)!=reflect)return out;if(wi[2]<0)out.eta=eta;}
    if(wi[2]==0)return out;
    out.direction=frame.world(wi);
    if(alpha==0 && (dielectric || choice<metal || (wi[0]==-wo[0] && wi[1]==-wo[1]))) {
      out.delta=true;double F=fresnel(wo[2],eta);
      if(wi[2]<0){out.pdf=glass*(1-F);out.weight=scaled(colour,1/(eta*eta));}
      else {out.pdf=metal+glass*F+diffuse*reflection_probability();Vec3 f=plus(scaled(schlick(colour,wo[2]),metal),Vec3{(glass+diffuse*specular)*F,(glass+diffuse*specular)*F,(glass+diffuse*specular)*F});
        if(out.pdf>0)out.weight=scaled(f,1/out.pdf);}
    }else{if((wi[2]<0)!=dielectric && wi[2]<0)return {};auto value=evaluate(out.direction);out.pdf=value.pdf;
      if(out.pdf>0)out.weight=scaled(value.f,std::abs(wi[2])/out.pdf);}
    return out;
  }
};
} }
