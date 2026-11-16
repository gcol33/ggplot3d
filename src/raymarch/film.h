#pragma once
#include "path.h"
#include "denoise.h"

namespace ggplot3d { namespace raymarch {
struct PathPixel {
  int count=0;bool stopped=false;std::array<double,4> sum{},mean{},m2{};
  std::array<std::array<double,11>,2> halves{};
  void add(const PathValue& value){int half=count%2;++count;
    for(int k=0;k<4;++k){sum[k]+=value.rgba[k];double d=value.rgba[k]-mean[k];mean[k]+=d/count;m2[k]+=d*(value.rgba[k]-mean[k]);halves[half][k]+=value.rgba[k];}
    for(int k=0;k<7;++k)halves[half][k+4]+=value.feature[k];}
  bool converged(double threshold,int minimum)const {
    if(threshold<=0 || count<std::max(4,minimum) || count%2)return false;
    for(int k=0;k<4;++k){double error=std::sqrt(std::max(0.0,m2[k]/(count*(count-1))));
      double scale=k==3 ? 1:std::max(1e-3,std::abs(mean[k]));if(error>threshold*scale)return false;}return true;
  }
};
inline Rcpp::List finish_film(const std::vector<PathPixel>& film,int width,int height,bool denoise) {
  int n=film.size();Rcpp::NumericMatrix rgba(n,4),sum(n,4),variance(n,4),features(n,7);Rcpp::IntegerVector count(n);
  FilterImage a(width,height,3),b=a,fa(width,height,7),fb=fa;
  for(int p=0;p<n;++p){const auto& f=film[p];count[p]=f.count;
    for(int k=0;k<4;++k){rgba(p,k)=f.mean[k];sum(p,k)=f.sum[k];variance(p,k)=f.count>1 ? f.m2[k]/(f.count*(f.count-1)):0;}
    int na=(f.count+1)/2,nb=f.count/2;
    for(int k=0;k<3;++k){a.at(p,k)=f.halves[0][k]/std::max(1,na);b.at(p,k)=nb ? f.halves[1][k]/nb:a.at(p,k);}
    for(int k=0;k<7;++k){fa.at(p,k)=f.halves[0][k+4]/std::max(1,na);fb.at(p,k)=nb ? f.halves[1][k+4]/nb:fa.at(p,k);
      features(p,k)=(f.halves[0][k+4]+f.halves[1][k+4])/std::max(1,f.count);}}
  Rcpp::NumericMatrix filtered(n,3);auto output=denoise ? nfor(a,b,fa,fb):image_mean(a,b);
  for(int p=0;p<n;++p)for(int k=0;k<3;++k)filtered(p,k)=denoise ? std::max(0.0,output.at(p,k)):rgba(p,k);
  return Rcpp::List::create(Rcpp::Named("rgba")=rgba,Rcpp::Named("sum")=sum,Rcpp::Named("variance")=variance,Rcpp::Named("features")=features,
    Rcpp::Named("counts")=count,Rcpp::Named("colour")=filtered);
}
} }
