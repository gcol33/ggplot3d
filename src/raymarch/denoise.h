#pragma once
#include "sampler.h"

namespace ggplot3d { namespace raymarch {
struct FilterImage {
  int width=0,height=0,channels=0;std::vector<double> data;
  FilterImage()=default;
  FilterImage(int w,int h,int c):width(w),height(h),channels(c),data(std::size_t(w)*h*c){}
  int size()const{return width*height;}
  double& at(int i,int c){return data[std::size_t(i)*channels+c];}
  double at(int i,int c)const{return data[std::size_t(i)*channels+c];}
};
inline FilterImage image_mean(const FilterImage& a,const FilterImage& b) {
  FilterImage out=a;for(std::size_t i=0;i<a.data.size();++i)out.data[i]=(a.data[i]+b.data[i])*.5;return out;
}
inline FilterImage half_variance(const FilterImage& a,const FilterImage& b) {
  FilterImage out=a;for(std::size_t i=0;i<a.data.size();++i){double d=a.data[i]-b.data[i];out.data[i]=d*d*.25;}return out;
}
// Summed-area patch distances bound storage by one displacement image.
template<class Function> inline void nlm_weights(const FilterImage& guide,const FilterImage& variance,int patch,int radius,double bandwidth,Function apply,int begin=0,int end=-1) {
  int w=guide.width,h=guide.height,c=guide.channels;if(end<0)end=w*h;
  int left=std::max(0,begin/h-patch),right=std::min(w,(end-1)/h+1+patch);
  std::vector<double> integral(std::size_t(right-left+1)*(h+1));
  for(int dx=-radius;dx<=radius;++dx)for(int dy=-radius;dy<=radius;++dy){std::fill(integral.begin(),integral.end(),0);
    for(int x=left;x<right;++x)for(int y=0;y<h;++y){int qx=std::clamp(x+dx,0,w-1),qy=std::clamp(y+dy,0,h-1),p=y+h*x,q=qy+h*qx;double d=0;
      for(int k=0;k<c;++k){double delta=guide.at(p,k)-guide.at(q,k),vp=variance.at(p,k),vq=variance.at(q,k);
        d+=(delta*delta-vp-std::min(vp,vq))/(1e-12+bandwidth*bandwidth*(vp+vq));}
      int a=(x-left+1)*(h+1)+y+1;integral[a]=d/c+integral[a-1]+integral[a-h-1]-integral[a-h-2];}
    for(int x=std::max(begin/h,-dx);x<std::min((end-1)/h+1,w-dx);++x)for(int y=std::max(0,-dy);y<std::min(h,h-dy);++y){
      if(y+h*x<begin || y+h*x>=end)continue;
      int x0=std::max(0,x-patch),x1=std::min(w,x+patch+1),y0=std::max(0,y-patch),y1=std::min(h,y+patch+1);
      double distance=(integral[(x1-left)*(h+1)+y1]-integral[(x0-left)*(h+1)+y1]-integral[(x1-left)*(h+1)+y0]+integral[(x0-left)*(h+1)+y0])/((x1-x0)*(y1-y0));
      apply(y+h*x,y+dy+h*(x+dx),std::exp(-std::max(0.0,distance)));}
  }
}
inline FilterImage nlmeans(const FilterImage& input,const FilterImage& guide,const FilterImage& variance,int patch,int radius,double k) {
  FilterImage out(input.width,input.height,input.channels);std::vector<double> weight(input.size());
  nlm_weights(guide,variance,patch,radius,k,[&](int p,int q,double w){weight[p]+=w;for(int c=0;c<input.channels;++c)out.at(p,c)+=w*input.at(q,c);});
  for(int p=0;p<input.size();++p)for(int c=0;c<input.channels;++c)out.at(p,c)/=weight[p];
  return out;
}
inline std::array<double,10> regression_row(const FilterImage& features,int centre,int q) {
  std::array<double,10> row{1,double(q/features.height-centre/features.height)/9,double(q%features.height-centre%features.height)/9};
  for(int k=0;k<7;++k)row[k+3]=features.at(q,k)-features.at(centre,k);
  return row;
}
// Cholesky with feature-scaled regularisation keeps constant features well posed.
inline void regression_solve(std::array<double,100>& a,std::array<double,30>& b) {
  double scale[10];for(int j=0;j<10;++j)scale[j]=std::sqrt(std::max(a[j*10+j],1e-20));
  for(int j=0;j<10;++j){for(int k=0;k<10;++k)a[j*10+k]/=scale[j]*scale[k];for(int c=0;c<3;++c)b[j*3+c]/=scale[j];a[j*10+j]+=j==0 ? 1e-12:1e-6;}
  for(int j=0;j<10;++j)for(int k=0;k<=j;++k){double value=a[j*10+k];for(int i=0;i<k;++i)value-=a[j*10+i]*a[k*10+i];a[j*10+k]=j==k ? std::sqrt(std::max(value,1e-12)):value/a[k*10+k];}
  for(int c=0;c<3;++c){for(int j=0;j<10;++j){for(int k=0;k<j;++k)b[j*3+c]-=a[j*10+k]*b[k*3+c];b[j*3+c]/=a[j*10+j];}
    for(int j=9;j>=0;--j){for(int k=j+1;k<10;++k)b[j*3+c]-=a[k*10+j]*b[k*3+c];b[j*3+c]/=a[j*10+j];}}
  for(int j=0;j<10;++j)for(int c=0;c<3;++c)b[j*3+c]/=scale[j];
}
inline FilterImage collaborative_regression(const FilterImage& colour,const FilterImage& features,const FilterImage& variance,double bandwidth) {
  int n=colour.size();FilterImage out(colour.width,colour.height,3);std::vector<double> weights(n);
  constexpr int block_size=4096;
  for(int begin=0;begin<n;begin+=block_size){int end=std::min(n,begin+block_size);
    std::vector<std::array<double,100>> a(end-begin);std::vector<std::array<double,30>> b(end-begin);
    nlm_weights(colour,variance,3,9,bandwidth,[&](int p,int q,double w){if(p<begin || p>=end || w<1e-10)return;
      auto x=regression_row(features,p,q);auto& aa=a[p-begin];auto& bb=b[p-begin];
      for(int j=0;j<10;++j){for(int k=0;k<=j;++k)aa[j*10+k]+=w*x[j]*x[k];for(int c=0;c<3;++c)bb[j*3+c]+=w*x[j]*colour.at(q,c);}},begin,end);
    for(int p=begin;p<end;++p){auto& aa=a[p-begin];for(int j=0;j<10;++j)for(int k=0;k<j;++k)aa[k*10+j]=aa[j*10+k];regression_solve(aa,b[p-begin]);}
    nlm_weights(colour,variance,3,9,bandwidth,[&](int p,int q,double w){if(p<begin || p>=end || w<1e-10)return;auto x=regression_row(features,p,q);weights[q]+=w;
      for(int c=0;c<3;++c){double fit=0;for(int j=0;j<10;++j)fit+=x[j]*b[p-begin][j*3+c];out.at(q,c)+=w*fit;}},begin,end);
    Rcpp::checkUserInterrupt();
  }
  for(int p=0;p<n;++p)for(int c=0;c<3;++c)out.at(p,c)=weights[p]>0 ? out.at(p,c)/weights[p]:colour.at(p,c);
  return out;
}
inline FilterImage nfor(const FilterImage& colourA,const FilterImage& colourB,const FilterImage& featureA,const FilterImage& featureB) {
  auto colour=image_mean(colourA,colourB),cv=half_variance(colourA,colourB),fv=half_variance(featureA,featureB);
  auto twice=[](FilterImage image){for(double& v:image.data)v*=2;return image;};
  FilterImage fa=featureA,fb=featureB;
  for(int k=0;k<7;++k){FilterImage a(colour.width,colour.height,1),b=a,var=a;
    for(int p=0;p<a.size();++p){a.at(p,0)=featureA.at(p,k);b.at(p,0)=featureB.at(p,k);var.at(p,0)=2*fv.at(p,k);}
    auto filteredA=nlmeans(a,b,var,3,5,.5),filteredB=nlmeans(b,a,var,3,5,.5);
    for(int p=0;p<a.size();++p){fa.at(p,k)=filteredA.at(p,0);fb.at(p,k)=filteredB.at(p,0);}}
  std::array<FilterImage,2> a,b,mse;
  for(int k=0;k<2;++k){a[k]=collaborative_regression(colourA,fb,twice(cv),k==0 ? .5:1);b[k]=collaborative_regression(colourB,fa,twice(cv),k==0 ? .5:1);
    FilterImage error=colour;for(std::size_t i=0;i<error.data.size();++i){double da=colourB.data[i]-a[k].data[i],db=colourA.data[i]-b[k].data[i],d=a[k].data[i]-b[k].data[i];
      error.data[i]=.5*(da*da+db*db)-2*cv.data[i]-.25*d*d;}mse[k]=nlmeans(error,colour,cv,1,9,1);}
  FilterImage select=colour;for(int p=0;p<select.size();++p){double ea=0,eb=0;for(int k=0;k<3;++k){ea+=mse[0].at(p,k);eb+=mse[1].at(p,k);}for(int k=0;k<3;++k)select.at(p,k)=ea<=eb ? 1:0;}
  select=nlmeans(select,colour,cv,1,9,1);FilterImage ra=colour,rb=colour;
  for(std::size_t i=0;i<ra.data.size();++i){double w=select.data[i];ra.data[i]=a[0].data[i]*w+a[1].data[i]*(1-w);rb.data[i]=b[0].data[i]*w+b[1].data[i]*(1-w);}
  auto ff=image_mean(fa,fb),ffv=half_variance(fa,fb);
  for(int k=0;k<7;++k){FilterImage v(colour.width,colour.height,1),var=v;for(int p=0;p<v.size();++p){v.at(p,0)=ff.at(p,k);var.at(p,0)=ffv.at(p,k);}
    auto filtered=nlmeans(v,v,var,3,2,.5);for(int p=0;p<v.size();++p)ff.at(p,k)=filtered.at(p,0);}
  return collaborative_regression(image_mean(ra,rb),ff,half_variance(ra,rb),1);
}
} }
