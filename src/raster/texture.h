#pragma once
#include <Rcpp.h>
#include <algorithm>
#include <array>
#include <cmath>
#include <cstdint>
#include <vector>

namespace ggplot3d { namespace raster {

// Premultiplied linear-light RGBA.
using Texel = std::array<double,4>;

// An image as a mipmap pyramid (Williams 1983) of premultiplied linear-light
// texels, level 0 the image itself, each further level averaging 2 x 2 texels
// of the previous one (the last row or column repeated where a side is odd),
// down to one texel. Texture coordinates u (left to right) and v (bottom to
// top) cover the image on [0, 1]; outside it the texture is transparent.
struct Texture {
  struct Level {
    int width=0,height=0;
    std::vector<Texel> texel;
    const Texel& at(int x,int y) const {
      x=std::max(0,std::min(width-1,x)); y=std::max(0,std::min(height-1,y));
      return texel[static_cast<std::size_t>(y)*width+x];
    }
  };
  std::vector<Level> levels;
  bool interpolate=true;

  // Bilinear sample of `level` at texture coordinates; rows run from the top.
  Texel bilinear(int level,double u,double v) const {
    const Level& l=levels[level];
    double x=u*l.width-.5,y=(1-v)*l.height-.5;
    double fx=std::floor(x),fy=std::floor(y);
    double ax=x-fx,ay=y-fy;
    int x0=static_cast<int>(fx),y0=static_cast<int>(fy);
    const Texel &a=l.at(x0,y0),&b=l.at(x0+1,y0),&c=l.at(x0,y0+1),&d=l.at(x0+1,y0+1);
    Texel out;
    for(int k=0;k<4;++k) out[k]=(1-ay)*((1-ax)*a[k]+ax*b[k])+ay*((1-ax)*c[k]+ax*d[k]);
    return out;
  }
  Texel nearest(int level,double u,double v) const {
    const Level& l=levels[level];
    return l.at(static_cast<int>(std::floor(u*l.width)),static_cast<int>(std::floor((1-v)*l.height)));
  }
  // Sample at (u, v) where one canvas sample spans `footprint` level-0
  // texels: nearest at level zero for sharp pixels, otherwise bilinear under
  // magnification and trilinear around log2(footprint) under minification.
  Texel sample(double u,double v,double footprint) const {
    if(!(u>=0 && u<=1 && v>=0 && v<=1)) return Texel{0,0,0,0};
    if(!interpolate) return nearest(0,u,v);
    double lod=footprint>0 && std::isfinite(footprint) ? std::log2(footprint):0;
    const int last=static_cast<int>(levels.size())-1;
    if(lod<=0 || last==0) return bilinear(0,u,v);
    lod=std::min(lod,static_cast<double>(last));
    int lower=static_cast<int>(std::floor(lod));
    double t=lod-lower;
    Texel a=bilinear(lower,u,v);
    if(t==0 || lower==last) return a;
    Texel b=bilinear(lower+1,u,v),out;
    for(int k=0;k<4;++k) out[k]=(1-t)*a[k]+t*b[k];
    return out;
  }
};

static inline double texture_decode(double v) {return v<=0.04045 ? v/12.92:std::pow((v+0.055)/1.055,2.4);}

// A texture from a nativeRaster (an integer matrix of packed 8-bit encoded
// sRGB channels R | G << 8 | B << 16 | A << 24, rows from the top).
static inline Texture read_texture(SEXP image,bool interpolate) {
  Rcpp::IntegerVector x(image);
  Rcpp::IntegerVector dim=x.attr("dim");
  if(dim.size()!=2 || dim[0]<1 || dim[1]<1 || static_cast<R_xlen_t>(dim[0])*dim[1]!=x.size())
    Rcpp::stop("invalid texture image");
  Texture t; t.interpolate=interpolate;
  std::array<double,256> decode;
  for(int k=0;k<256;++k) decode[k]=texture_decode(k/255.0);
  Texture::Level base; base.height=dim[0]; base.width=dim[1]; base.texel.resize(x.size());
  for(R_xlen_t i=0;i<x.size();++i) {
    std::uint32_t p=static_cast<std::uint32_t>(x[i]);
    double a=((p>>24)&255u)/255.0;
    base.texel[i]={decode[p&255u]*a,decode[(p>>8)&255u]*a,decode[(p>>16)&255u]*a,a};
  }
  t.levels.push_back(std::move(base));
  while(t.levels.back().width>1 || t.levels.back().height>1) {
    const Texture::Level& prev=t.levels.back();
    Texture::Level next; next.width=(prev.width+1)/2; next.height=(prev.height+1)/2;
    next.texel.resize(static_cast<std::size_t>(next.width)*next.height);
    for(int y=0;y<next.height;++y) for(int xx=0;xx<next.width;++xx) {
      Texel s{0,0,0,0};
      for(int dy=0;dy<2;++dy) for(int dx=0;dx<2;++dx) {
        const Texel& q=prev.at(2*xx+dx,2*y+dy);
        for(int k=0;k<4;++k) s[k]+=q[k]/4;
      }
      next.texel[static_cast<std::size_t>(y)*next.width+xx]=s;
    }
    t.levels.push_back(std::move(next));
  }
  return t;
}

} }
