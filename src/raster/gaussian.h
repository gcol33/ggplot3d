#pragma once
#include "../scene/gaussian.h"

namespace ggplot3d { namespace raster {
struct GaussianFootprint {
  gaussian::Projected projected;
  Colour colour;
  double a=0,b=0,c=0,rx=0,ry=0;
  std::array<int,4> tiles{0,0,-1,-1};
  double alpha(double x,double y) const {
    x-=projected.x;y-=projected.y;
    if(std::abs(x)>rx || std::abs(y)>ry)return 0;
    double q=a*x*x+2*b*x*y+c*y*y;
    return q<=9 ? colour.a*std::exp(-.5*q):0;
  }
};
struct GaussianTiles {
  std::vector<GaussianFootprint> footprints;
  std::vector<std::size_t> start;
  std::vector<int> indices;
  GaussianTiles(const Rcpp::List& specs,const engine::Projection& p,const Canvas& canvas,int threads) {
    for(SEXP spec:specs){auto source=gaussian::read(Rcpp::List(spec),p);std::size_t offset=footprints.size();footprints.resize(offset+source.size());
      engine::parallel_blocks((source.size()+1023)/1024,std::max(1,threads),[&](std::size_t block){
        for(std::size_t i=block*1024;i<std::min(source.size(),(block+1)*1024);++i){const auto& g=source[i];auto& f=footprints[offset+i];
          f.projected=gaussian::project(g,p,canvas.pixel_x,canvas.pixel_y);const auto& v=f.projected;
          if(!std::isfinite(v.x+v.y+v.xx+v.xy+v.yy) || v.z<p.resolved.near || v.z>p.resolved.far || g.rgba[3]<=0)continue;
          if(p.camera.clip_data && (g.centre[0]<0 || g.centre[1]<0 || g.centre[2]<0 || g.centre[0]>p.scale[0] || g.centre[1]>p.scale[1] || g.centre[2]>p.scale[2]))continue;
          double det=v.xx*v.yy-v.xy*v.xy;if(!(det>0))continue;
          f.a=v.yy/det;f.b=-v.xy/det;f.c=v.xx/det;f.rx=3*std::sqrt(v.xx);f.ry=3*std::sqrt(v.yy);
          f.colour={g.rgba[0],g.rgba[1],g.rgba[2],g.rgba[3]};if(canvas.linear)f.colour=linear_colour(f.colour);
          auto bounds=canvas.samples({(v.x-f.rx)*canvas.pixel_x,(v.y-f.ry)*canvas.pixel_y,
            (v.x+f.rx)*canvas.pixel_x,(v.y+f.ry)*canvas.pixel_y});
          if(bounds[0]<=bounds[2] && bounds[1]<=bounds[3]){int ts=canvas.tile_samples();f.tiles={bounds[0]/ts,bounds[1]/ts,bounds[2]/ts,bounds[3]/ts};}
        }});
    }
    start.assign(std::size_t(canvas.tiles_x)*canvas.tiles_y+1,0);
    auto each=[&](const GaussianFootprint& f,auto fn){for(int y=f.tiles[1];y<=f.tiles[3];++y)for(int x=f.tiles[0];x<=f.tiles[2];++x)fn(y*canvas.tiles_x+x);};
    for(const auto& f:footprints)each(f,[&](int t){++start[t+1];});
    for(std::size_t i=1;i<start.size();++i)start[i]+=start[i-1];
    engine::require(start.back()<=std::size_t(1)<<28,"Gaussian tile references exceed the memory budget");
    indices.resize(start.back());auto cursor=start;
    for(std::size_t i=0;i<footprints.size();++i)each(footprints[i],[&](int t){indices[cursor[t]++]=i;});
    engine::parallel_blocks(start.size()-1,std::max(1,threads),[&](std::size_t t){std::sort(indices.begin()+start[t],indices.begin()+start[t+1],[&](int a,int b){
      double za=footprints[a].projected.z,zb=footprints[b].projected.z;return za==zb ? a<b:za<zb;});});
  }
};
} }
