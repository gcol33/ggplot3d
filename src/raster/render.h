#pragma once
#include "geometry.h"

namespace ggplot3d { namespace raster {
struct Colour { double r=0,g=0,b=0,a=0; };
struct Paint {
  Colour colour;
  std::vector<double> dash;
  double period=0;
  std::array<double,4> bounds;
};

static inline double edge(const Screen& a,const Screen& b,double x,double y) {
  return (a.x-x)*(b.y-y)-(a.y-y)*(b.x-x);
}
static inline bool edge_inside(double value,const Screen& a,const Screen& b,double orientation) {
  if(value!=0) return value>0;
  double dx=(b.x-a.x)*orientation,dy=(b.y-a.y)*orientation;
  return dy>0 || (dy==0 && dx<0);
}
static inline double depth_at(double a,double b,double t,bool perspective) {
  return perspective ? 1/((1-t)/a+t/b) : (1-t)*a+t*b;
}
static inline bool dash_on(const Paint& paint,double distance) {
  if(paint.dash.empty()) return false;
  if(!std::isfinite(paint.period)) return true;
  double phase=std::fmod(distance,paint.period);
  std::size_t k=0;
  while(k+1<paint.dash.size() && phase>=paint.dash[k]) phase-=paint.dash[k++];
  return k%2==0;
}
static inline bool footprint(const Scene& scene,Index row,double x,double y) {
  const auto& f=scene.packet.footprints;
  Index id=scene.packet.markers.footprint_id[row];
  double angle=f.angle[id]*pi/180,cs=std::cos(angle),sn=std::sin(angle);
  double u=cs*x+sn*y,v=-sn*x+cs*y;
  if(f.kind[id]==1) {
    if(f.radius_x_mm[id]==0 || f.radius_y_mm[id]==0) return false;
    return u*u/(f.radius_x_mm[id]*f.radius_x_mm[id])+v*v/(f.radius_y_mm[id]*f.radius_y_mm[id])<=1;
  }
  const auto& vertices=scene.packet.footprint_vertices;
  bool inside=false;
  Index first=f.offset[id],end=first+f.count[id];
  while(first<end) {
    Index stop=first+1;
    while(stop<end && vertices.ring[stop]==vertices.ring[first]) ++stop;
    for(Index i=first,j=stop-1;i<stop;j=i++) {
      double ax=vertices.x_mm[j],ay=vertices.y_mm[j],bx=vertices.x_mm[i],by=vertices.y_mm[i];
      if((ay>v)!=(by>v) && u<ax+(v-ay)*(bx-ax)/(by-ay)) inside=!inside;
    }
    first=stop;
  }
  return inside;
}

static inline double sample(const Scene& scene,const Item& item,const Paint& paint,double x,double y) {
  const Screen* v=scene.vertices.data()+item.offset;
  if(x<paint.bounds[0] || y<paint.bounds[1] || x>paint.bounds[2] || y>paint.bounds[3]) return R_PosInf;
  if(item.geometry && !dash_on(paint,item.path_start)) return R_PosInf;
  if(item.geometry==2) {
    double radius=scene.packet.polylines.width_mm[item.row]*.5;
    return std::hypot(x-v[0].x,y-v[0].y)<=radius ? v[0].depth:R_PosInf;
  }
  if(item.kind==1 || item.geometry==1) {
    for(Index i=1;i+1<item.count;++i) {
      double area=edge(v[0],v[i],v[i+1].x,v[i+1].y);
      if(area==0) continue;
      double a=edge(v[i],v[i+1],x,y)/area;
      double b=edge(v[i+1],v[0],x,y)/area;
      double c=edge(v[0],v[i],x,y)/area;
      double orientation=area>0 ? 1:-1;
      if(edge_inside(a,v[i],v[i+1],orientation) && edge_inside(b,v[i+1],v[0],orientation) &&
         edge_inside(c,v[0],v[i],orientation))
        return scene.projection.camera.perspective ? 1/(a/v[0].depth+b/v[i].depth+c/v[i+1].depth) :
          a*v[0].depth+b*v[i].depth+c*v[i+1].depth;
    }
  } else if(item.kind==2) {
    double dx=v[1].x-v[0].x,dy=v[1].y-v[0].y,length2=dx*dx+dy*dy;
    if(length2==0) return R_PosInf;
    double length=std::sqrt(length2),t=((x-v[0].x)*dx+(y-v[0].y)*dy)/length2;
    const auto& line=scene.packet.polylines;
    double radius=line.width_mm[item.row]*.5;
    double clamped=std::max(0.0,std::min(1.0,t));
    if((t<0 && (!item.cap_start || line.cap[item.row]==0)) ||
       (t>1 && (!item.cap_end || line.cap[item.row]==0))) return R_PosInf;
    if(line.cap[item.row]==2) clamped=std::max(-radius/length,std::min(1+radius/length,t));
    double ex=x-v[0].x-clamped*dx,ey=y-v[0].y-clamped*dy;
    if(ex*ex+ey*ey>radius*radius) return R_PosInf;
    if(!dash_on(paint,item.path_start+std::max(0.0,std::min(1.0,t))*length)) return R_PosInf;
    return depth_at(v[0].depth,v[1].depth,std::max(0.0,std::min(1.0,t)),scene.projection.camera.perspective);
  } else if(item.kind==3 && footprint(scene,item.row,x-v[0].x,y-v[0].y)) return v[0].depth;
  return R_PosInf;
}

static inline std::array<double,4> bounds(const Scene& scene,const Item& item) {
  const Screen* v=scene.vertices.data()+item.offset;
  std::array<double,4> b{v[0].x,v[0].y,v[0].x,v[0].y};
  for(Index i=1;i<item.count;++i) {
    b[0]=std::min(b[0],v[i].x); b[1]=std::min(b[1],v[i].y);
    b[2]=std::max(b[2],v[i].x); b[3]=std::max(b[3],v[i].y);
  }
  double radius=0;
  if(item.kind==2 && item.geometry!=1) radius=scene.packet.polylines.width_mm[item.row]*.5;
  if(item.kind==3) {
    const auto& f=scene.packet.footprints;
    Index id=scene.packet.markers.footprint_id[item.row];
    if(f.kind[id]==1) radius=std::max(f.radius_x_mm[id],f.radius_y_mm[id]);
    else for(Index j=f.offset[id];j<f.offset[id]+f.count[id];++j)
      radius=std::max(radius,std::hypot(scene.packet.footprint_vertices.x_mm[j],scene.packet.footprint_vertices.y_mm[j]));
  }
  b[0]-=radius; b[1]-=radius; b[2]+=radius; b[3]+=radius;
  return b;
}

// Higher partitions and coplanar paint keys are closer in compositing order.
static inline bool before(const Scene& scene,Index a,double za,Index b,double zb) {
  if(b<0) return true;
  const auto& ia=scene.items[a]; const auto& ib=scene.items[b];
  const auto& ta=scene.table(ia.kind); const auto& tb=scene.table(ib.kind);
  int pa=ta.partition[ia.row],pb=tb.partition[ib.row];
  if(pa!=pb) return pa>pb;
  if(pa==1 && std::abs(za-zb)>scene.projection.resolved.tolerance) return za<zb;
  return std::make_tuple(pa==1 ? ta.role[ia.row]:0,ta.layer_id[ia.row],ta.source_row[ia.row],ta.primitive_id[ia.row]) >
    std::make_tuple(pb==1 ? tb.role[ib.row]:0,tb.layer_id[ib.row],tb.source_row[ib.row],tb.primitive_id[ib.row]);
}

static inline std::vector<Paint> paints(const Scene& scene,Rcpp::List colours,Rcpp::List dashes,bool id_pass) {
  std::vector<Paint> out; out.reserve(scene.items.size());
  std::array<Rcpp::NumericMatrix,4> rgba{{Rcpp::as<Rcpp::NumericMatrix>(colours[0]),Rcpp::as<Rcpp::NumericMatrix>(colours[1]),
    Rcpp::as<Rcpp::NumericMatrix>(colours[2]),Rcpp::as<Rcpp::NumericMatrix>(colours[3])}};
  for(const auto& item:scene.items) {
    const auto& table=scene.table(item.kind); auto& c=rgba[item.kind-1];
    require(c.nrow()==4 && c.ncol()==static_cast<int>(table.count.size()),"invalid raster colour table");
    Paint p; p.bounds=bounds(scene,item);
    p.colour={c(0,item.row),c(1,item.row),c(2,item.row),(id_pass ? 1:c(3,item.row))*table.opacity[item.row]};
    if(item.kind==2) {
      p.dash=Rcpp::as<std::vector<double>>(dashes[item.row]);
      for(double value:p.dash) {require(value>0,"invalid raster dash length"); p.period+=value;}
    }
    out.push_back(std::move(p));
  }
  return out;
}

static inline Rcpp::List draw(SEXP pointer,Rcpp::List colours,Rcpp::List dashes,
                              int width,int height,int aa,bool id_pass) {
  auto start=Clock::now();
  Rcpp::XPtr<Scene> holder(pointer); const Scene& scene=*holder;
  require(width>0 && height>0 && aa>0,"raster dimensions and antialias must be positive");
  double sample_count=static_cast<double>(width)*height*aa*aa;
  require(sample_count<=static_cast<double>(std::numeric_limits<int>::max()) && sample_count*80<1073741824.0,
          "raster sample buffers exceed the 1 GiB budget; reduce dpi or antialias");
  int w=width*aa,h=height*aa; std::size_t n=static_cast<std::size_t>(w)*h;
  auto paint=paints(scene,colours,dashes,id_pass);
  std::vector<Colour> composite(n);
  std::vector<double> depth(n,R_PosInf),previous_depth(n,R_NegInf);
  std::vector<Index> hit(n,-1),previous(n,-1),visible(n,0);
  std::vector<unsigned char> done(n,0);
  double dx=scene.projection.viewport.width_mm/w,dy=scene.projection.viewport.height_mm/h;
  int passes=0;
  for(;;) {
    bool found=false;
    for(Index i=0;i<static_cast<Index>(scene.items.size());++i) {
      if(i%4096==0) Rcpp::checkUserInterrupt();
      const auto& item=scene.items[i]; const auto& p=paint[i];
      if(item.kind==4 || p.colour.a==0) continue;
      int x0=static_cast<int>(std::max(0.0,std::min(static_cast<double>(w),std::ceil(p.bounds[0]/dx-.5))));
      int y0=static_cast<int>(std::max(0.0,std::min(static_cast<double>(h),std::ceil(p.bounds[1]/dy-.5))));
      int x1=static_cast<int>(std::max(-1.0,std::min(static_cast<double>(w-1),std::floor(p.bounds[2]/dx-.5))));
      int y1=static_cast<int>(std::max(-1.0,std::min(static_cast<double>(h-1),std::floor(p.bounds[3]/dy-.5))));
      for(int y=y0;y<=y1;++y) for(int x=x0;x<=x1;++x) {
        std::size_t pixel=static_cast<std::size_t>(y)*w+x;
        if(done[pixel]) continue;
        double z=sample(scene,item,p,(x+.5)*dx,(y+.5)*dy);
        if(!std::isfinite(z)) continue;
        if(previous[pixel]>=0) {
          const auto& prev=scene.items[previous[pixel]];
          if(prev.kind==item.kind && prev.row==item.row) continue;
          if(!before(scene,previous[pixel],previous_depth[pixel],i,z)) continue;
        }
        if(before(scene,i,z,hit[pixel],depth[pixel])) { hit[pixel]=i; depth[pixel]=z; found=true; }
      }
    }
    if(!found) break;
    ++passes; bool more=false;
    for(std::size_t pixel=0;pixel<n;++pixel) {
      if(hit[pixel]<0) {done[pixel]=1;continue;}
      Index i=hit[pixel]; const auto& c=paint[i].colour; auto& out=composite[pixel];
      double weight=(1-out.a)*c.a;
      out.r+=weight*c.r; out.g+=weight*c.g; out.b+=weight*c.b; out.a+=weight;
      if(passes==1) visible[pixel]=scene.table(scene.items[i].kind).primitive_id[scene.items[i].row];
      done[pixel]=c.a==1;
      more=more || !done[pixel];
      previous[pixel]=i; previous_depth[pixel]=depth[pixel]; hit[pixel]=-1; depth[pixel]=R_PosInf;
    }
    if(!more) break;
  }
  Rcpp::IntegerMatrix raster(height,width),ids(height,width);
  auto byte=[](double value) { return static_cast<std::uint32_t>(std::round(std::max(0.0,std::min(1.0,value))*255)); };
  for(int y=0;y<height;++y) for(int x=0;x<width;++x) {
    Colour sum;
    for(int sy=0;sy<aa;++sy) for(int sx=0;sx<aa;++sx) {
      const auto& c=composite[static_cast<std::size_t>(y*aa+sy)*w+x*aa+sx];
      sum.r+=c.r;sum.g+=c.g;sum.b+=c.b;sum.a+=c.a;
    }
    double alpha=sum.a/(static_cast<double>(aa)*aa);
    std::uint32_t packed=byte(alpha)<<24;
    if(sum.a>0) packed|=byte(sum.r/sum.a)|(byte(sum.g/sum.a)<<8)|(byte(sum.b/sum.a)<<16);
    raster[static_cast<R_xlen_t>(height-1-y)*width+x]=static_cast<int>(packed);
    ids(height-1-y,x)=visible[static_cast<std::size_t>(y*aa+aa/2)*w+x*aa+aa/2];
  }
  raster.attr("class")="nativeRaster";
  std::vector<int> text_rows; std::vector<double> text_x,text_y;
  for(Index i=0;i<static_cast<Index>(scene.items.size());++i) {
    const auto& item=scene.items[i]; if(item.kind!=4 || paint[i].colour.a==0) continue;
    const auto& anchor=scene.vertices[item.offset]; bool hidden=false;
    if(scene.packet.text.partition[item.row]==1) for(Index j=0;j<static_cast<Index>(scene.items.size());++j) {
      if(scene.items[j].kind==4 || paint[j].colour.a<1 || scene.table(scene.items[j].kind).partition[scene.items[j].row]!=1) continue;
      double z=sample(scene,scene.items[j],paint[j],anchor.x,anchor.y);
      if(std::isfinite(z) && before(scene,j,z,i,anchor.depth)) {hidden=true;break;}
    }
    if(!hidden) {text_rows.push_back(item.row+1);text_x.push_back(anchor.x/scene.projection.viewport.width_mm);text_y.push_back(anchor.y/scene.projection.viewport.height_mm);}
  }
  return Rcpp::List::create(Rcpp::Named("image")=raster,Rcpp::Named("id")=ids,
    Rcpp::Named("text")=Rcpp::List::create(Rcpp::Named("row")=text_rows,Rcpp::Named("x")=text_x,Rcpp::Named("y")=text_y),
    Rcpp::Named("passes")=passes,Rcpp::Named("raster_seconds")=seconds(start),
    Rcpp::Named("width")=width,Rcpp::Named("height")=height,Rcpp::Named("antialias")=aa);
}
} }
