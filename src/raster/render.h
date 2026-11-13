#pragma once
#include "geometry.h"
#include <numeric>
#include <atomic>
#include <mutex>
#include <thread>
#include <R_ext/Utils.h>

namespace ggplot3d { namespace raster {
struct Colour { double r=0,g=0,b=0,a=0; };
struct Paint {
  Colour colour,outline;
  std::vector<double> dash,hidden_dash;
  double period=0;
  double width=0;
  bool separate=false;
  std::array<double,4> bounds;
};

// Per-draw constants shared read-only by all tiles: the scene, the paints,
// marker footprint rotations and the fan-triangle areas of every polygon item.
struct Shapes {
  const Scene& scene;
  const std::vector<Paint>& paint;
  std::vector<std::array<double,2>> rotation;
  std::vector<double> area;
};

static inline double edge(const Screen& a,const Screen& b,double x,double y) {
  // A shared edge uses one evaluation order, including under fused arithmetic.
  bool reverse=std::make_tuple(a.x,a.y)>std::make_tuple(b.x,b.y);
  const Screen& first=reverse ? b:a;
  const Screen& last=reverse ? a:b;
  double value=(first.x-x)*(last.y-y)-(first.y-y)*(last.x-x);
  return reverse ? -value:value;
}
static inline bool edge_inside(double value,const Screen& a,const Screen& b,double orientation) {
  if(value!=0) return value>0;
  double dx=(b.x-a.x)*orientation,dy=(b.y-a.y)*orientation;
  return dy>0 || (dy==0 && dx<0);
}
static inline double depth_at(double a,double b,double t,bool perspective) {
  return perspective ? 1/((1-t)/a+t/b) : (1-t)*a+t*b;
}
static inline bool dash_on(const Paint& paint,double distance,bool hidden=false) {
  const auto& dash=hidden ? paint.hidden_dash:paint.dash;
  if(dash.empty()) return false;
  double period=hidden ? std::accumulate(dash.begin(),dash.end(),0.0):paint.period;
  if(!std::isfinite(period)) return true;
  double phase=std::fmod(distance,period);
  std::size_t k=0;
  while(k+1<dash.size() && phase>=dash[k]) phase-=dash[k++];
  return k%2==0;
}
static inline bool footprint(const Shapes& shapes,Index row,double x,double y,double inset=0) {
  const auto& packet=shapes.scene.packet;
  const auto& f=packet.footprints;
  Index id=packet.markers.footprint_id[row];
  double cs=shapes.rotation[id][0],sn=shapes.rotation[id][1];
  double u=cs*x+sn*y,v=-sn*x+cs*y;
  if(f.kind[id]==1) {
    double rx=f.radius_x_mm[id]-inset,ry=f.radius_y_mm[id]-inset;
    if(rx<=0 || ry<=0) return false;
    return u*u/(rx*rx)+v*v/(ry*ry)<=1;
  }
  const auto& vertices=packet.footprint_vertices;
  bool inside=false;
  Index first=f.offset[id],end=first+f.count[id];
  while(first<end) {
    Index stop=first+1;
    while(stop<end && vertices.ring[stop]==vertices.ring[first]) ++stop;
    for(Index i=first,j=stop-1;i<stop;j=i++) {
      double ax=vertices.x_mm[j],ay=vertices.y_mm[j],bx=vertices.x_mm[i],by=vertices.y_mm[i];
      double dx=bx-ax,dy=by-ay,length=dx*dx+dy*dy;
      double t=length>0 ? std::max(0.0,std::min(1.0,((u-ax)*dx+(v-ay)*dy)/length)):0;
      if(inset>0 && std::hypot(u-ax-t*dx,v-ay-t*dy)<inset) return false;
      if((ay>v)!=(by>v) && u<ax+(v-ay)*(bx-ax)/(by-ay)) inside=!inside;
    }
    first=stop;
  }
  return inside;
}

// Depth of an item's paint at a canvas point, or +Inf where it paints nothing.
static inline double sample(const Shapes& shapes,const Item& item,const Paint& paint,double x,double y,
                            bool* border=nullptr,bool hidden=false,bool ignore_dash=false) {
  const Scene& scene=shapes.scene;
  const Screen* v=scene.vertices.data()+item.offset;
  if(x<paint.bounds[0] || y<paint.bounds[1] || x>paint.bounds[2] || y>paint.bounds[3]) return R_PosInf;
  if(border) *border=false;
  if(item.geometry>0 && item.geometry<3 && item.kind==2 && !ignore_dash && !dash_on(paint,item.path_start,hidden)) return R_PosInf;
  if((item.kind==1 || item.geometry>=3) && paint.outline.a>0 && paint.width>0) {
    for(Index j=0;j<item.count-(item.geometry==4 ? 1:0);++j) {
      const auto& a=v[j];const auto& b=v[(j+1)%item.count];
      double dx=b.x-a.x,dy=b.y-a.y,length=dx*dx+dy*dy;
      double t=length>0 ? std::max(0.0,std::min(1.0,((x-a.x)*dx+(y-a.y)*dy)/length)):0;
      if(std::hypot(x-a.x-t*dx,y-a.y-t*dy)<=paint.width*.5) {
        if(border) *border=true;
        return depth_at(a.depth,b.depth,t,scene.projection.camera.perspective);
      }
    }
  }
  if(item.geometry==2) {
    double radius=scene.packet.polylines.width_mm[item.row]*.5;
    return std::hypot(x-v[0].x,y-v[0].y)<=radius ? v[0].depth:R_PosInf;
  }
  if(item.geometry==4) return R_PosInf;
  if(item.kind==1 || item.geometry==1 || item.geometry==3) {
    const double* areas=shapes.area.data()+item.offset;
    for(Index i=1;i+1<item.count;++i) {
      double area=areas[i];
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
    if(!ignore_dash && !dash_on(paint,item.path_start+std::max(0.0,std::min(1.0,t))*length,hidden)) return R_PosInf;
    return depth_at(v[0].depth,v[1].depth,std::max(0.0,std::min(1.0,t)),scene.projection.camera.perspective);
  } else if(item.kind==3 && footprint(shapes,item.row,x-v[0].x,y-v[0].y)) {
    if(border) *border=paint.separate && !footprint(shapes,item.row,x-v[0].x,y-v[0].y,paint.width);
    return v[0].depth;
  }
  return R_PosInf;
}

static inline Shapes shapes_of(const Scene& scene,const std::vector<Paint>& paint) {
  Shapes shapes{scene,paint,{},{}};
  const auto& f=scene.packet.footprints;
  shapes.rotation.resize(f.kind.size());
  for(std::size_t id=0;id<f.kind.size();++id) {
    double angle=f.angle[id]*pi/180;
    shapes.rotation[id]={std::cos(angle),std::sin(angle)};
  }
  shapes.area.assign(scene.vertices.size(),0);
  for(const auto& item:scene.items) {
    const Screen* v=scene.vertices.data()+item.offset;
    for(Index i=1;i+1<item.count;++i) shapes.area[item.offset+i]=edge(v[0],v[i],v[i+1].x,v[i+1].y);
  }
  return shapes;
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
  if(ia.kind==2 && ib.kind==2 && ia.row==ib.row && (ia.geometry>=3)!=(ib.geometry>=3))
    return ia.geometry>=3;
  return std::make_tuple(pa==1 ? ta.role[ia.row]:0,ta.layer_id[ia.row],ta.source_row[ia.row],ta.primitive_id[ia.row]) >
    std::make_tuple(pb==1 ? tb.role[ib.row]:0,tb.layer_id[ib.row],tb.source_row[ib.row],tb.primitive_id[ib.row]);
}

static inline std::vector<Paint> paints(const Scene& scene,Rcpp::List colours,Rcpp::List dashes,bool id_pass) {
  struct Table {
    Rcpp::NumericMatrix colour,outline; Rcpp::NumericVector width; Rcpp::LogicalVector separate; bool styled=false;
  };
  std::array<Table,4> tables;
  for(int kind=1;kind<=4;++kind) {
    Table& t=tables[kind-1]; t.colour=Rcpp::as<Rcpp::NumericMatrix>(colours[kind-1]);
    require(t.colour.nrow()==4 && t.colour.ncol()==static_cast<int>(scene.table(kind).count.size()),"invalid raster colour table");
    t.styled=t.colour.hasAttribute("outline");
    if(t.styled) {
      t.outline=Rcpp::as<Rcpp::NumericMatrix>(t.colour.attr("outline"));
      t.width=Rcpp::as<Rcpp::NumericVector>(t.colour.attr("width_mm"));
      t.separate=Rcpp::as<Rcpp::LogicalVector>(t.colour.attr("separate"));
    }
  }
  std::vector<std::vector<double>> dash(dashes.size()),hidden_dash(dashes.size());
  Rcpp::List hidden=dashes.hasAttribute("hidden") ? Rcpp::List(dashes.attr("hidden")):Rcpp::List();
  for(R_xlen_t row=0;row<dashes.size();++row) {
    dash[row]=Rcpp::as<std::vector<double>>(dashes[row]);
    for(double value:dash[row]) require(value>0,"invalid raster dash length");
    if(hidden.size()) hidden_dash[row]=Rcpp::as<std::vector<double>>(hidden[row]);
  }
  std::vector<Paint> out; out.reserve(scene.items.size());
  for(const auto& item:scene.items) {
    const auto& table=scene.table(item.kind); const Table& t=tables[item.kind-1]; const auto& c=t.colour;
    Paint p; p.bounds=bounds(scene,item);
    p.colour={c(0,item.row),c(1,item.row),c(2,item.row),(id_pass ? 1:c(3,item.row))*table.opacity[item.row]};
    if(t.styled) {
      p.outline={t.outline(0,item.row),t.outline(1,item.row),t.outline(2,item.row),t.outline(3,item.row)*table.opacity[item.row]};
      p.width=t.width[item.row];p.separate=t.separate[item.row];
      if(item.kind==1 && p.outline.a>0) {p.bounds[0]-=p.width*.5;p.bounds[1]-=p.width*.5;p.bounds[2]+=p.width*.5;p.bounds[3]+=p.width*.5;}
    }
    if(item.kind==2) {
      p.dash=dash[item.row];
      for(double value:p.dash) p.period+=value;
      p.hidden_dash=hidden_dash[item.row];
    }
    out.push_back(std::move(p));
  }
  return out;
}

static inline void add_arrowheads(Scene& scene,std::vector<Paint>& paint,Rcpp::NumericMatrix line_colours,std::size_t items) {
  if(!line_colours.hasAttribute("arrows")) return;
  Rcpp::NumericMatrix arrows=line_colours.attr("arrows"),fill=line_colours.attr("arrow_fill");
  for(std::size_t i=0;i<items;++i) {
    Item item=scene.items[i];
    if(item.kind!=2 || item.geometry!=0 || arrows(item.row,0)<=0) continue;
    for(bool start:{true,false}) {
      if(!(start ? item.cap_start && scene.packet.polylines.arrow_start[item.row]:
                   item.cap_end && scene.packet.polylines.arrow_end[item.row])) continue;
      Screen tip=scene.vertices[item.offset+(start ? 0:1)],other=scene.vertices[item.offset+(start ? 1:0)];
      double dx=tip.x-other.x,dy=tip.y-other.y,length=std::hypot(dx,dy);
      if(length==0) continue;
      dx/=length;dy/=length;
      double along=arrows(item.row,0)*std::cos(arrows(item.row,1)),across=arrows(item.row,0)*std::sin(arrows(item.row,1));
      Item head=item;head.offset=scene.vertices.size();head.count=3;head.geometry=arrows(item.row,2)==2 ? 3:4;
      scene.vertices.push_back({tip.x-dx*along-dy*across,tip.y-dy*along+dx*across,tip.depth});
      scene.vertices.push_back(tip);
      scene.vertices.push_back({tip.x-dx*along+dy*across,tip.y-dy*along-dx*across,tip.depth});
      scene.items.push_back(head);
      Paint material=paint[i];material.outline=material.colour;
      material.colour={fill(0,item.row),fill(1,item.row),fill(2,item.row),fill(3,item.row)*scene.packet.polylines.opacity[item.row]};
      material.width=scene.packet.polylines.width_mm[item.row];material.bounds=bounds(scene,head);
      paint.push_back(std::move(material));
    }
  }
}

// Sample grid of the canvas: sample centres sit at origin + (index + 0.5) * step.
struct Canvas {
  double origin_x=0,origin_y=0,dx=1,dy=1;
  int width=0,height=0,aa=1,w=0,h=0;
  int tile=64,tiles_x=0,tiles_y=0;
  std::array<int,4> samples(const std::array<double,4>& b) const {
    return {static_cast<int>(std::max(0.0,std::min(static_cast<double>(w),std::ceil((b[0]-origin_x)/dx-.5)))),
      static_cast<int>(std::max(0.0,std::min(static_cast<double>(h),std::ceil((b[1]-origin_y)/dy-.5)))),
      static_cast<int>(std::max(-1.0,std::min(static_cast<double>(w-1),std::floor((b[2]-origin_x)/dx-.5)))),
      static_cast<int>(std::max(-1.0,std::min(static_cast<double>(h-1),std::floor((b[3]-origin_y)/dy-.5))))};
  }
  int tile_samples() const {return tile*aa;}
};

// Static per-item facts used by the tile loop.
struct Span {
  std::array<int,4> samples;
  double nearest;
  int partition;
  bool lift,occluder,retained_occluder;
};

// A painted sample: the item, its depth, and whether the outline paints it.
struct Fragment { Index item; double z; int sample; bool border; };

// The nearest opaque coverage of a sample: its partition and, within the depth
// partition, its depth. A fragment in a lower partition, or more than the
// tolerance behind it in the depth partition, follows it in compositing order.
struct Occluder {
  int partition=-1; double z=R_PosInf;
  bool hides(int p,double depth,double tolerance) const {
    return p<partition || (p==1 && partition==1 && depth>z+tolerance);
  }
  void add(int p,double depth) {
    if(p>partition) {partition=p;z=depth;}
    else if(p==partition && depth<z) z=depth;
  }
};

struct SampleResult {
  Colour colour;
  Index first=-1;
  double depth=R_PosInf,opaque_depth=R_PosInf;
  int peels=0;
};

// Front-to-back compositing order of the fragments of one sample: compositing
// order, then item index among fragments of one primitive.
static inline bool composite_order(const Scene& scene,const Fragment& a,const Fragment& b) {
  if(before(scene,a.item,a.z,b.item,b.z)) return true;
  if(before(scene,b.item,b.z,a.item,a.z)) return false;
  return a.item<b.item;
}
static inline void order_fragments(const Scene& scene,Fragment* f,std::size_t k,std::vector<Fragment>& buffer) {
  auto less=[&](const Fragment& a,const Fragment& b){return composite_order(scene,a,b);};
  if(k<=16) {
    for(std::size_t i=1;i<k;++i) {
      Fragment x=f[i]; std::size_t j=i;
      while(j>0 && less(x,f[j-1])) {f[j]=f[j-1];--j;}
      f[j]=x;
    }
    return;
  }
  buffer.resize(k);
  Fragment* from=f; Fragment* to=buffer.data();
  for(std::size_t width=1;width<k;width*=2) {
    for(std::size_t lo=0;lo<k;lo+=2*width) {
      std::size_t mid=std::min(lo+width,k),hi=std::min(lo+2*width,k),a=lo,b=mid,o=lo;
      while(a<mid && b<hi) to[o++]=less(from[b],from[a]) ? from[b++]:from[a++];
      while(a<mid) to[o++]=from[a++];
      while(b<hi) to[o++]=from[b++];
    }
    std::swap(from,to);
  }
  if(from!=f) std::copy(from,from+k,f);
}

// Walks one sample's fragments in compositing order. A primitive paints a
// sample once in succession and a translucent stroke once in total; the walk
// stops when the accumulated coverage is opaque.
static inline SampleResult composite_sample(const Shapes& shapes,const Fragment* f,std::size_t k,
                                            bool translucent_strokes,std::vector<Index>& painted) {
  const Scene& scene=shapes.scene;
  SampleResult out; painted.clear();
  Index previous=-1; double previous_z=R_NegInf;
  for(std::size_t j=0;j<k;++j) {
    const Item& item=scene.items[f[j].item];
    if(translucent_strokes && item.kind==2 && std::find(painted.begin(),painted.end(),item.row)!=painted.end()) continue;
    if(previous>=0) {
      const Item& prev=scene.items[previous];
      if(prev.kind==item.kind && prev.row==item.row) continue;
      if(!before(scene,previous,previous_z,f[j].item,f[j].z)) continue;
    }
    const Paint& p=shapes.paint[f[j].item];
    const Colour& c=f[j].border ? p.outline:p.colour;
    if(translucent_strokes && item.kind==2) painted.push_back(item.row);
    double weight=(1-out.colour.a)*c.a;
    out.colour.r+=weight*c.r; out.colour.g+=weight*c.g; out.colour.b+=weight*c.b; out.colour.a+=weight;
    if(++out.peels==1) {out.first=f[j].item;out.depth=f[j].z;}
    if(c.a==1) {out.opaque_depth=f[j].z;break;}
    previous=f[j].item; previous_z=f[j].z;
  }
  return out;
}

struct Buffers {
  std::vector<std::uint32_t> image;
  std::vector<int> id;
  std::vector<Index> item;
  std::vector<double> depth,opaque_depth;
  int passes=0;
};

struct TileScratch {
  std::vector<Fragment> fragments,ordered,merge;
  std::vector<Occluder> occluders;
  std::vector<double> opaque;
  std::vector<int> start;
  std::vector<SampleResult> results;
  std::vector<Index> order,painted;
};

static inline std::uint32_t channel_byte(double value) {
  return static_cast<std::uint32_t>(std::round(std::max(0.0,std::min(1.0,value))*255));
}

static inline void render_tile(const Shapes& shapes,const Canvas& canvas,const std::vector<Span>& spans,
                               const Index* tile_items,std::size_t tile_count,int tx,int ty,
                               bool retained,bool translucent_strokes,bool keep_buffers,
                               TileScratch& s,Buffers& out,int& passes) {
  const Scene& scene=shapes.scene;
  const auto& line=scene.packet.polylines;
  const double tolerance=scene.projection.resolved.tolerance;
  const int ts=canvas.tile_samples();
  const int sx0=tx*ts,sy0=ty*ts,sx1=std::min(canvas.w,sx0+ts),sy1=std::min(canvas.h,sy0+ts);
  const int tw=sx1-sx0,th=sy1-sy0; const std::size_t n=static_cast<std::size_t>(tw)*th;
  s.fragments.clear(); s.occluders.assign(n,Occluder());
  if(retained) s.opaque.assign(n,R_PosInf);
  s.order.assign(tile_items,tile_items+tile_count);
  std::sort(s.order.begin(),s.order.end(),[&](Index a,Index b) {
    const Span& x=spans[a]; const Span& y=spans[b];
    if(x.retained_occluder!=y.retained_occluder) return x.retained_occluder;
    if(x.partition!=y.partition) return x.partition>y.partition;
    if(x.nearest!=y.nearest) return x.nearest<y.nearest;
    return a<b;
  });
  for(Index i:s.order) {
    const Item& item=scene.items[i]; const Paint& p=shapes.paint[i]; const Span& span=spans[i];
    int x0=std::max(span.samples[0],sx0),y0=std::max(span.samples[1],sy0);
    int x1=std::min(span.samples[2],sx1-1),y1=std::min(span.samples[3],sy1-1);
    for(int y=y0;y<=y1;++y) for(int x=x0;x<=x1;++x) {
      int local=(y-sy0)*tw+(x-sx0);
      Occluder& occluder=s.occluders[local];
      if(!span.lift && occluder.hides(span.partition,span.nearest,tolerance)) continue;
      bool border=false;double xx=canvas.origin_x+(x+.5)*canvas.dx,yy=canvas.origin_y+(y+.5)*canvas.dy;
      double z=sample(shapes,item,p,xx,yy,&border,false,span.lift);
      if(span.retained_occluder) s.opaque[local]=std::min(s.opaque[local],z);
      if(span.lift && std::isfinite(z)) {
        bool hidden=z>s.opaque[local]+tolerance;
        z=sample(shapes,item,p,xx,yy,&border,hidden && line.hidden[item.row]==1);
        if(hidden && std::isfinite(z)) z=s.opaque[local];
      }
      if(!std::isfinite(z)) continue;
      const Colour& c=border ? p.outline:p.colour;
      if(c.a==0 || occluder.hides(span.partition,z,tolerance)) continue;
      s.fragments.push_back({i,z,local,border});
      if(span.occluder && c.a==1) occluder.add(span.partition,z);
    }
  }
  s.start.assign(n+1,0);
  for(const auto& f:s.fragments) ++s.start[f.sample+1];
  for(std::size_t k=0;k<n;++k) s.start[k+1]+=s.start[k];
  s.ordered.resize(s.fragments.size());
  {
    std::vector<int> cursor(s.start.begin(),s.start.end()-1);
    for(const auto& f:s.fragments) s.ordered[cursor[f.sample]++]=f;
  }
  s.results.assign(n,SampleResult());
  for(std::size_t k=0;k<n;++k) {
    Fragment* f=s.ordered.data()+s.start[k];
    std::size_t count=s.start[k+1]-s.start[k],kept=0;
    for(std::size_t j=0;j<count;++j)
      if(!s.occluders[k].hides(spans[f[j].item].partition,f[j].z,tolerance)) f[kept++]=f[j];
    order_fragments(scene,f,kept,s.merge);
    s.results[k]=composite_sample(shapes,f,kept,translucent_strokes,s.painted);
    passes=std::max(passes,s.results[k].peels);
  }
  const int aa=canvas.aa;
  for(int py=sy0/aa;py<sy1/aa;++py) for(int px=sx0/aa;px<sx1/aa;++px) {
    Colour sum;
    for(int sy=0;sy<aa;++sy) for(int sx=0;sx<aa;++sx) {
      const auto& c=s.results[static_cast<std::size_t>(py*aa+sy-sy0)*tw+px*aa+sx-sx0].colour;
      sum.r+=c.r;sum.g+=c.g;sum.b+=c.b;sum.a+=c.a;
    }
    double alpha=sum.a/(static_cast<double>(aa)*aa);
    std::uint32_t packed=channel_byte(alpha)<<24;
    if(sum.a>0) packed|=channel_byte(sum.r/sum.a)|(channel_byte(sum.g/sum.a)<<8)|(channel_byte(sum.b/sum.a)<<16);
    std::size_t pixel=static_cast<std::size_t>(canvas.height-1-py)*canvas.width+px;
    const SampleResult& centre=s.results[static_cast<std::size_t>(py*aa+aa/2-sy0)*tw+px*aa+aa/2-sx0];
    out.image[pixel]=packed;
    out.id[pixel]=centre.first<0 ? 0:scene.table(scene.items[centre.first].kind).primitive_id[scene.items[centre.first].row];
    if(keep_buffers) {
      out.item[pixel]=centre.first;out.depth[pixel]=centre.depth;out.opaque_depth[pixel]=centre.opaque_depth;
    }
  }
}

static inline void check_interrupt(void*) {R_CheckUserInterrupt();}
static inline bool interrupt_pending() {return R_ToplevelExec(check_interrupt,nullptr)==FALSE;}

// Runs every tile once on `threads` workers. Workers never call the R API;
// the calling thread takes tiles too and polls for user interrupts.
template<class Work>
static inline void parallel_tiles(int tiles,int threads,Work work) {
  std::atomic<int> next(0); std::atomic<bool> stop(false);
  std::mutex mutex; std::string failure;
  auto run=[&](int worker,bool main) {
    for(;;) {
      if(stop.load()) return;
      int tile=next.fetch_add(1);
      if(tile>=tiles) return;
      try {work(worker,tile);}
      catch(const std::exception& e) {std::lock_guard<std::mutex> lock(mutex);if(failure.empty()) failure=e.what();stop=true;return;}
      if(main && interrupt_pending()) {stop=true;std::lock_guard<std::mutex> lock(mutex);if(failure.empty()) failure="\x01";return;}
    }
  };
  std::vector<std::thread> pool;
  for(int t=1;t<threads;++t) pool.emplace_back(run,t,false);
  run(0,true);
  for(auto& t:pool) t.join();
  if(failure=="\x01") throw Rcpp::internal::InterruptedException();
  require(failure.empty(),failure);
}

static inline int thread_count(int requested,int tiles) {
  int threads=requested>0 ? requested:static_cast<int>(std::max(1u,std::thread::hardware_concurrency()));
  return std::max(1,std::min(threads,tiles));
}

static inline Rcpp::List draw(SEXP pointer,Rcpp::List colours,Rcpp::List dashes,
                              int width,int height,int aa,bool id_pass,int threads,bool keep_buffers) {
  auto start=Clock::now();
  Rcpp::XPtr<Scene> holder(pointer); Scene& scene=*holder;
  struct Restore {
    Scene& scene;std::size_t items,vertices;
    ~Restore() {scene.items.resize(items);scene.vertices.resize(vertices);}
  } restore{scene,scene.items.size(),scene.vertices.size()};
  require(width>0 && height>0 && aa>0,"raster dimensions and antialias must be positive");
  auto paint=paints(scene,colours,dashes,id_pass);
  add_arrowheads(scene,paint,colours[1],restore.items);
  Canvas canvas;
  double pixel_x=scene.projection.viewport.width_mm/width,pixel_y=scene.projection.viewport.height_mm/height;
  if(!scene.projection.camera.clip) {
    double xmax=scene.projection.viewport.width_mm,ymax=scene.projection.viewport.height_mm;
    for(const auto& a:paint) {canvas.origin_x=std::min(canvas.origin_x,a.bounds[0]);canvas.origin_y=std::min(canvas.origin_y,a.bounds[1]);xmax=std::max(xmax,a.bounds[2]);ymax=std::max(ymax,a.bounds[3]);}
    canvas.origin_x=std::floor(canvas.origin_x/pixel_x)*pixel_x;canvas.origin_y=std::floor(canvas.origin_y/pixel_y)*pixel_y;
    double cols=std::ceil((xmax-canvas.origin_x)/pixel_x),rows=std::ceil((ymax-canvas.origin_y)/pixel_y);
    require(cols<=std::numeric_limits<int>::max() && rows<=std::numeric_limits<int>::max(),"unrepresentable raster canvas");
    width=static_cast<int>(cols);height=static_cast<int>(rows);
  }
  require(static_cast<double>(width)*aa<=std::numeric_limits<int>::max() &&
          static_cast<double>(height)*aa<=std::numeric_limits<int>::max() &&
          static_cast<double>(width)*height<=std::numeric_limits<int>::max(),"unrepresentable raster canvas");
  canvas.width=width;canvas.height=height;canvas.aa=aa;canvas.w=width*aa;canvas.h=height*aa;
  canvas.dx=pixel_x/aa;canvas.dy=pixel_y/aa;
  canvas.tiles_x=(width+canvas.tile-1)/canvas.tile;canvas.tiles_y=(height+canvas.tile-1)/canvas.tile;
  Shapes shapes=shapes_of(scene,paint);
  const auto& line=scene.packet.polylines;
  bool retained=std::any_of(line.hidden.begin(),line.hidden.end(),[](Index x){return x!=0;});
  bool translucent_strokes=std::any_of(line.opacity.begin(),line.opacity.end(),[](double x){return x>0 && x<1;});
  const Index items=static_cast<Index>(scene.items.size());
  std::vector<Span> spans(items);
  std::vector<int> tile_start(static_cast<std::size_t>(canvas.tiles_x)*canvas.tiles_y+1,0);
  const int ts=canvas.tile_samples();
  auto each_tile=[&](const Span& span,auto f) {
    if(span.samples[0]>span.samples[2] || span.samples[1]>span.samples[3]) return;
    for(int ty=span.samples[1]/ts;ty<=span.samples[3]/ts;++ty)
      for(int tx=span.samples[0]/ts;tx<=span.samples[2]/ts;++tx) f(static_cast<std::size_t>(ty)*canvas.tiles_x+tx);
  };
  for(Index i=0;i<items;++i) {
    if(i%4096==0) Rcpp::checkUserInterrupt();
    const Item& item=scene.items[i]; const Paint& p=paint[i]; Span& span=spans[i];
    span.partition=scene.table(item.kind).partition[item.row];
    span.lift=retained && item.kind==2 && line.hidden[item.row]!=0 && span.partition==1;
    span.occluder=item.kind==1 || item.kind==3;
    span.retained_occluder=retained && item.kind==1 && p.colour.a==1 && span.partition==1;
    span.nearest=R_PosInf;
    for(Index j=0;j<item.count;++j) span.nearest=std::min(span.nearest,scene.vertices[item.offset+j].depth);
    if(scene.projection.camera.perspective && !(span.nearest>0)) span.nearest=R_NegInf;
    span.samples={0,0,-1,-1};
    if(item.kind==4 || (p.colour.a==0 && p.outline.a==0)) continue;
    span.samples=canvas.samples(p.bounds);
    each_tile(span,[&](std::size_t t){++tile_start[t+1];});
  }
  for(std::size_t t=0;t+1<tile_start.size();++t) tile_start[t+1]+=tile_start[t];
  std::vector<Index> tile_items(tile_start.back());
  {
    std::vector<int> cursor(tile_start.begin(),tile_start.end()-1);
    for(Index i=0;i<items;++i) each_tile(spans[i],[&](std::size_t t){tile_items[cursor[t]++]=i;});
  }
  Buffers buffers;
  std::size_t pixels=static_cast<std::size_t>(width)*height;
  buffers.image.assign(pixels,0); buffers.id.assign(pixels,0);
  if(keep_buffers) {buffers.item.assign(pixels,-1);buffers.depth.assign(pixels,R_PosInf);buffers.opaque_depth.assign(pixels,R_PosInf);}
  int tiles=canvas.tiles_x*canvas.tiles_y;
  threads=thread_count(threads,tiles);
  std::vector<TileScratch> scratch(threads);
  std::vector<int> worker_passes(threads,0);
  double setup_seconds=seconds(start); auto tiles_start=Clock::now();
  parallel_tiles(tiles,threads,[&](int worker,int tile) {
    int tx=tile%canvas.tiles_x,ty=tile/canvas.tiles_x;
    render_tile(shapes,canvas,spans,tile_items.data()+tile_start[tile],tile_start[tile+1]-tile_start[tile],
      tx,ty,retained,translucent_strokes,keep_buffers,scratch[worker],buffers,worker_passes[worker]);
  });
  scratch.clear();
  buffers.passes=*std::max_element(worker_passes.begin(),worker_passes.end());
  double tile_seconds=seconds(tiles_start);
  Rcpp::IntegerMatrix raster(height,width),ids(height,width);
  std::copy(buffers.image.begin(),buffers.image.end(),reinterpret_cast<std::uint32_t*>(INTEGER(raster)));
  for(int y=0;y<height;++y) for(int x=0;x<width;++x)
    ids(height-1-y,x)=buffers.id[static_cast<std::size_t>(height-1-y)*width+x];
  raster.attr("class")="nativeRaster";
  std::vector<int> text_rows; std::vector<double> text_x,text_y;
  std::vector<Index> text_items;
  for(Index i=0;i<items;++i) if(scene.items[i].kind==4 && paint[i].colour.a!=0) text_items.push_back(i);
  if(!text_items.empty()) {
    // Opaque depth-partition items binned by their continuous bounds, so an
    // anchor tests only the items whose bounds share its cell.
    const double cell_x=ts*canvas.dx,cell_y=ts*canvas.dy;
    auto cell=[&](double value,double origin,double size,int count) {
      return static_cast<int>(std::max(0.0,std::min(static_cast<double>(count-1),std::floor((value-origin)/size))));
    };
    std::vector<int> cell_start(static_cast<std::size_t>(tiles)+1,0);
    auto each_cell=[&](Index j,auto f) {
      const auto& item=scene.items[j];
      if(item.kind==4 || paint[j].colour.a<1 || spans[j].partition!=1) return;
      const auto& b=paint[j].bounds;
      for(int cy=cell(b[1],canvas.origin_y,cell_y,canvas.tiles_y);cy<=cell(b[3],canvas.origin_y,cell_y,canvas.tiles_y);++cy)
        for(int cx=cell(b[0],canvas.origin_x,cell_x,canvas.tiles_x);cx<=cell(b[2],canvas.origin_x,cell_x,canvas.tiles_x);++cx)
          f(static_cast<std::size_t>(cy)*canvas.tiles_x+cx);
    };
    for(Index j=0;j<items;++j) each_cell(j,[&](std::size_t t){++cell_start[t+1];});
    for(int t=0;t<tiles;++t) cell_start[t+1]+=cell_start[t];
    std::vector<Index> cell_items(cell_start.back());
    {
      std::vector<int> cursor(cell_start.begin(),cell_start.end()-1);
      for(Index j=0;j<items;++j) each_cell(j,[&](std::size_t t){cell_items[cursor[t]++]=j;});
    }
    for(Index i:text_items) {
      const auto& item=scene.items[i];
      const auto& anchor=scene.vertices[item.offset]; bool hidden=false;
      if(scene.packet.text.partition[item.row]==1) {
        std::size_t t=static_cast<std::size_t>(cell(anchor.y,canvas.origin_y,cell_y,canvas.tiles_y))*canvas.tiles_x+
          cell(anchor.x,canvas.origin_x,cell_x,canvas.tiles_x);
        for(int k=cell_start[t];k<cell_start[t+1] && !hidden;++k) {
          Index j=cell_items[k];
          double z=sample(shapes,scene.items[j],paint[j],anchor.x,anchor.y);
          hidden=std::isfinite(z) && before(scene,j,z,i,anchor.depth);
        }
      }
      if(!hidden) {text_rows.push_back(item.row+1);text_x.push_back(anchor.x/scene.projection.viewport.width_mm);text_y.push_back(anchor.y/scene.projection.viewport.height_mm);}
    }
  }
  Rcpp::List result=Rcpp::List::create(Rcpp::Named("image")=raster,Rcpp::Named("id")=ids,
    Rcpp::Named("text")=Rcpp::List::create(Rcpp::Named("row")=text_rows,Rcpp::Named("x")=text_x,Rcpp::Named("y")=text_y),
    Rcpp::Named("passes")=buffers.passes,Rcpp::Named("raster_seconds")=seconds(start),
    Rcpp::Named("extent")=Rcpp::NumericVector::create(canvas.origin_x/scene.projection.viewport.width_mm,canvas.origin_y/scene.projection.viewport.height_mm,
      (canvas.origin_x+width*pixel_x)/scene.projection.viewport.width_mm,(canvas.origin_y+height*pixel_y)/scene.projection.viewport.height_mm),
    Rcpp::Named("width")=width,Rcpp::Named("height")=height,Rcpp::Named("antialias")=aa,Rcpp::Named("threads")=threads);
  result["timing"]=Rcpp::NumericVector::create(Rcpp::Named("setup")=setup_seconds,Rcpp::Named("tiles")=tile_seconds);
  if(keep_buffers) {
    // Buffers at device pixel resolution, rows from the top, read at each
    // pixel's centre sample: depth of the nearest painted fragment, depth at
    // which coverage becomes opaque, its kind and 1-based table row, and the
    // model-space normal of a face.
    Rcpp::NumericMatrix depth(height,width),opaque(height,width),normal_x(height,width),normal_y(height,width),normal_z(height,width);
    Rcpp::IntegerMatrix kind(height,width),row(height,width);
    for(int y=0;y<height;++y) for(int x=0;x<width;++x) {
      std::size_t pixel=static_cast<std::size_t>(y)*width+x;
      Index i=buffers.item[pixel];
      depth(y,x)=std::isfinite(buffers.depth[pixel]) ? buffers.depth[pixel]:NA_REAL;
      opaque(y,x)=std::isfinite(buffers.opaque_depth[pixel]) ? buffers.opaque_depth[pixel]:NA_REAL;
      kind(y,x)=i<0 ? 0:scene.items[i].kind;
      row(y,x)=i<0 ? NA_INTEGER:scene.items[i].row+1;
      bool face=i>=0 && scene.items[i].kind==1;
      normal_x(y,x)=face ? scene.normals[scene.items[i].row][0]:NA_REAL;
      normal_y(y,x)=face ? scene.normals[scene.items[i].row][1]:NA_REAL;
      normal_z(y,x)=face ? scene.normals[scene.items[i].row][2]:NA_REAL;
    }
    result["buffers"]=Rcpp::List::create(Rcpp::Named("depth")=depth,Rcpp::Named("opaque_depth")=opaque,
      Rcpp::Named("kind")=kind,Rcpp::Named("row")=row,
      Rcpp::Named("normal")=Rcpp::List::create(Rcpp::Named("x")=normal_x,Rcpp::Named("y")=normal_y,Rcpp::Named("z")=normal_z));
  }
  return result;
}
} }
