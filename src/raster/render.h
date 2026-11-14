#pragma once
#include "geometry.h"
#include "../scene/halo.h"
#include "../shade/read.h"
#include "../raymarch/volume.h"
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
  // A splat is a sphere impostor: radius in mm, and depth units per mm at its
  // centre (0 for other paint). edl is the eye-dome lighting strength.
  double radius=0,splat=0,edl=0;
  // Paint-once key: a sample takes at most one fragment per key, the first
  // in compositing order; -1 paints every fragment.
  Index once=-1;
};

// Smooth faces (Phong 1975 normals, Gouraud 1971 colours): per face row the
// unlit linear base colour, whether it is lit and its face normal; the light,
// the direction toward the viewer, and per item whether its vertices carry
// attributes that vary over it.
struct Smooth {
  std::vector<Vec3> base,normal;
  std::vector<bool> lit,item;
  std::vector<int> material;
  std::vector<shade::Material> materials;
  shade::Lights lights;
  shade::View view;
  bool linear=true,active=false;
};

// Per-draw constants shared read-only by all tiles: the scene, the paints,
// marker footprint rotations and the fan-triangle areas of every polygon item.
struct Shapes {
  const Scene& scene;
  const std::vector<Paint>& paint;
  std::vector<std::array<double,2>> rotation;
  std::vector<double> area;
  const Smooth* smooth=nullptr;
  const Smooth* quadric_lighting=nullptr;
};

// Where a sample hits a polygon item: fan triangle (0, i, i+1) and the screen
// barycentric coordinates of its corners.
struct Hit { Index i=0; double a=0,b=0,c=0; };

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
                            bool* border=nullptr,bool hidden=false,bool ignore_dash=false,Hit* hit=nullptr) {
  const Scene& scene=shapes.scene;
  const Screen* v=scene.vertices.data()+item.offset;
  if(x<paint.bounds[0] || y<paint.bounds[1] || x>paint.bounds[2] || y>paint.bounds[3]) return R_PosInf;
  if(border) *border=false;
  if(item.kind==5) {
    const QuadricHit hit=scene.quadric_hit(item,x,y);
    return hit.valid ? hit.t:R_PosInf;
  }
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
         edge_inside(c,v[0],v[i],orientation)) {
        if(hit) *hit={i,a,b,c};
        return scene.projection.camera.perspective ? 1/(a/v[0].depth+b/v[i].depth+c/v[i+1].depth) :
          a*v[0].depth+b*v[i].depth+c*v[i+1].depth;
      }
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
  } else if(item.kind==3 && paint.splat>0) {
    double dx=x-v[0].x,dy=y-v[0].y,d2=dx*dx+dy*dy,r2=paint.radius*paint.radius;
    return d2<=r2 ? v[0].depth-paint.splat*std::sqrt(r2-d2):R_PosInf;
  } else if(item.kind==3 && footprint(shapes,item.row,x-v[0].x,y-v[0].y)) {
    if(border) *border=paint.separate && !footprint(shapes,item.row,x-v[0].x,y-v[0].y,paint.width);
    return v[0].depth;
  }
  return R_PosInf;
}

// Depth at which a haloed stroke segment masks a canvas point: within half its
// mask width of the segment (round ends, so the bands of consecutive segments
// join), +Inf elsewhere (Appel, Rohlf and Stein 1979).
static inline double halo_depth(const Scene& scene,const Item& item,double x,double y) {
  const Screen* v=scene.vertices.data()+item.offset;
  const double radius=scene.packet.polylines.mask_width_mm[item.row]*.5;
  double dx=v[1].x-v[0].x,dy=v[1].y-v[0].y,length2=dx*dx+dy*dy;
  double t=length2>0 ? std::max(0.0,std::min(1.0,((x-v[0].x)*dx+(y-v[0].y)*dy)/length2)):0;
  double ex=x-v[0].x-t*dx,ey=y-v[0].y-t*dy;
  if(ex*ex+ey*ey>radius*radius) return R_PosInf;
  return depth_at(v[0].depth,v[1].depth,t,scene.projection.camera.perspective);
}

static inline Shapes shapes_of(const Scene& scene,const std::vector<Paint>& paint) {
  Shapes shapes{scene,paint,{},{},{}};
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
    Rcpp::NumericMatrix colour,outline; Rcpp::NumericVector width,edl; Rcpp::LogicalVector separate,splat; bool styled=false;
  };
  std::array<Table,5> tables;
  require(colours.size()==5,"invalid raster colour tables");
  for(int kind=1;kind<=5;++kind) {
    Table& t=tables[kind-1]; t.colour=Rcpp::as<Rcpp::NumericMatrix>(colours[kind-1]);
    require(t.colour.nrow()==4 && t.colour.ncol()==static_cast<int>(scene.table(kind).count.size()),"invalid raster colour table");
    t.styled=t.colour.hasAttribute("outline");
    if(t.styled) {
      t.outline=Rcpp::as<Rcpp::NumericMatrix>(t.colour.attr("outline"));
      t.width=Rcpp::as<Rcpp::NumericVector>(t.colour.attr("width_mm"));
      t.separate=Rcpp::as<Rcpp::LogicalVector>(t.colour.attr("separate"));
    }
    if(t.colour.hasAttribute("splat")) t.splat=Rcpp::as<Rcpp::LogicalVector>(t.colour.attr("splat"));
    if(t.colour.hasAttribute("edl")) t.edl=Rcpp::as<Rcpp::NumericVector>(t.colour.attr("edl"));
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
    if(t.edl.size()) p.edl=t.edl[item.row];
    p.colour={c(0,item.row),c(1,item.row),c(2,item.row),(id_pass ? 1:c(3,item.row))*table.opacity[item.row]};
    if(t.styled) {
      p.outline={t.outline(0,item.row),t.outline(1,item.row),t.outline(2,item.row),t.outline(3,item.row)*table.opacity[item.row]};
      p.width=t.width[item.row];p.separate=t.separate[item.row];
      if(item.kind==1 && p.outline.a>0) {p.bounds[0]-=p.width*.5;p.bounds[1]-=p.width*.5;p.bounds[2]+=p.width*.5;p.bounds[3]+=p.width*.5;}
    }
    if(item.kind==3 && t.splat.size() && t.splat[item.row]) {
      // Orthographic and perspective images scale camera x by sy/2 * height / w.
      const auto& projection=scene.projection;
      double w=projection.camera.perspective ? scene.vertices[item.offset].depth:1;
      p.radius=(p.bounds[2]-p.bounds[0])*.5;
      p.splat=w/(.5*projection.sy*projection.viewport.height_mm);
      p.outline=Colour();p.separate=false;
    }
    if(item.kind==2) {
      // Haloed strokes sample their mask band too.
      double grow=std::max(0.0,(scene.packet.polylines.mask_width_mm[item.row]-scene.packet.polylines.width_mm[item.row])*.5);
      if(item.geometry==0) {p.bounds[0]-=grow;p.bounds[1]-=grow;p.bounds[2]+=grow;p.bounds[3]+=grow;}
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

// Paint-once keys. Translucent strokes, arrowheads included, paint a sample
// once per polyline row. Faces of one translucent sheet drawn with
// alpha_mode = "surface" (the "sheet" attribute of the face colours, 0 for
// none) share a key, so a sample takes the sheet's nearest piece only and
// sheets composite over each other in depth order.
static inline void paint_once(const Scene& scene,std::vector<Paint>& paint,Rcpp::NumericMatrix face_colours,
                              bool translucent_strokes) {
  const Index lines=static_cast<Index>(scene.packet.polylines.count.size());
  Rcpp::IntegerVector sheet;
  if(face_colours.hasAttribute("sheet")) {
    sheet=Rcpp::as<Rcpp::IntegerVector>(face_colours.attr("sheet"));
    require(sheet.size()==face_colours.ncol(),"invalid raster sheet keys");
  }
  for(std::size_t i=0;i<scene.items.size();++i) {
    const Item& item=scene.items[i];
    if(item.kind==2 && translucent_strokes) paint[i].once=item.row;
    if(item.kind==1 && sheet.size() && sheet[item.row]>0) paint[i].once=lines+sheet[item.row]-1;
  }
}

// Sample grid of the canvas: sample centres sit at origin + (index + 0.5) * step.
// With `linear`, paints are composited and averaged in linear light and the
// averaged pixel is sRGB-encoded.
struct Canvas {
  double origin_x=0,origin_y=0,dx=1,dy=1;
  double pixel_x=1,pixel_y=1;
  int width=0,height=0,aa=1,w=0,h=0;
  bool linear=false;
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
  bool lift,occluder,retained_occluder,halo=false;
};

// A painted sample: the item, its depth, and whether the outline paints it.
// A smooth face carries its own colour at the sample (`own`).
struct Fragment { Index item; double z; int sample; bool border; bool own=false; Colour colour{}; };

// The nearest opaque coverage of a sample: its partition and, within the depth
// partition, its depth. A fragment in a lower partition, or more than the
// tolerance behind it in the depth partition, follows it in compositing order.
// Halo bands are kept apart from opaque depth and hide only depth-partition
// strokes, as the scene compiler's line coverage does: up to four bands of
// distinct polylines, the nearest kept, so a stroke is cut by the nearest
// band of a polyline it does not join (HaloJoins).
struct Occluder {
  static constexpr int bands=4;
  int partition=-1,halos=0; double z=R_PosInf;
  std::array<double,bands> halo_depth; std::array<Index,bands> halo_row;
  bool hides(int p,double depth,double tolerance) const {
    return p<partition || (p==1 && partition==1 && depth>z+tolerance);
  }
  bool hides_line(int p,double depth,double tolerance,Index row,const HaloJoins& joins) const {
    if(hides(p,depth,tolerance)) return true;
    if(p!=1 || partition>1) return false;
    for(int k=0;k<halos;++k)
      if(depth>halo_depth[k]+tolerance && !joins.joined(halo_row[k],row)) return true;
    return false;
  }
  void add(int p,double depth) {
    if(p>partition) {partition=p;z=depth;if(p>1) halos=0;}
    else if(p==partition && depth<z) z=depth;
  }
  void add_halo(Index row,double depth) {
    if(partition>1) return;
    for(int k=0;k<halos;++k) if(halo_row[k]==row) {halo_depth[k]=std::min(halo_depth[k],depth);return;}
    int slot=halos<bands ? halos++:-1;
    if(slot<0) {
      slot=0;
      for(int k=1;k<bands;++k) if(halo_depth[k]>halo_depth[slot]) slot=k;
      if(halo_depth[slot]<=depth) return;
    }
    halo_depth[slot]=depth;halo_row[slot]=row;
  }
};

// `first` is the nearest painting item, or -1 when nothing or a traced layer
// (`traced`, 1-based) paints first. `coverage` is the depth at which the
// accumulated alpha reaches half its final value.
struct SampleResult {
  Colour colour;
  Index first=-1;
  int traced=0;
  double depth=R_PosInf,opaque_depth=R_PosInf,coverage=R_PosInf;
  int peels=0;
};

// One compositing step of a sample: a fragment or a traced surface at depth
// z, or a medium segment [a, b] of alpha `segment` composited over `before`.
struct Step { double z,alpha,a,b,before,segment; bool medium; };

// Ray-traced content per canvas sample, rays in the order of
// raymarch_rays_cpp (rows from the top, column-major): the nearest opaque
// traced surface (camera depth, straight colour in the compositing space,
// traced layer, model-space normal) and the absorption volumes, integrated
// along each sample's data-space ray. Camera depth z maps to the ray
// parameter (z - near) * length.
struct Traced {
  bool active=false;
  int h=0;
  std::vector<double> depth;
  std::vector<Colour> colour;
  std::vector<int> layer,ids,volume_layer;
  std::vector<Vec3> normal,origin,direction;
  std::vector<double> length;
  std::vector<raymarch::Volume> volumes;
  double near=0,far=0;
  std::size_t ray(int sx,int sy) const {return static_cast<std::size_t>(h-1-sy)+static_cast<std::size_t>(h)*sx;}
  raymarch::Ray segment(std::size_t i,double a,double b) const {
    return {origin[i],direction[i],(a-near)*length[i],(b-near)*length[i]};
  }
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

// IEC 61966-2-1 sRGB transfer function and its inverse.
static inline double srgb_decode(double v) {return v<=0.04045 ? v/12.92:std::pow((v+0.055)/1.055,2.4);}
static inline double srgb_encode(double v) {return v<=0.0031308 ? 12.92*v:1.055*std::pow(v,1/2.4)-0.055;}
static inline Colour linear_colour(const Colour& c) {return {srgb_decode(c.r),srgb_decode(c.g),srgb_decode(c.b),c.a};}

// Composites the absorption volumes between camera depths a and b of ray i.
// The integral is premultiplied linear light; an sRGB compositing space
// receives its encoded straight colour, premultiplied again.
static inline void traced_medium(const Traced& t,std::size_t i,double a,double b,bool linear,
                                 SampleResult& out,std::vector<Step>* steps) {
  if(t.volumes.empty() || !(b>a)) return;
  raymarch::Medium m=raymarch::absorb(t.volumes,t.segment(i,a,b));
  if(!(m.alpha>0)) return;
  Colour c{m.colour[0],m.colour[1],m.colour[2],m.alpha};
  if(!linear) {c.r=srgb_encode(c.r/c.a)*c.a;c.g=srgb_encode(c.g/c.a)*c.a;c.b=srgb_encode(c.b/c.a)*c.a;}
  double before=out.colour.a,weight=1-before;
  out.colour.r+=weight*c.r; out.colour.g+=weight*c.g; out.colour.b+=weight*c.b; out.colour.a+=weight*c.a;
  if(++out.peels==1) {
    out.depth=t.near+m.entry/t.length[i];
    Vec3 entry=raymarch::point(t.origin[i],t.direction[i],m.entry);
    out.traced=t.volume_layer[0];
    for(std::size_t v=0;v<t.volumes.size();++v) if(t.volumes[v].contains(entry)) {out.traced=t.volume_layer[v];break;}
  }
  if(steps) steps->push_back({b,out.colour.a,a,b,before,m.alpha,true});
}

// Composites the opaque traced surface of ray i.
static inline void traced_surface(const Traced& t,std::size_t i,SampleResult& out,std::vector<Step>* steps) {
  const Colour& c=t.colour[i];
  double weight=1-out.colour.a;
  out.colour.r+=weight*c.r; out.colour.g+=weight*c.g; out.colour.b+=weight*c.b; out.colour.a+=weight;
  if(++out.peels==1) {out.traced=t.layer[i];out.depth=t.depth[i];}
  out.opaque_depth=t.depth[i];
  if(steps) steps->push_back({t.depth[i],out.colour.a,0,0,0,0,false});
}

// Depth inside a medium segment at which the accumulated alpha reaches
// `target`: regula falsi (Illinois) on the optical depth of the partial
// segment, which is linear in depth for a homogeneous medium.
static inline double medium_coverage(const Traced& t,std::size_t i,const Step& s,double target) {
  auto optical=[](double alpha){return -std::log1p(-std::min(alpha,1-1e-12));};
  double goal=optical((target-s.before)/(1-s.before));
  double lo=s.a,hi=s.b,flo=-goal,fhi=optical(s.segment)-goal;
  int side=0;
  double best=hi,error=std::abs(fhi);
  for(int iteration=0;iteration<16 && fhi>flo && error>1e-12*goal;++iteration) {
    double z=hi-fhi*(hi-lo)/(fhi-flo);
    if(!(z>lo && z<hi)) z=.5*(lo+hi);
    double fz=optical(raymarch::absorb(t.volumes,t.segment(i,s.a,z)).alpha)-goal;
    if(std::abs(fz)<error) {best=z;error=std::abs(fz);}
    if(fz>0) {hi=z;fhi=fz;if(side==1) flo*=.5;side=1;}
    else {lo=z;flo=fz;if(side==-1) fhi*=.5;side=-1;}
  }
  return best;
}

static inline void coverage_of(const Traced* t,std::size_t i,const std::vector<Step>& steps,SampleResult& out) {
  if(!(out.colour.a>0)) return;
  double target=.5*out.colour.a;
  for(const Step& s:steps) if(s.alpha>=target) {
    out.coverage=s.medium ? medium_coverage(*t,i,s,target):s.z;
    return;
  }
}

// Walks one sample's fragments in compositing order. A primitive paints a
// sample once in succession and each paint-once key once in total; the walk
// stops when the accumulated coverage is opaque. With traced content, the
// absorption volumes are integrated between consecutive depth-partition
// fragments, front to back, and the traced surface is composited at its
// depth; overlay fragments precede and underlay fragments follow both. A
// fragment within the depth tolerance of the traced surface precedes it.
static inline SampleResult composite_sample(const Shapes& shapes,const Fragment* f,std::size_t k,
                                            std::vector<Index>& painted,
                                            const Traced* traced,std::size_t ray,bool linear,
                                            std::vector<Step>* steps) {
  const Scene& scene=shapes.scene;
  SampleResult out; painted.clear();
  if(steps) steps->clear();
  const bool trace=traced && traced->active;
  const double tolerance=scene.projection.resolved.tolerance;
  const double surface=trace ? traced->depth[ray]:R_PosInf;
  double cursor=trace ? traced->near:R_PosInf;
  bool opaque=false;
  Index previous=-1; double previous_z=R_NegInf;
  for(std::size_t j=0;j<k;++j) {
    const Item& item=scene.items[f[j].item];
    const Index once=shapes.paint[f[j].item].once;
    if(once>=0 && std::find(painted.begin(),painted.end(),once)!=painted.end()) continue;
    if(previous>=0) {
      const Item& prev=scene.items[previous];
      if(prev.kind==item.kind && prev.row==item.row) continue;
      if(!before(scene,previous,previous_z,f[j].item,f[j].z)) continue;
    }
    if(trace) {
      int partition=scene.table(item.kind).partition[item.row];
      if(partition<=1) {
        double z=partition==1 ? f[j].z:R_PosInf;
        if(surface+tolerance<z) {
          traced_medium(*traced,ray,cursor,surface,linear,out,steps);
          traced_surface(*traced,ray,out,steps);
          opaque=true;
          break;
        }
        double until=partition==1 ? z:traced->far;
        traced_medium(*traced,ray,cursor,until,linear,out,steps);
        cursor=std::max(cursor,until);
      }
    }
    const Paint& p=shapes.paint[f[j].item];
    const Colour& c=f[j].own ? f[j].colour:f[j].border ? p.outline:p.colour;
    if(once>=0) painted.push_back(once);
    double weight=(1-out.colour.a)*c.a;
    out.colour.r+=weight*c.r; out.colour.g+=weight*c.g; out.colour.b+=weight*c.b; out.colour.a+=weight;
    if(++out.peels==1) {out.first=f[j].item;out.depth=f[j].z;}
    if(steps) steps->push_back({f[j].z,out.colour.a,0,0,0,0,false});
    if(c.a==1) {out.opaque_depth=f[j].z;opaque=true;break;}
    previous=f[j].item; previous_z=f[j].z;
  }
  if(trace && !opaque) {
    traced_medium(*traced,ray,cursor,std::min(surface,traced->far),linear,out,steps);
    if(std::isfinite(surface)) traced_surface(*traced,ray,out,steps);
  }
  if(steps) coverage_of(traced,ray,*steps,out);
  return out;
}

// Per device pixel, rows from the top: the sum of premultiplied sample
// colours, and the centre sample's primitive id, nearest item or traced
// layer and depths.
struct Buffers {
  std::vector<Colour> colour;
  std::vector<int> id,traced;
  std::vector<Index> item;
  std::vector<double> depth,opaque_depth,coverage;
  int passes=0;
};

struct TileScratch {
  std::vector<Fragment> fragments,ordered,merge;
  std::vector<Occluder> occluders;
  std::vector<double> opaque;
  std::vector<int> start;
  std::vector<SampleResult> results;
  std::vector<Index> order,painted;
  std::vector<Step> steps;
};

static inline std::uint32_t channel_byte(double value) {
  return static_cast<std::uint32_t>(std::round(std::max(0.0,std::min(1.0,value))*255));
}

// Colour of a smooth face at a sample: corner colours and normals
// interpolated perspective-correctly (weights a / depth under perspective),
// the normal renormalised and lit by the shading kernel in linear light, then
// put in the compositing space with the paint's alpha.
static inline Colour smooth_colour(const Shapes& shapes,const Item& item,const Paint& paint,const Hit& hit) {
  const Scene& scene=shapes.scene; const Smooth& sm=*shapes.smooth;
  const Index k[3]={item.offset,item.offset+hit.i,item.offset+hit.i+1};
  double w[3]={hit.a,hit.b,hit.c};
  if(scene.projection.camera.perspective) {
    double total=0;
    for(int j=0;j<3;++j) {w[j]/=scene.vertices[k[j]].depth;total+=w[j];}
    for(double& x:w) x/=total;
  }
  auto blend=[&](bool normal) {
    Vec3 out{0,0,0};
    for(int j=0;j<3;++j) {
      const Vec3& v=normal ? scene.shading[k[j]].normal:scene.shading[k[j]].colour;
      if(!std::isfinite(v[0]+v[1]+v[2])) return Vec3{NAN,NAN,NAN};
      for(int d=0;d<3;++d) out[d]+=w[j]*v[d];
    }
    return out;
  };
  Vec3 colour=blend(false);
  if(!std::isfinite(colour[0]+colour[1]+colour[2])) colour=sm.base[item.row];
  if(sm.lit[item.row]) {
    Vec3 normal=blend(true);
    if(!std::isfinite(normal[0]+normal[1]+normal[2]) || !(dot(normal,normal)>0)) normal=sm.normal[item.row];
    Vec3 position{0,0,0};
    for(int j=0;j<3;++j) for(int d=0;d<3;++d) position[d]+=w[j]*scene.shading[k[j]].position[d];
    const shade::vec3 p=shade::to_vec3(position);
    const shade::vec3 lit=shade::shade(shade::normalize(shade::to_vec3(normal)),p,sm.view.at(p),shade::to_vec3(colour),
      sm.materials[sm.material[item.row]],sm.lights);
    colour={lit.x,lit.y,lit.z};
  }
  Colour out{std::max(0.0,std::min(1.0,colour[0])),std::max(0.0,std::min(1.0,colour[1])),
             std::max(0.0,std::min(1.0,colour[2])),paint.colour.a};
  if(!sm.linear) {out.r=srgb_encode(out.r);out.g=srgb_encode(out.g);out.b=srgb_encode(out.b);}
  return out;
}

// Colour of a lit quadric item at a canvas point: its linear base colour
// through the shading kernel at the exact hit point and normal, or its paint
// colour where the quadric is unlit.
static inline Colour quadric_colour(const Shapes& shapes,const Item& item,const Paint& paint,double x,double y) {
  const Scene& scene=shapes.scene; const Smooth& sm=*shapes.quadric_lighting;
  if(!sm.lit[item.row]) return paint.colour;
  Ray ray;
  const QuadricHit hit=scene.quadric_hit(item,x,y,&ray);
  if(!hit.valid) return paint.colour;
  const Vec3 box=ray_point(ray,hit.t);
  const Vec3 normal=quadric_normal(scene.quadrics[item.row],scene.projection,box,hit.part);
  const shade::vec3 position=shade::to_vec3(scene.projection.model(box));
  const shade::vec3 lit=shade::shade(shade::to_vec3(normal),position,sm.view.at(position),
    shade::to_vec3(sm.base[item.row]),sm.materials[sm.material[item.row]],sm.lights);
  Colour out{std::max(0.0,std::min(1.0,lit.x)),std::max(0.0,std::min(1.0,lit.y)),std::max(0.0,std::min(1.0,lit.z)),paint.colour.a};
  if(!sm.linear) {out.r=srgb_encode(out.r);out.g=srgb_encode(out.g);out.b=srgb_encode(out.b);}
  return out;
}

static inline void render_tile(const Shapes& shapes,const Canvas& canvas,const std::vector<Span>& spans,
                               const Index* tile_items,std::size_t tile_count,int tx,int ty,
                               bool retained,const Traced& traced,bool keep_buffers,
                               const HaloJoins& joins,TileScratch& s,Buffers& out,int& passes) {
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
    const bool smooth_item=shapes.smooth && shapes.smooth->active && item.kind==1 && shapes.smooth->item[i];
    const bool lit_quadric=item.kind==5 && shapes.quadric_lighting && shapes.quadric_lighting->active;
    const Index line_row=item.kind==2 ? item.row:-1;
    int x0=std::max(span.samples[0],sx0),y0=std::max(span.samples[1],sy0);
    int x1=std::min(span.samples[2],sx1-1),y1=std::min(span.samples[3],sy1-1);
    for(int y=y0;y<=y1;++y) for(int x=x0;x<=x1;++x) {
      int local=(y-sy0)*tw+(x-sx0);
      Occluder& occluder=s.occluders[local];
      if(!span.lift && (line_row>=0 ? occluder.hides_line(span.partition,span.nearest,tolerance,line_row,joins):
                        occluder.hides(span.partition,span.nearest,tolerance))) continue;
      bool border=false;double xx=canvas.origin_x+(x+.5)*canvas.dx,yy=canvas.origin_y+(y+.5)*canvas.dy;
      Hit hit;
      if(span.halo) {
        // A haloed stroke hides what lies behind it in its mask band.
        double h=halo_depth(scene,item,xx,yy);
        if(std::isfinite(h)) occluder.add_halo(item.row,h);
      }
      double z=sample(shapes,item,p,xx,yy,&border,false,span.lift,&hit);
      if(span.retained_occluder) s.opaque[local]=std::min(s.opaque[local],z);
      if(span.lift && std::isfinite(z)) {
        bool hidden=z>s.opaque[local]+tolerance;
        z=sample(shapes,item,p,xx,yy,&border,hidden && line.hidden[item.row]==1);
        if(hidden && std::isfinite(z)) z=s.opaque[local];
      }
      if(!std::isfinite(z)) continue;
      const Colour& c=border ? p.outline:p.colour;
      if(c.a==0 || (line_row>=0 ? occluder.hides_line(span.partition,z,tolerance,line_row,joins):
                                  occluder.hides(span.partition,z,tolerance))) continue;
      Fragment fragment{i,z,local,border};
      if(!border && smooth_item) {fragment.own=true;fragment.colour=smooth_colour(shapes,item,p,hit);}
      if(lit_quadric) {fragment.own=true;fragment.colour=quadric_colour(shapes,item,p,xx,yy);}
      s.fragments.push_back(fragment);
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
    {
      const Item& item=scene.items[f[j].item]; const int partition=spans[f[j].item].partition;
      if(!(item.kind==2 ? s.occluders[k].hides_line(partition,f[j].z,tolerance,item.row,joins):
                          s.occluders[k].hides(partition,f[j].z,tolerance))) f[kept++]=f[j];
    }
    order_fragments(scene,f,kept,s.merge);
    const int sx=sx0+static_cast<int>(k%tw),sy=sy0+static_cast<int>(k/tw);
    const bool centre=keep_buffers && sx%canvas.aa==canvas.aa/2 && sy%canvas.aa==canvas.aa/2;
    s.results[k]=composite_sample(shapes,f,kept,s.painted,
      &traced,traced.active ? traced.ray(sx,sy):0,canvas.linear,centre ? &s.steps:nullptr);
    passes=std::max(passes,s.results[k].peels);
  }
  const int aa=canvas.aa;
  for(int py=sy0/aa;py<sy1/aa;++py) for(int px=sx0/aa;px<sx1/aa;++px) {
    Colour sum;
    for(int sy=0;sy<aa;++sy) for(int sx=0;sx<aa;++sx) {
      const auto& c=s.results[static_cast<std::size_t>(py*aa+sy-sy0)*tw+px*aa+sx-sx0].colour;
      sum.r+=c.r;sum.g+=c.g;sum.b+=c.b;sum.a+=c.a;
    }
    std::size_t pixel=static_cast<std::size_t>(canvas.height-1-py)*canvas.width+px;
    const SampleResult& centre=s.results[static_cast<std::size_t>(py*aa+aa/2-sy0)*tw+px*aa+aa/2-sx0];
    out.colour[pixel]=sum;
    out.id[pixel]=centre.first>=0 ? scene.table(scene.items[centre.first].kind).primitive_id[scene.items[centre.first].row]:
      centre.traced>0 ? traced.ids[centre.traced-1]:0;
    out.item[pixel]=centre.first;out.traced[pixel]=centre.traced;
    out.depth[pixel]=centre.depth;out.opaque_depth[pixel]=centre.opaque_depth;out.coverage[pixel]=centre.coverage;
  }
}

// Eye-dome lighting (Boucheny 2009, as in ParaView): a pixel whose nearest
// paint has a positive strength darkens by
// exp(-edl_scale * strength * response), where response averages, over the
// eight neighbours one pixel away, how far each neighbour lies in front in
// log2 of the depth normalised to the near-far range. Empty neighbours lie at
// the far plane and never darken. ParaView scales by 300 on a perspective
// z-buffer, which compresses depth toward the far plane; this depth is linear
// in eye distance, so neighbouring steps are larger and the scale smaller.
static constexpr double edl_scale=40;
static inline void eye_dome_lighting(const Shapes& shapes,const Canvas& canvas,Buffers& b) {
  const auto& resolved=shapes.scene.projection.resolved;
  const double range=resolved.far-resolved.near;
  auto level=[&](std::size_t pixel) {
    double z=b.depth[pixel];
    if(!std::isfinite(z)) return 0.0;
    return std::log2(std::max(1e-6,std::min(1.0,(z-resolved.near)/range)));
  };
  const int width=canvas.width,height=canvas.height;
  std::vector<double> shade(b.colour.size(),1);
  for(int y=0;y<height;++y) for(int x=0;x<width;++x) {
    std::size_t pixel=static_cast<std::size_t>(y)*width+x;
    Index i=b.item[pixel];
    if(i<0 || shapes.paint[i].edl<=0) continue;
    double centre=level(pixel),response=0;
    for(int dy=-1;dy<=1;++dy) for(int dx=-1;dx<=1;++dx) {
      if(dx==0 && dy==0) continue;
      int nx=std::max(0,std::min(width-1,x+dx)),ny=std::max(0,std::min(height-1,y+dy));
      response+=std::max(0.0,centre-level(static_cast<std::size_t>(ny)*width+nx));
    }
    shade[pixel]=std::exp(-edl_scale*shapes.paint[i].edl*response/8);
  }
  for(std::size_t pixel=0;pixel<b.colour.size();++pixel) {
    b.colour[pixel].r*=shade[pixel];b.colour[pixel].g*=shade[pixel];b.colour[pixel].b*=shade[pixel];
  }
}

static inline std::uint32_t pack_pixel(const Colour& sum,const Canvas& canvas) {
  double alpha=sum.a/(static_cast<double>(canvas.aa)*canvas.aa);
  std::uint32_t packed=channel_byte(alpha)<<24;
  if(sum.a>0) {
    Colour c{sum.r/sum.a,sum.g/sum.a,sum.b/sum.a,0};
    if(canvas.linear) c={srgb_encode(c.r),srgb_encode(c.g),srgb_encode(c.b),0};
    packed|=channel_byte(c.r)|(channel_byte(c.g)<<8)|(channel_byte(c.b)<<16);
  }
  return packed;
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

// Undoes the items and vertices a draw appends to the prepared scene.
struct ItemRestore {
  Scene& scene;std::size_t items,vertices;
  explicit ItemRestore(Scene& s):scene(s),items(s.items.size()),vertices(s.vertices.size()) {}
  ~ItemRestore() {scene.items.resize(items);scene.vertices.resize(vertices);}
};

// The canvas of a width x height pixel request. An unclipped camera grows
// it, on the same pixel grid, to the bounds of every paint.
static inline Canvas canvas_of(const Scene& scene,const std::vector<Paint>& paint,int width,int height,int aa,bool linear) {
  require(width>0 && height>0 && aa>0,"raster dimensions and antialias must be positive");
  Canvas canvas; canvas.linear=linear;
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
  canvas.pixel_x=pixel_x;canvas.pixel_y=pixel_y;
  canvas.width=width;canvas.height=height;canvas.aa=aa;canvas.w=width*aa;canvas.h=height*aa;
  canvas.dx=pixel_x/aa;canvas.dy=pixel_y/aa;
  canvas.tiles_x=(width+canvas.tile-1)/canvas.tile;canvas.tiles_y=(height+canvas.tile-1)/canvas.tile;
  return canvas;
}

// Canvas extent in panel npc: left, bottom, right, top.
static inline Rcpp::NumericVector canvas_extent(const Scene& scene,const Canvas& canvas) {
  const auto& viewport=scene.projection.viewport;
  return Rcpp::NumericVector::create(canvas.origin_x/viewport.width_mm,canvas.origin_y/viewport.height_mm,
    (canvas.origin_x+canvas.width*canvas.pixel_x)/viewport.width_mm,(canvas.origin_y+canvas.height*canvas.pixel_y)/viewport.height_mm);
}

static inline Rcpp::List canvas_info(SEXP pointer,Rcpp::List colours,Rcpp::List dashes,int width,int height,int aa) {
  Rcpp::XPtr<Scene> holder(pointer); Scene& scene=*holder;
  ItemRestore restore(scene);
  auto paint=paints(scene,colours,dashes,false);
  add_arrowheads(scene,paint,colours[1],restore.items);
  Canvas c=canvas_of(scene,paint,width,height,aa,false);
  return Rcpp::List::create(Rcpp::Named("width")=c.width,Rcpp::Named("height")=c.height,
    Rcpp::Named("antialias")=aa,Rcpp::Named("extent")=canvas_extent(scene,c));
}

// Reads the traced input of a draw: per-sample vectors over the canvas
// samples and, with absorption volumes, the data-space rays. Colours arrive
// in linear light and are encoded for an sRGB compositing space.
static inline Traced traced_of(Rcpp::List input,const Canvas& canvas) {
  Traced t;
  if(input.size()==0) return t;
  t.active=true; t.h=canvas.h;
  const std::size_t n=static_cast<std::size_t>(canvas.w)*canvas.h;
  Rcpp::NumericVector depth=input["depth"];
  Rcpp::NumericMatrix colour=input["colour"],normal=input["normal"];
  Rcpp::IntegerVector layer=input["layer"];
  require(static_cast<std::size_t>(depth.size())==n && static_cast<std::size_t>(layer.size())==n &&
          static_cast<std::size_t>(colour.nrow())==n && colour.ncol()==3 &&
          static_cast<std::size_t>(normal.nrow())==n && normal.ncol()==3,"traced samples do not match the raster canvas");
  t.ids=Rcpp::as<std::vector<int>>(input["ids"]);
  t.depth.resize(n);t.colour.resize(n);t.layer.resize(n);t.normal.resize(n);
  for(std::size_t i=0;i<n;++i) {
    t.depth[i]=Rcpp::NumericVector::is_na(depth[i]) ? R_PosInf:depth[i];
    t.layer[i]=layer[i];
    require(layer[i]>=0 && layer[i]<=static_cast<int>(t.ids.size()),"invalid traced layer");
    Colour c{colour(i,0),colour(i,1),colour(i,2),1};
    if(!canvas.linear) c={srgb_encode(c.r),srgb_encode(c.g),srgb_encode(c.b),1};
    t.colour[i]=c;
    t.normal[i]={normal(i,0),normal(i,1),normal(i,2)};
  }
  t.near=Rcpp::as<double>(input["near"]);t.far=Rcpp::as<double>(input["far"]);
  Rcpp::List volumes=input["volumes"];
  if(volumes.size()) {
    for(SEXP v:volumes) t.volumes.emplace_back(Rcpp::as<Rcpp::List>(v));
    t.volume_layer=Rcpp::as<std::vector<int>>(input["volume_layer"]);
    Rcpp::NumericMatrix origin=input["origin"],direction=input["direction"];
    Rcpp::NumericVector length=input["length"];
    require(t.volume_layer.size()==t.volumes.size() && static_cast<std::size_t>(origin.nrow())==n &&
            static_cast<std::size_t>(direction.nrow())==n && static_cast<std::size_t>(length.size())==n,"invalid traced rays");
    t.origin.resize(n);t.direction.resize(n);t.length.assign(length.begin(),length.end());
    for(std::size_t i=0;i<n;++i) {
      t.origin[i]={origin(i,0),origin(i,1),origin(i,2)};
      t.direction[i]={direction(i,0),direction(i,1),direction(i,2)};
    }
  }
  return t;
}

// The smooth-face input of a draw, from the "smooth" attribute of the face
// colours: list(base = n x 3 unlit linear colours, lit, normal = n x 3 unit
// model-space face normals, material (index, 0 for the default), materials,
// lights (or NULL) and view, as read by src/shade/read.h).
static inline Smooth smooth_of(const Scene& scene,Rcpp::NumericMatrix face_colours,bool linear) {
  Smooth sm; sm.linear=linear;
  if(!scene.corner.any() || !face_colours.hasAttribute("smooth")) return sm;
  Rcpp::List x=face_colours.attr("smooth");
  Rcpp::NumericMatrix base=x["base"],normal=x["normal"];
  Rcpp::LogicalVector lit=x["lit"];
  Rcpp::IntegerVector material=x["material"];
  const std::size_t n=scene.packet.faces.count.size();
  require(static_cast<std::size_t>(base.nrow())==n && base.ncol()==3 && static_cast<std::size_t>(normal.nrow())==n &&
          normal.ncol()==3 && static_cast<std::size_t>(lit.size())==n && static_cast<std::size_t>(material.size())==n,
          "invalid raster smooth-face input");
  sm.materials=shade::read_materials(x["materials"]);
  sm.base.resize(n);sm.normal.resize(n);sm.lit.resize(n);sm.material.resize(n);
  for(std::size_t i=0;i<n;++i) {
    sm.base[i]={base(i,0),base(i,1),base(i,2)};
    sm.normal[i]={normal(i,0),normal(i,1),normal(i,2)};
    sm.lit[i]=lit[i]==TRUE;
    require(material[i]>=0 && material[i]<static_cast<int>(sm.materials.size()),"invalid raster material index");
    sm.material[i]=material[i];
  }
  if(!Rf_isNull(x["lights"])) sm.lights=shade::read_lights(x["lights"]);
  sm.view=shade::read_view(x["view"]);
  sm.item.assign(scene.items.size(),false);
  for(std::size_t i=0;i<scene.items.size();++i) {
    const Item& item=scene.items[i];
    if(item.kind!=1 || static_cast<std::size_t>(item.offset+item.count)>scene.shading.size()) continue;
    bool varies=false;
    for(Index j=0;j<item.count && !varies;++j) {
      const Shading& v=scene.shading[item.offset+j];
      varies=std::isfinite(v.colour[0]+v.colour[1]+v.colour[2]) ||
        (sm.lit[item.row] && std::isfinite(v.normal[0]+v.normal[1]+v.normal[2]));
    }
    sm.item[i]=varies;
    sm.active=sm.active || varies;
  }
  return sm;
}

// Lighting of the quadrics, from the "smooth" attribute of their colour table
// as for faces: per quadric row the linear base colour, whether it is lit and
// its material, with the panel's materials, lights and view.
static inline Smooth quadric_lighting_of(const Scene& scene,Rcpp::NumericMatrix colours,bool linear) {
  Smooth sm; sm.linear=linear;
  if(!colours.hasAttribute("smooth")) return sm;
  Rcpp::List x=colours.attr("smooth");
  Rcpp::NumericMatrix base=x["base"];
  Rcpp::LogicalVector lit=x["lit"];
  Rcpp::IntegerVector material=x["material"];
  const std::size_t n=scene.packet.quadrics.kind.size();
  require(static_cast<std::size_t>(base.nrow())==n && base.ncol()==3 && static_cast<std::size_t>(lit.size())==n &&
          static_cast<std::size_t>(material.size())==n,"invalid raster quadric lighting");
  sm.materials=shade::read_materials(x["materials"]);
  sm.base.resize(n);sm.lit.resize(n);sm.material.resize(n);
  for(std::size_t i=0;i<n;++i) {
    sm.base[i]={base(i,0),base(i,1),base(i,2)};
    sm.lit[i]=lit[i]==TRUE;
    require(material[i]>=0 && material[i]<static_cast<int>(sm.materials.size()),"invalid raster material index");
    sm.material[i]=material[i];
    sm.active=sm.active || sm.lit[i];
  }
  if(!Rf_isNull(x["lights"])) sm.lights=shade::read_lights(x["lights"]);
  sm.view=shade::read_view(x["view"]);
  return sm;
}

static inline Rcpp::List draw(SEXP pointer,Rcpp::List colours,Rcpp::List dashes,
                              int width,int height,int aa,bool id_pass,int threads,bool keep_buffers,bool linear,
                              Rcpp::List traced_input) {
  auto start=Clock::now();
  Rcpp::XPtr<Scene> holder(pointer); Scene& scene=*holder;
  ItemRestore restore(scene);
  require(width>0 && height>0 && aa>0,"raster dimensions and antialias must be positive");
  auto paint=paints(scene,colours,dashes,id_pass);
  add_arrowheads(scene,paint,colours[1],restore.items);
  const auto& opacity=scene.packet.polylines.opacity;
  paint_once(scene,paint,colours[0],std::any_of(opacity.begin(),opacity.end(),[](double x){return x>0 && x<1;}));
  if(linear) for(auto& p:paint) {p.colour=linear_colour(p.colour);p.outline=linear_colour(p.outline);}
  Canvas canvas=canvas_of(scene,paint,width,height,aa,linear);
  width=canvas.width;height=canvas.height;
  const Traced traced=traced_of(traced_input,canvas);
  Shapes shapes=shapes_of(scene,paint);
  const Smooth smooth=smooth_of(scene,Rcpp::as<Rcpp::NumericMatrix>(colours[0]),linear);
  shapes.smooth=&smooth;
  const Smooth quadric_lighting=quadric_lighting_of(scene,Rcpp::as<Rcpp::NumericMatrix>(colours[4]),linear);
  shapes.quadric_lighting=&quadric_lighting;
  const auto& line=scene.packet.polylines;
  bool retained=std::any_of(line.hidden.begin(),line.hidden.end(),[](Index x){return x!=0;});
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
    span.occluder=item.kind==1 || item.kind==3 || item.kind==5;
    span.retained_occluder=retained && item.kind==1 && p.colour.a==1 && span.partition==1;
    span.halo=item.kind==2 && item.geometry==0 && span.partition==1 &&
      line.mask_width_mm[item.row]>line.width_mm[item.row];
    span.nearest=R_PosInf;
    for(Index j=0;j<item.count;++j) span.nearest=std::min(span.nearest,scene.vertices[item.offset+j].depth);
    if(scene.projection.camera.perspective && !(span.nearest>0)) span.nearest=R_NegInf;
    span.nearest-=p.splat*p.radius;
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
  buffers.colour.assign(pixels,Colour()); buffers.id.assign(pixels,0);
  buffers.item.assign(pixels,-1);buffers.depth.assign(pixels,R_PosInf);buffers.opaque_depth.assign(pixels,R_PosInf);
  buffers.traced.assign(pixels,0);buffers.coverage.assign(pixels,R_PosInf);
  int tiles=canvas.tiles_x*canvas.tiles_y;
  threads=thread_count(threads,tiles);
  std::vector<TileScratch> scratch(threads);
  std::vector<int> worker_passes(threads,0);
  const HaloJoins joins(scene.packet);
  double setup_seconds=seconds(start); auto tiles_start=Clock::now();
  parallel_tiles(tiles,threads,[&](int worker,int tile) {
    int tx=tile%canvas.tiles_x,ty=tile/canvas.tiles_x;
    render_tile(shapes,canvas,spans,tile_items.data()+tile_start[tile],tile_start[tile+1]-tile_start[tile],
      tx,ty,retained,traced,keep_buffers,joins,scratch[worker],buffers,worker_passes[worker]);
  });
  scratch.clear();
  buffers.passes=*std::max_element(worker_passes.begin(),worker_passes.end());
  double tile_seconds=seconds(tiles_start);
  if(std::any_of(paint.begin(),paint.end(),[](const Paint& p){return p.edl>0;}))
    eye_dome_lighting(shapes,canvas,buffers);
  Rcpp::IntegerMatrix raster(height,width),ids(height,width);
  std::uint32_t* packed=reinterpret_cast<std::uint32_t*>(INTEGER(raster));
  for(std::size_t pixel=0;pixel<pixels;++pixel) packed[pixel]=pack_pixel(buffers.colour[pixel],canvas);
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
        int sx=static_cast<int>(std::floor((anchor.x-canvas.origin_x)/canvas.dx));
        int sy=static_cast<int>(std::floor((anchor.y-canvas.origin_y)/canvas.dy));
        if(!hidden && traced.active && sx>=0 && sy>=0 && sx<canvas.w && sy<canvas.h)
          hidden=traced.depth[traced.ray(sx,sy)]<anchor.depth-scene.projection.resolved.tolerance;
      }
      if(!hidden) {text_rows.push_back(item.row+1);text_x.push_back(anchor.x/scene.projection.viewport.width_mm);text_y.push_back(anchor.y/scene.projection.viewport.height_mm);}
    }
  }
  Rcpp::List result=Rcpp::List::create(Rcpp::Named("image")=raster,Rcpp::Named("id")=ids,
    Rcpp::Named("text")=Rcpp::List::create(Rcpp::Named("row")=text_rows,Rcpp::Named("x")=text_x,Rcpp::Named("y")=text_y),
    Rcpp::Named("passes")=buffers.passes,Rcpp::Named("raster_seconds")=seconds(start),
    Rcpp::Named("extent")=canvas_extent(scene,canvas),
    Rcpp::Named("width")=width,Rcpp::Named("height")=height,Rcpp::Named("antialias")=aa,Rcpp::Named("threads")=threads);
  result["timing"]=Rcpp::NumericVector::create(Rcpp::Named("setup")=setup_seconds,Rcpp::Named("tiles")=tile_seconds);
  if(keep_buffers) {
    // Buffers at device pixel resolution, rows from the top, read at each
    // pixel's centre sample: depth of the nearest painted fragment, depth at
    // which coverage becomes opaque, its kind (6 for a traced layer) and
    // 1-based row (table row, or traced layer), the model-space normal of a
    // face or traced surface, and the depth at which the accumulated alpha
    // reaches half its final value.
    Rcpp::NumericMatrix depth(height,width),opaque(height,width),coverage(height,width),
      normal_x(height,width),normal_y(height,width),normal_z(height,width);
    Rcpp::IntegerMatrix kind(height,width),row(height,width);
    auto finite=[](double z){return std::isfinite(z) ? z:NA_REAL;};
    for(int y=0;y<height;++y) for(int x=0;x<width;++x) {
      std::size_t pixel=static_cast<std::size_t>(y)*width+x;
      Index i=buffers.item[pixel];
      int layer=buffers.traced[pixel];
      depth(y,x)=finite(buffers.depth[pixel]);
      opaque(y,x)=finite(buffers.opaque_depth[pixel]);
      coverage(y,x)=finite(buffers.coverage[pixel]);
      kind(y,x)=i>=0 ? scene.items[i].kind:layer>0 ? 6:0;
      row(y,x)=i>=0 ? scene.items[i].row+1:layer>0 ? layer:NA_INTEGER;
      std::array<double,3> n{NA_REAL,NA_REAL,NA_REAL};
      if(i>=0 && scene.items[i].kind==1) n={scene.normals[scene.items[i].row][0],scene.normals[scene.items[i].row][1],scene.normals[scene.items[i].row][2]};
      if(i>=0 && scene.items[i].kind==5) {
        Ray ray;
        const double px=canvas.origin_x+(x+.5)*canvas.pixel_x,py=canvas.origin_y+(height-1-y+.5)*canvas.pixel_y;
        const QuadricHit hit=scene.quadric_hit(scene.items[i],px,py,&ray);
        if(hit.valid) {
          const Vec3 q=quadric_normal(scene.quadrics[scene.items[i].row],scene.projection,ray_point(ray,hit.t),hit.part);
          n={q[0],q[1],q[2]};
        }
      }
      if(i<0 && layer>0) {
        std::size_t r=traced.ray(x*aa+aa/2,(height-1-y)*aa+aa/2);
        if(traced.layer[r]==layer) n={traced.normal[r][0],traced.normal[r][1],traced.normal[r][2]};
      }
      normal_x(y,x)=n[0];normal_y(y,x)=n[1];normal_z(y,x)=n[2];
    }
    result["buffers"]=Rcpp::List::create(Rcpp::Named("depth")=depth,Rcpp::Named("opaque_depth")=opaque,
      Rcpp::Named("kind")=kind,Rcpp::Named("row")=row,
      Rcpp::Named("normal")=Rcpp::List::create(Rcpp::Named("x")=normal_x,Rcpp::Named("y")=normal_y,Rcpp::Named("z")=normal_z),
      Rcpp::Named("coverage_depth")=coverage);
  }
  return result;
}

// Closed rings bounding a set of grid cells, in cell-corner coordinates. Each
// boundary side of a cell becomes a directed edge with the cell on its left,
// and edges are chained at shared corners. Filled with the even-odd rule the
// rings cover exactly the set, whichever way chains split at pinch corners.
struct CellRings { std::vector<int> x,y,ring; };
static inline void trace_cells(const std::vector<std::int64_t>& cells,std::int64_t columns,CellRings& out) {
  if(cells.empty()) return;
  auto member=[&](std::int64_t x,std::int64_t y) {
    return x>=0 && x<columns && y>=0 && std::binary_search(cells.begin(),cells.end(),y*columns+x);
  };
  struct Edge {std::int64_t from,to;};
  const std::int64_t stride=columns+1;
  std::vector<Edge> edges;
  for(std::int64_t cell:cells) {
    std::int64_t x=cell%columns,y=cell/columns;
    std::int64_t a=y*stride+x,b=a+1,c=b+stride,d=a+stride;
    if(!member(x,y-1)) edges.push_back({a,b});
    if(!member(x+1,y)) edges.push_back({b,c});
    if(!member(x,y+1)) edges.push_back({c,d});
    if(!member(x-1,y)) edges.push_back({d,a});
  }
  std::sort(edges.begin(),edges.end(),[](const Edge& p,const Edge& q){return p.from<q.from;});
  std::vector<bool> used(edges.size(),false);
  auto find=[&](std::int64_t from) {
    auto it=std::lower_bound(edges.begin(),edges.end(),from,[](const Edge& e,std::int64_t v){return e.from<v;});
    for(;it!=edges.end() && it->from==from;++it) if(!used[it-edges.begin()]) return static_cast<std::ptrdiff_t>(it-edges.begin());
    return static_cast<std::ptrdiff_t>(-1);
  };
  int ring=out.ring.empty() ? 0:out.ring.back();
  for(std::size_t first=0;first<edges.size();++first) {
    if(used[first]) continue;
    ++ring;
    std::vector<std::int64_t> corners;
    std::ptrdiff_t e=static_cast<std::ptrdiff_t>(first);
    while(e>=0) {used[e]=true;corners.push_back(edges[e].from);e=find(edges[e].to);}
    std::size_t n=corners.size();
    for(std::size_t k=0;k<n;++k) {
      std::int64_t p=corners[(k+n-1)%n],q=corners[k],r=corners[(k+1)%n];
      bool straight=(q-p)==(r-q);
      if(straight) continue;
      out.x.push_back(static_cast<int>(q%stride));out.y.push_back(static_cast<int>(q/stride));out.ring.push_back(ring);
    }
  }
}

// Visibility of vector primitives against an opaque raster depth buffer on the
// raster's pixel grid. Per primitive (kind, row): 0 when no painted pixel
// centre lies behind the raster, 1 when every one does, 2 otherwise, with the
// rings of its hidden pixels. Text is tested at its anchor. Underlay
// primitives count as behind and overlay primitives as in front.
static inline Rcpp::List occlusion(SEXP pointer,Rcpp::List colours,Rcpp::List dashes,
                                   Rcpp::NumericVector extent,int width,int height,int aa,Rcpp::NumericMatrix depth) {
  Rcpp::XPtr<Scene> holder(pointer); Scene& scene=*holder;
  ItemRestore restore(scene);
  require(width>0 && height>0 && depth.nrow()==height && depth.ncol()==width,"invalid raster depth buffer");
  auto paint=paints(scene,colours,dashes,false);
  add_arrowheads(scene,paint,colours[1],restore.items);
  Shapes shapes=shapes_of(scene,paint);
  Canvas canvas;
  const auto& viewport=scene.projection.viewport;
  canvas.width=width;canvas.height=height;canvas.w=width;canvas.h=height;
  canvas.origin_x=extent[0]*viewport.width_mm;canvas.origin_y=extent[1]*viewport.height_mm;
  canvas.dx=(extent[2]-extent[0])*viewport.width_mm/width;canvas.dy=(extent[3]-extent[1])*viewport.height_mm/height;
  const double tolerance=scene.projection.resolved.tolerance;
  auto raster_depth=[&](int x,int y) {
    x=std::max(0,std::min(width-1,x)); y=std::max(0,std::min(height-1,y));
    double z=depth(height-1-y,x);
    return Rcpp::NumericVector::is_na(z) ? R_PosInf:z;
  };
  // Raster depth that hides a canvas point: the farthest of the four buffer
  // samples around it (each pixel's centre sample, (aa / 2 + 0.5) / aa of a
  // pixel in). Paint lying on a rasterised surface is never hidden by the
  // interpolation error between samples, and an occlusion edge moves by at
  // most one pixel toward the visible side.
  auto depth_at_point=[&](double px,double py) {
    const double offset=(aa/2+.5)/aa;
    double u=(px-canvas.origin_x)/canvas.dx-offset,v=(py-canvas.origin_y)/canvas.dy-offset;
    int x0=static_cast<int>(std::floor(u)),y0=static_cast<int>(std::floor(v));
    return std::max(std::max(raster_depth(x0,y0),raster_depth(x0+1,y0)),
                    std::max(raster_depth(x0,y0+1),raster_depth(x0+1,y0+1)));
  };
  // The point whose depth decides a sample: a stroke ribbon lies at the depth
  // of its centreline, so a stroke sample is seen when the nearest centreline
  // point is; other paint is tested where it is drawn.
  auto decisive=[&](const Item& item,double x,double y) {
    const Screen* v=scene.vertices.data()+item.offset;
    if(item.kind==2 && item.geometry==0) {
      double dx=v[1].x-v[0].x,dy=v[1].y-v[0].y,length2=dx*dx+dy*dy;
      double t=length2>0 ? std::max(0.0,std::min(1.0,((x-v[0].x)*dx+(y-v[0].y)*dy)/length2)):0;
      return std::array<double,2>{v[0].x+t*dx,v[0].y+t*dy};
    }
    if(item.kind==2 && (item.geometry==1 || item.geometry==2)) return std::array<double,2>{v[0].x,v[0].y};
    return std::array<double,2>{x,y};
  };
  struct State {std::vector<std::int64_t> hidden; bool shown=false;};
  std::array<std::vector<State>,5> states;
  for(int kind=1;kind<=5;++kind) states[kind-1].resize(scene.table(kind).count.size());
  for(std::size_t i=0;i<scene.items.size();++i) {
    if(i%4096==0) Rcpp::checkUserInterrupt();
    const Item& item=scene.items[i]; const Paint& p=paint[i];
    State& state=states[item.kind-1][item.row];
    int partition=scene.table(item.kind).partition[item.row];
    if(partition!=1) continue;
    if(item.kind==4) {
      const Screen& anchor=scene.vertices[item.offset];
      int x=static_cast<int>(std::floor((anchor.x-canvas.origin_x)/canvas.dx));
      int y=static_cast<int>(std::floor((anchor.y-canvas.origin_y)/canvas.dy));
      bool inside=x>=0 && y>=0 && x<width && y<height;
      if(inside && anchor.depth>depth_at_point(anchor.x,anchor.y)+tolerance) state.hidden.push_back(static_cast<std::int64_t>(y)*width+x);
      else state.shown=true;
      continue;
    }
    auto b=canvas.samples(p.bounds);
    for(int y=b[1];y<=b[3];++y) for(int x=b[0];x<=b[2];++x) {
      double xx=canvas.origin_x+(x+.5)*canvas.dx,yy=canvas.origin_y+(y+.5)*canvas.dy;
      double z=sample(shapes,item,p,xx,yy,nullptr,false,true);
      if(!std::isfinite(z)) continue;
      auto point=decisive(item,xx,yy);
      double occluder=depth_at_point(point[0],point[1]);
      if(z>occluder+tolerance) state.hidden.push_back(static_cast<std::int64_t>(y)*width+x);
      else state.shown=true;
    }
  }
  Rcpp::List status(5);
  CellRings rings; std::vector<int> ring_kind,ring_row;
  for(int kind=1;kind<=5;++kind) {
    const auto& table=scene.table(kind);
    Rcpp::IntegerVector out(table.count.size());
    for(std::size_t row=0;row<table.count.size();++row) {
      State& state=states[kind-1][row];
      int partition=table.partition[row];
      if(partition==0) {out[row]=1;continue;}
      if(partition==2 || state.hidden.empty()) {out[row]=0;continue;}
      if(!state.shown) {out[row]=1;continue;}
      out[row]=2;
      std::sort(state.hidden.begin(),state.hidden.end());
      state.hidden.erase(std::unique(state.hidden.begin(),state.hidden.end()),state.hidden.end());
      std::size_t before=rings.ring.size();
      trace_cells(state.hidden,width,rings);
      ring_kind.insert(ring_kind.end(),rings.ring.size()-before,kind);
      ring_row.insert(ring_row.end(),rings.ring.size()-before,static_cast<int>(row)+1);
    }
    status[kind-1]=out;
  }
  status.names()=Rcpp::CharacterVector::create("faces","polylines","markers","text","quadrics");
  Rcpp::NumericVector x(rings.x.size()),y(rings.y.size());
  for(std::size_t k=0;k<rings.x.size();++k) {
    x[k]=(canvas.origin_x+rings.x[k]*canvas.dx)/viewport.width_mm;
    y[k]=(canvas.origin_y+rings.y[k]*canvas.dy)/viewport.height_mm;
  }
  return Rcpp::List::create(Rcpp::Named("status")=status,
    Rcpp::Named("rings")=Rcpp::List::create(Rcpp::Named("x")=x,Rcpp::Named("y")=y,Rcpp::Named("ring")=rings.ring,
      Rcpp::Named("kind")=ring_kind,Rcpp::Named("row")=ring_row));
}
} }
