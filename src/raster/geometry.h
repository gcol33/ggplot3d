#pragma once
#include "../scene/wire.h"
#include "../scene/clipping.h"
#include "../shade/shade.h"
#include <chrono>

namespace ggplot3d { namespace raster {
using namespace engine;
using Clock = std::chrono::steady_clock;
static inline double seconds(Clock::time_point start) {
  return std::chrono::duration<double>(Clock::now()-start).count();
}
struct Screen { double x, y, depth; };
struct Item {
  Index kind, row, offset, count;
  double path_start = 0;
  int geometry = 0;
  bool cap_start = true, cap_end = true;
};
// Interpolated corner attributes at a face vertex: the unit model-space
// normal and the linear-light colour, each NaN where not supplied.
struct Shading {
  Vec3 normal{NAN, NAN, NAN}, colour{NAN, NAN, NAN}, position{NAN, NAN, NAN};
};
// Optional corner columns of the packet, NaN for NA: normal_x/y/z and
// red/green/blue.
struct CornerAttributes {
  std::array<std::vector<double>, 6> columns;
  bool normals = false, colours = false;
  bool any() const {return normals || colours;}
};
struct Scene {
  ScenePacket packet;
  Projection projection;
  std::vector<Screen> vertices;
  std::vector<Item> items;
  std::vector<Vec3> normals;
  // Model-space centroid of each drawn face, NaN for faces not drawn.
  std::vector<Vec3> centres;
  CornerAttributes corner;
  // Per vertex of the prepared face items; empty without corner attributes.
  std::vector<Shading> shading;
  Scene(ScenePacket p, const Camera& c, const Viewport& v)
    : packet(std::move(p)), projection(c,v) {}
  const PrimitiveTable& table(int kind) const {
    const PrimitiveTable* tables[] = {&packet.faces,&packet.polylines,&packet.markers,&packet.text};
    return *tables[kind-1];
  }
  Screen project(const Vertex& v) const {
    auto c=projection.camera_point(dehomogenised(v).xyz); auto xy=projection.npc(c);
    return {xy[0]*projection.viewport.width_mm,xy[1]*projection.viewport.height_mm,c[2]};
  }
  void append(int kind, Index row, const Polygon& polygon, double start=0) {
    items.push_back({kind,row,static_cast<Index>(vertices.size()),static_cast<Index>(polygon.size()),start});
    const std::size_t first=vertices.size();
    for(const auto& v:polygon) vertices.push_back(project(v));
    if(kind==1) {
      Vec3 centre{0,0,0};
      for(const auto& v:polygon) for(int k=0;k<3;++k) centre[k]+=v.xyz[k]*projection.resolved.model_scale[k]/polygon.size();
      centres[row]=centre;
    }
    if(corner.any()) {
      shading.resize(vertices.size());
      if(kind==1) for(std::size_t j=0;j<polygon.size();++j) shading[first+j]=interpolate_shading(polygon[j]);
    }
  }
  // Each attribute is sum weight * w * value over the source corners (the
  // weights of a dehomogenised vertex), NaN when a contributing corner lacks it.
  std::vector<double> corner_w;
  Shading interpolate_shading(const Vertex& v) const {
    Shading out;
    for(int k=0;k<3;++k) out.position[k]=v.xyz[k]*projection.resolved.model_scale[k];
    std::array<double,6> value{0,0,0,0,0,0};
    for(int k=0;k<6;++k) {
      const auto& column=corner.columns[k];
      if(column.empty()) {value[k]=NAN;continue;}
      for(const auto& weight:v.weights) value[k]+=weight.second*corner_w[weight.first]*column[weight.first];
    }
    if(corner.normals && std::isfinite(value[0]+value[1]+value[2])) {
      Vec3 n{value[0]/projection.resolved.model_scale[0],value[1]/projection.resolved.model_scale[1],value[2]/projection.resolved.model_scale[2]};
      double length=std::sqrt(dot(n,n));
      if(length>0) out.normal={n[0]/length,n[1]/length,n[2]/length};
    }
    if(corner.colours && std::isfinite(value[3]+value[4]+value[5])) out.colour={value[3],value[4],value[5]};
    return out;
  }
  void join(Index row,const Screen& a,const Screen& b,const Screen& c,double distance) {
    const auto& line=packet.polylines;
    double radius=line.width_mm[row]*.5;
    double ax=b.x-a.x,ay=b.y-a.y,bx=c.x-b.x,by=c.y-b.y;
    double al=std::hypot(ax,ay),bl=std::hypot(bx,by);
    if(al==0 || bl==0 || radius==0) return;
    ax/=al;ay/=al;bx/=bl;by/=bl;
    double turn=ax*by-ay*bx;
    if(std::abs(turn)<=64*std::numeric_limits<double>::epsilon()) return;
    Item item{2,row,static_cast<Index>(vertices.size()),1,distance};
    item.geometry=line.join[row]==0 ? 2:1;
    vertices.push_back(b);
    if(item.geometry==1) {
      double side=turn>0 ? 1:-1;
      Screen p{b.x+side*ay*radius,b.y-side*ax*radius,b.depth};
      Screen q{b.x+side*by*radius,b.y-side*bx*radius,b.depth};
      vertices.push_back(p);
      double denominator=1+ax*bx+ay*by;
      if(line.join[row]==1 && denominator>0) {
        Screen tip{b.x+side*(ay+by)*radius/denominator,b.y-side*(ax+bx)*radius/denominator,b.depth};
        if(std::hypot(tip.x-b.x,tip.y-b.y)<=line.mitre_limit[row]*radius) vertices.push_back(tip);
      }
      vertices.push_back(q);
      item.count=static_cast<Index>(vertices.size())-item.offset;
    }
    items.push_back(item);
    if(corner.any()) shading.resize(vertices.size());
  }
};

static inline void prepare_geometry(Scene& scene) {
  const auto& p=scene.projection;
  auto planes=clipping_planes(p);
  const std::size_t slab_begin=p.camera.clip_data ? 6:0;
  const std::size_t centre_end=planes.size()-(p.camera.clip ? 4:0);
  const auto points=homogeneous_points(scene.packet.vertices,p);
  scene.normals.resize(scene.packet.faces.count.size());
  scene.centres.assign(scene.packet.faces.count.size(),Vec3{NAN,NAN,NAN});
  for(int kind=1;kind<=4;++kind) {
    const auto& table=scene.table(kind);
    for(Index row=0;row<static_cast<Index>(table.count.size());++row) {
      if(row%4096==0) Rcpp::checkUserInterrupt();
      if(table.opacity[row]==0) continue;
      Polygon polygon; polygon.reserve(table.count[row]);
      const bool weighted=kind==1 && scene.corner.any();
      for(Index j=0;j<table.count[row];++j) {
        Index corner=table.offset[row]+j;
        polygon.push_back(corner_vertex(points,corner,scene.packet.corners.vertex[corner]));
        if(!weighted) polygon.back().weights.clear();
      }
      if(kind==1) {
        // A face through infinity takes its normal from its clipped ring.
        const bool unbounded=outside_chart(polygon);
        if(!unbounded) {
          Polygon chart=polygon;
          dehomogenise(chart);
          scene.normals[row]=face_normal(chart,p);
        }
        bool outside=false,clipped=false;
        for(const auto& plane:planes) for(const auto& v:polygon)
          outside=outside || signed_distance(plane,v)<0;
        if(outside) polygon=clip_polygon(std::move(polygon),planes,clipped);
        if(polygon.size()<3) continue;
        dehomogenise(polygon);
        if(unbounded) {
          bool degenerate=false;
          scene.normals[row]=face_normal(polygon,p,&degenerate);
          if(degenerate) continue;
        }
        scene.append(kind,row,polygon);
      } else if(kind==2) {
        double cumulative=0;
        for(std::size_t j=1;j<polygon.size();++j) {
          const auto& a=polygon[j-1]; const auto& b=polygon[j];
          auto range=clip_segment_range(a,b,planes,0,centre_end);
          auto slab=clip_segment_range(a,b,planes,slab_begin,slab_begin+2);
          if(slab.first<=slab.second && (!(interpolate(a,b,slab.first).w>0) || !(interpolate(a,b,slab.second).w>0)))
            slab=range;
          if(slab.first>slab.second) continue;
          Screen origin=scene.project(interpolate(a,b,slab.first));
          Screen end=scene.project(interpolate(a,b,slab.second));
          double length=std::hypot(end.x-origin.x,end.y-origin.y);
          if(range.first<=range.second) {
            Polygon segment{interpolate(a,b,range.first),interpolate(a,b,range.second)};
            auto first=scene.project(segment[0]);
            scene.append(kind,row,segment,cumulative+std::hypot(first.x-origin.x,first.y-origin.y));
            scene.items.back().cap_start=j==1 && range.first==0;
            scene.items.back().cap_end=j+1==polygon.size() && range.second==1;
            if(j+1<polygon.size() && range.second==1) {
              auto next=clip_segment_range(b,polygon[j+1],planes,0,centre_end);
              if(next.first==0 && next.second>0) scene.join(row,first,scene.project(segment[1]),
                scene.project(interpolate(b,polygon[j+1],next.second)),cumulative+length);
            }
          }
          cumulative+=length;
        }
      } else {
        bool visible=true;
        for(std::size_t j=0;j<centre_end;++j) visible=visible && signed_distance(planes[j],polygon[0])>=0;
        if(visible) scene.append(kind,row,polygon);
      }
    }
  }
}

// Corner colours are encoded sRGB channels, decoded here to linear light.
static inline void read_corner_attributes(Scene& scene, const Rcpp::List& packet) {
  Rcpp::List corners=packet["corners"];
  const char* names[]={"normal_x","normal_y","normal_z","red","green","blue"};
  auto& c=scene.corner;
  for(int k=0;k<6;++k) if(corners.containsElementNamed(names[k])) {
    Rcpp::NumericVector x=corners[names[k]];
    c.columns[k].resize(x.size());
    for(R_xlen_t i=0;i<x.size();++i) {
      double v=Rcpp::NumericVector::is_na(x[i]) ? NAN:x[i];
      if(k>=3 && std::isfinite(v)) v=v<=0.04045 ? v/12.92:std::pow((v+0.055)/1.055,2.4);
      c.columns[k][i]=v;
    }
  }
  c.normals=!c.columns[0].empty() && !c.columns[1].empty() && !c.columns[2].empty();
  c.colours=!c.columns[3].empty() && !c.columns[4].empty() && !c.columns[5].empty();
  if(!c.normals) for(int k=0;k<3;++k) c.columns[k].clear();
  if(!c.colours) for(int k=3;k<6;++k) c.columns[k].clear();
  if(!c.any()) return;
  const auto points=homogeneous_points(scene.packet.vertices,scene.projection);
  scene.corner_w.resize(scene.packet.corners.vertex.size());
  for(std::size_t i=0;i<scene.corner_w.size();++i) scene.corner_w[i]=points[scene.packet.corners.vertex[i]][3];
}

static inline Rcpp::List prepare(Rcpp::List packet, Rcpp::List camera, Rcpp::List viewport) {
  auto start=Clock::now();
  Viewport v{Rcpp::as<double>(viewport["width_mm"]),Rcpp::as<double>(viewport["height_mm"]),Rcpp::as<double>(viewport["dpi"])};
  require(std::isfinite(v.width_mm) && std::isfinite(v.height_mm) && std::isfinite(v.dpi) &&
          v.width_mm>0 && v.height_mm>0 && v.dpi>0,"invalid raster viewport");
  Rcpp::XPtr<Scene> scene(new Scene(read_packet(packet),read_camera(camera),v),true);
  read_corner_attributes(*scene,packet);
  prepare_geometry(*scene);
  Rcpp::NumericMatrix normals(scene->normals.size(),3),centres(scene->centres.size(),3);
  for(std::size_t i=0;i<scene->normals.size();++i) for(int k=0;k<3;++k) {
    normals(i,k)=scene->normals[i][k];
    centres(i,k)=std::isfinite(scene->centres[i][k]) ? scene->centres[i][k]:NA_REAL;
  }
  const auto& c=scene->projection.resolved;
  return Rcpp::List::create(Rcpp::Named("pointer")=scene,Rcpp::Named("normals")=normals,Rcpp::Named("centres")=centres,
    Rcpp::Named("camera")=Rcpp::List::create(Rcpp::Named("right")=c.right,Rcpp::Named("up")=c.up,
      Rcpp::Named("forward")=c.forward,Rcpp::Named("ray")=c.ray,Rcpp::Named("model_scale")=c.model_scale,Rcpp::Named("tolerance")=c.tolerance,
      Rcpp::Named("eye")=c.eye,Rcpp::Named("distance")=c.distance,
      Rcpp::Named("perspective")=scene->projection.camera.perspective),
    Rcpp::Named("engine_seconds")=seconds(start));
}
} }
