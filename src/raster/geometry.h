#pragma once
#include "../scene/wire.h"
#include "../scene/clipping.h"
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
struct Scene {
  ScenePacket packet;
  Projection projection;
  std::vector<Screen> vertices;
  std::vector<Item> items;
  std::vector<Vec3> normals;
  Scene(ScenePacket p, const Camera& c, const Viewport& v)
    : packet(std::move(p)), projection(c,v) {}
  const PrimitiveTable& table(int kind) const {
    const PrimitiveTable* tables[] = {&packet.faces,&packet.polylines,&packet.markers,&packet.text};
    return *tables[kind-1];
  }
  Screen project(const Vertex& v) const {
    auto c=projection.camera_point(v.xyz); auto xy=projection.npc(c);
    return {xy[0]*projection.viewport.width_mm,xy[1]*projection.viewport.height_mm,c[2]};
  }
  void append(int kind, Index row, const Polygon& polygon, double start=0) {
    items.push_back({kind,row,static_cast<Index>(vertices.size()),static_cast<Index>(polygon.size()),start});
    for(const auto& v:polygon) vertices.push_back(project(v));
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
  }
};

static inline void prepare_geometry(Scene& scene) {
  const auto& p=scene.projection;
  auto planes=clipping_planes(p);
  const std::size_t slab_begin=p.camera.clip_data ? 6:0;
  const std::size_t centre_end=planes.size()-(p.camera.clip ? 4:0);
  scene.normals.resize(scene.packet.faces.count.size());
  for(int kind=1;kind<=4;++kind) {
    const auto& table=scene.table(kind);
    for(Index row=0;row<static_cast<Index>(table.count.size());++row) {
      if(row%4096==0) Rcpp::checkUserInterrupt();
      if(table.opacity[row]==0) continue;
      Polygon polygon; polygon.reserve(table.count[row]);
      for(Index j=0;j<table.count[row];++j) {
        Index v=scene.packet.corners.vertex[table.offset[row]+j];
        polygon.push_back({{scene.packet.vertices.x[v],scene.packet.vertices.y[v],scene.packet.vertices.z[v]}, {}});
      }
      if(kind==1) {
        scene.normals[row]=face_normal(polygon,p);
        bool outside=false,clipped=false;
        for(const auto& plane:planes) for(const auto& v:polygon)
          outside=outside || signed_distance(plane,v.xyz)<0;
        if(outside) polygon=clip_polygon(std::move(polygon),planes,clipped);
        if(polygon.size()>=3) scene.append(kind,row,polygon);
      } else if(kind==2) {
        double cumulative=0;
        for(std::size_t j=1;j<polygon.size();++j) {
          const auto& a=polygon[j-1]; const auto& b=polygon[j];
          auto slab=clip_segment_range(a,b,planes,slab_begin,slab_begin+2);
          if(slab.first>slab.second) continue;
          Screen origin=scene.project(interpolate(a,b,slab.first));
          Screen end=scene.project(interpolate(a,b,slab.second));
          double length=std::hypot(end.x-origin.x,end.y-origin.y);
          auto range=clip_segment_range(a,b,planes,0,centre_end);
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
        for(std::size_t j=0;j<centre_end;++j) visible=visible && signed_distance(planes[j],polygon[0].xyz)>=0;
        if(visible) scene.append(kind,row,polygon);
      }
    }
  }
}

static inline Rcpp::List prepare(Rcpp::List packet, Rcpp::List camera, Rcpp::List viewport) {
  auto start=Clock::now();
  Viewport v{Rcpp::as<double>(viewport["width_mm"]),Rcpp::as<double>(viewport["height_mm"]),Rcpp::as<double>(viewport["dpi"])};
  require(std::isfinite(v.width_mm) && std::isfinite(v.height_mm) && std::isfinite(v.dpi) &&
          v.width_mm>0 && v.height_mm>0 && v.dpi>0,"invalid raster viewport");
  Rcpp::XPtr<Scene> scene(new Scene(read_packet(packet),read_camera(camera),v),true);
  prepare_geometry(*scene);
  Rcpp::NumericMatrix normals(scene->normals.size(),3);
  for(std::size_t i=0;i<scene->normals.size();++i) for(int k=0;k<3;++k) normals(i,k)=scene->normals[i][k];
  const auto& c=scene->projection.resolved;
  return Rcpp::List::create(Rcpp::Named("pointer")=scene,Rcpp::Named("normals")=normals,
    Rcpp::Named("camera")=Rcpp::List::create(Rcpp::Named("right")=c.right,Rcpp::Named("up")=c.up,
      Rcpp::Named("forward")=c.forward,Rcpp::Named("model_scale")=c.model_scale,Rcpp::Named("tolerance")=c.tolerance),
    Rcpp::Named("engine_seconds")=seconds(start));
}
} }
