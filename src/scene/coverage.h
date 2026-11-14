#pragma once
#include "partition.h"
#include "bounds.h"

namespace ggplot3d { namespace engine {

static inline Vertex screen_vertex(double x, double y, double depth, const Projection& p) {
  double w = p.camera.perspective ? depth : 1;
  double cx = (2*x-1)*w/p.sx, cy = (2*y-1)*w/p.sy;
  Vertex out;
  for (int k = 0; k < 3; ++k)
    out.xyz[k] = (p.resolved.eye[k] + cx*p.resolved.right[k] +
      cy*p.resolved.up[k] + depth*p.resolved.ray[k])/p.scale[k];
  return out;
}

static inline Polygon marker_bounds(const ScenePacket& packet, Index row,
                                    const Vertex& anchor, const Projection& p, bool& clipped) {
  Index id = packet.markers.footprint_id[row];
  const auto& f = packet.footprints;
  double angle = std::remainder(f.angle[id],360.0)*pi/180;
  double co = std::cos(angle), si = std::sin(angle);
  double xmin = R_PosInf, xmax = R_NegInf, ymin = R_PosInf, ymax = R_NegInf;
  if (f.kind[id] == static_cast<Index>(FootprintKind::ellipse)) {
    xmax = std::hypot(f.radius_x_mm[id]*co, f.radius_y_mm[id]*si); xmin = -xmax;
    ymax = std::hypot(f.radius_x_mm[id]*si, f.radius_y_mm[id]*co); ymin = -ymax;
  } else {
    for (Index i = f.offset[id]; i < f.offset[id]+f.count[id]; ++i) {
      double x = packet.footprint_vertices.x_mm[i], y = packet.footprint_vertices.y_mm[i];
      double u = co*x-si*y, v = si*x+co*y;
      xmin = std::min(xmin,u); xmax = std::max(xmax,u);
      ymin = std::min(ymin,v); ymax = std::max(ymax,v);
    }
  }
  Vec3 c = p.camera_point(anchor.xyz); auto s = p.npc(c);
  if (p.camera.clip) {
    clipped=clipped || xmin < -s[0]*p.viewport.width_mm || xmax > (1-s[0])*p.viewport.width_mm ||
      ymin < -s[1]*p.viewport.height_mm || ymax > (1-s[1])*p.viewport.height_mm;
    xmin = std::max(xmin,-s[0]*p.viewport.width_mm);
    xmax = std::min(xmax,(1-s[0])*p.viewport.width_mm);
    ymin = std::max(ymin,-s[1]*p.viewport.height_mm);
    ymax = std::min(ymax,(1-s[1])*p.viewport.height_mm);
  }
  if (xmin > xmax || ymin > ymax) return {};
  if (xmin == xmax || ymin == ymax) return {anchor};
  Polygon result;
  for (auto xy : {std::array<double,2>{xmin,ymin}, {xmax,ymin}, {xmax,ymax}, {xmin,ymax}})
    result.push_back(screen_vertex(s[0]+xy[0]/p.viewport.width_mm,
                                  s[1]+xy[1]/p.viewport.height_mm,c[2],p));
  return result;
}

static inline void append_mask(CompiledScene& out, const Polygon& polygon, const Projection& p) {
  require(out.masks.ring.size()+polygon.size()<=std::numeric_limits<Index>::max(),
          "mask index capacity exceeded");
  out.fragments.mask_offset.back() = out.masks.ring.size();
  out.fragments.mask_count.back() = polygon.size();
  for (const Vertex& v : polygon) {
    auto s = p.npc(p.camera_point(v.xyz));
    out.masks.x_npc.push_back(s[0]); out.masks.y_npc.push_back(s[1]);
    out.masks.ring.push_back(1);
  }
}

static inline Polygon text_bounds(const ScenePacket& packet,Index row,const Vertex& anchor,
                                  const Projection& p) {
  const auto& text=packet.text;
  double xmin=-text.hjust[row]*text.width_mm[row], xmax=xmin+text.width_mm[row];
  double ymin=-text.vjust[row]*text.height_mm[row], ymax=ymin+text.height_mm[row];
  double angle=std::remainder(text.angle[row],360.0)*pi/180, co=std::cos(angle), si=std::sin(angle);
  auto camera=p.camera_point(anchor.xyz);auto s=p.npc(camera);
  Polygon result;
  for(auto xy:{std::array<double,2>{xmin,ymin},{xmax,ymin},{xmax,ymax},{xmin,ymax}})
    result.push_back(screen_vertex(s[0]+(co*xy[0]-si*xy[1])/p.viewport.width_mm,
                                  s[1]+(si*xy[0]+co*xy[1])/p.viewport.height_mm,camera[2],p));
  return result;
}

static inline Polygon stroke_bounds(const Piece& piece, const ScenePacket& packet,
                                    const Projection& p, Vec3& normal, int sector=0) {
  const auto& line=packet.polylines;
  Vec3 a=p.camera_point(piece.polygon[0].xyz), b=p.camera_point(piece.polygon[1].xyz);
  auto sa=p.npc(a), sb=p.npc(b);
  double dx=(sb[0]-sa[0])*p.viewport.width_mm, dy=(sb[1]-sa[1])*p.viewport.height_mm;
  double length=std::hypot(dx,dy);
  double model_per_mm=2/(p.sy*p.viewport.height_mm);
  if(p.camera.perspective) model_per_mm*=std::min(a[2],b[2]);
  if(length*model_per_mm<=p.resolved.tolerance) return {};
  double ux=dx/length, uy=dy/length;
  double extent=line.mask_width_mm[piece.row]*0.5;
  if(line.count[piece.row]>2 && line.join[piece.row]==static_cast<Index>(Join::mitre))
    extent*=line.mitre_limit[piece.row];
  bool constant_depth=std::abs(a[2]-b[2])<=p.resolved.tolerance;
  Vec3 across{};
  for(int k=0;k<3;++k) across[k]=(-uy*p.resolved.right[k]+ux*p.resolved.up[k]);
  Vec3 direction=subtract(p.model(piece.polygon[1].xyz),p.model(piece.polygon[0].xyz));
  normal=cross(direction,across);
  double norm=std::sqrt(dot(normal,normal));
  for(double& x:normal) x/=norm;
  if(sector!=0 || constant_depth) normal=p.resolved.forward;
  double first=0,last=length;
  if(sector<0) {first=-extent;last=0;}
  else if(sector>0) {first=length;last=length+extent;}
  else if(constant_depth && line.cap[piece.row]!=static_cast<Index>(Cap::butt)) {
    first=-extent;last=length+extent;
  }
  Polygon polygon;
  for(auto uv:{std::array<double,2>{first,-extent},{last,-extent},
              {last,extent},{first,extent}}) {
    double t=std::max(0.0,std::min(1.0,uv[0]/length));
    double depth=p.camera.perspective ? 1/((1-t)/a[2]+t/b[2]) : (1-t)*a[2]+t*b[2];
    polygon.push_back(screen_vertex(sa[0]+(uv[0]*ux-uv[1]*uy)/p.viewport.width_mm,
                                   sa[1]+(uv[0]*uy+uv[1]*ux)/p.viewport.height_mm,depth,p));
  }
  return polygon;
}

static inline bool screen_contains(const Polygon& polygon,const Vertex& anchor,const Projection& p) {
  auto q=p.npc(p.camera_point(anchor.xyz));
  bool positive=false,negative=false;
  for(std::size_t i=0;i<polygon.size();++i) {
    auto a=p.npc(p.camera_point(polygon[i].xyz));
    auto b=p.npc(p.camera_point(polygon[(i+1)%polygon.size()].xyz));
    double d=(b[0]-a[0])*(q[1]-a[1])-(b[1]-a[1])*(q[0]-a[0]);
    positive=positive || d>p.resolved.tolerance;
    negative=negative || d<-p.resolved.tolerance;
  }
  return !(positive && negative);
}

// An occluder of lines: an opaque or translucent face, or the occlusion band
// of a haloed line (mask_width_mm > width_mm), which removes the parts of
// other lines behind it whatever their hidden policy (Appel 1967).
struct CoverageFace {
  Polygon polygon;
  SplitPlane plane;
  std::array<double,4> bounds;
  bool opaque;
  bool halo = false;
  const PrimitiveTable* source = nullptr;
  Index row = -1;
};

static inline double face_ray_parameter(const CoverageFace& face,const Vertex& vertex,
                                       const Projection& p) {
  Vec3 model=p.model(vertex.xyz);
  Vec3 ray=p.camera.perspective ? subtract(model,p.resolved.eye) : p.resolved.ray;
  double denominator=dot(face.plane.normal,ray);
  if(std::abs(denominator)<=p.resolved.tolerance) return R_PosInf;
  return -dot(face.plane.normal,subtract(model,face.plane.origin))/denominator;
}
static inline void lift_coverage(Piece& piece,const CoverageFace& face,const Projection& p) {
  piece.whole_stroke_parts=0;
  if(piece.anchor.empty()) piece.anchor=piece.polygon;
  for(Vertex& v:piece.polygon) {
    Vec3 model=p.model(v.xyz);
    Vec3 ray=p.camera.perspective ? subtract(model,p.resolved.eye) : p.resolved.ray;
    double t=face_ray_parameter(face,v,p);
    require(std::isfinite(t),"unrepresentable hidden-stroke coverage plane");
    for(int k=0;k<3;++k) v.xyz[k]=(model[k]+t*ray[k])/p.scale[k];
    v.weights.clear();
  }
  piece.normal=face.plane.normal;
  piece.hidden=true;
}

class CoverageIndex {
  std::vector<CoverageFace> faces;
  BoundsIndex<CoverageFace> index;
  const Projection& projection;

public:
  CoverageIndex(const std::vector<Piece>& pieces,const ScenePacket& packet,const Projection& p):projection(p) {
    for (const Piece& piece:pieces) {
      if (piece.source->partition[piece.row]!=1) continue;
      const bool halo=piece.kind==Kind::polyline && piece.polygon.size()>=3 &&
        packet.polylines.mask_width_mm[piece.row]>packet.polylines.width_mm[piece.row];
      if (piece.kind!=Kind::face && !halo) continue;
      CoverageFace f{piece.polygon,supporting_plane(piece,p),{R_PosInf,R_PosInf,R_NegInf,R_NegInf},
                     halo || piece.source->opacity[piece.row]==1,halo,piece.source,piece.row};
      for(const Vertex& v:piece.polygon) {
        auto s=p.npc(p.camera_point(v.xyz));
        for(int k=0;k<2;++k) {f.bounds[k]=std::min(f.bounds[k],s[k]);f.bounds[k+2]=std::max(f.bounds[k+2],s[k]);}
      }
      // A face seen edge-on has no projected area and covers nothing.
      if(!std::isfinite(face_ray_parameter(f,piece.polygon[0],p))) continue;
      faces.push_back(std::move(f));
    }
    index.reset(faces);
  }

  std::vector<const CoverageFace*> query(const std::array<double,4>& bounds) const {
    std::vector<const CoverageFace*> result;
    index.query(bounds,0,[&](std::size_t i) {result.push_back(&faces[i]);});
    return result;
  }

  bool hidden(const Vertex& anchor) const {
    auto s=projection.npc(projection.camera_point(anchor.xyz));
    Vec3 model=projection.model(anchor.xyz);
    Vec3 ray=projection.camera.perspective ? subtract(model,projection.resolved.eye) : projection.resolved.ray;
    for(const CoverageFace* face:query({s[0],s[1],s[0],s[1]})) {
      if(!face->opaque || face->halo) continue;
      double t=face_ray_parameter(*face,anchor,projection);
      if(t>=-projection.resolved.tolerance/std::sqrt(dot(ray,ray))) continue;
      Vec3 hit=model;
      for(int k=0;k<3;++k) hit[k]+=t*ray[k];
      bool inside=true;
      for(std::size_t i=0;i<face->polygon.size();++i) {
        Vec3 a=projection.model(face->polygon[i].xyz);
        Vec3 b=projection.model(face->polygon[(i+1)%face->polygon.size()].xyz);
        Vec3 edge=subtract(b,a);
        if(dot(cross(edge,subtract(hit,a)),face->plane.normal)<
            -projection.resolved.tolerance*std::sqrt(dot(edge,edge))) {inside=false;break;}
      }
      if(inside) return true;
    }
    return false;
  }

  std::vector<Piece> resolve_lines(std::vector<Piece> pieces,const ScenePacket& packet,
                                 ResourceBudget& budget) const {
    std::vector<Piece> result;
    for(Piece& piece:pieces) {
      if(piece.kind!=Kind::polyline || piece.source->partition[piece.row]!=1) {
        result.push_back(std::move(piece)); continue;
      }
      std::array<double,4> bounds{R_PosInf,R_PosInf,R_NegInf,R_NegInf};
      for(const Vertex& v:piece.polygon) {
        auto s=projection.npc(projection.camera_point(v.xyz));
        for(int k=0;k<2;++k) {bounds[k]=std::min(bounds[k],s[k]);bounds[k+2]=std::max(bounds[k+2],s[k]);}
      }
      auto candidates=query(bounds);
      const PrimitiveTable* source=piece.source;const Index row=piece.row;
      std::vector<Piece> cells; cells.push_back(std::move(piece));
      for(const CoverageFace* face:candidates) {
        if(face->halo && face->source==source && face->row==row) continue;
        const Polygon& path=cells.empty() ? piece.polygon : (cells[0].anchor.empty() ? cells[0].polygon:cells[0].anchor);
        bool coplanar=!path.empty();
        for(const auto& v:path) coplanar=coplanar && std::abs(plane_distance(face->plane,v,projection))<=projection.resolved.tolerance;
        if(!coplanar && !face->halo &&
           (!face->opaque || packet.polylines.hidden[piece.row]==static_cast<Index>(Hidden::hide))) continue;
        std::vector<SplitPlane> boundaries{face->plane};
        for(std::size_t i=0;i<face->polygon.size();++i) {
          Vec3 a=projection.model(face->polygon[i].xyz);
          Vec3 b=projection.model(face->polygon[(i+1)%face->polygon.size()].xyz);
          Vec3 ray=projection.camera.perspective ? subtract(a,projection.resolved.eye) : projection.resolved.ray;
          Vec3 normal=cross(subtract(b,a),ray);
          double length=std::sqrt(dot(normal,normal));
          if(length<=projection.resolved.tolerance) continue;
          for(double& x:normal) x/=length;
          boundaries.push_back({normal,a});
        }
        for(const SplitPlane& plane:boundaries) {
          std::vector<Piece> next;
          for(Piece& cell:cells) {
            if(classify(cell,plane,projection)==3) {
              budget_split(cell,budget);
              auto halves=split_piece(std::move(cell),plane,projection);
              next.push_back(std::move(halves.first));next.push_back(std::move(halves.second));
            } else next.push_back(std::move(cell));
          }
          cells=std::move(next);
        }
        for(Piece& cell:cells) {
          Vertex centre{};
          for(const Vertex& v:cell.polygon) for(int k=0;k<3;++k) centre.xyz[k]+=v.xyz[k]/cell.polygon.size();
          if(!screen_contains(face->polygon,centre,projection)) continue;
          double t=face_ray_parameter(*face,centre,projection);
          Vec3 ray=projection.camera.perspective ? subtract(projection.model(centre.xyz),projection.resolved.eye) : projection.resolved.ray;
          if(face->halo) {
            if(!coplanar && t < -projection.resolved.tolerance/std::sqrt(dot(ray,ray))) cell.behind_halo=true;
          } else if(coplanar || t < -projection.resolved.tolerance/std::sqrt(dot(ray,ray))) {
            bool hidden=cell.hidden;
            lift_coverage(cell,*face,projection);
            if(coplanar) cell.hidden=hidden;
          }
        }
      }
      for(Piece& cell:cells) {
        if(cell.behind_halo) continue;
        if(!cell.hidden || packet.polylines.hidden[cell.row]!=static_cast<Index>(Hidden::hide))
          result.push_back(std::move(cell));
      }
    }
    return result;
  }
};

} }
