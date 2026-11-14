#pragma once
#include "ordering.h"

namespace ggplot3d { namespace engine {

// Stroke area is the union of segment strips, endpoint caps and join sectors.
// The original device outline supplies circular boundaries under polygon masks.
static inline std::vector<Piece> stroke_area(Piece source,const ScenePacket& packet,
                                            const Projection& p,ResourceBudget& budget) {
  const Polygon original=source.polygon;
  Polygon path;
  for(const auto& vertex:original) {
    while(path.size()>=2) {
      auto a=p.model(path[path.size()-2].xyz),b=p.model(path.back().xyz),c=p.model(vertex.xyz);
      auto ab=subtract(b,a),bc=subtract(c,b),normal=cross(ab,bc);
      double length=std::sqrt(dot(ab,ab))+std::sqrt(dot(bc,bc));
      if(dot(ab,bc)<0 || std::sqrt(dot(normal,normal))>p.resolved.tolerance*length) break;
      path.pop_back();
    }
    path.push_back(vertex);
  }
  const auto& line=packet.polylines;
  double radius=line.mask_width_mm[source.row]*.5;
  std::vector<Point2> xy;
  std::vector<double> depths;
  for(const auto& v:path) {
    auto c=p.camera_point(v.xyz);auto s=p.npc(c);
    xy.push_back({s[0]*p.viewport.width_mm,s[1]*p.viewport.height_mm});depths.push_back(c[2]);
  }
  std::vector<Piece> areas;
  auto append=[&](const Ring2& ring,const std::vector<double>& z) {
    if(ring.size()<3 || ring_area(ring)==0) return;
    Piece cell=source;cell.anchor=original;cell.polygon.clear();cell.clipped=true;
    for(std::size_t k=0;k<ring.size();++k)
      cell.polygon.push_back(screen_vertex(ring[k][0]/p.viewport.width_mm,
                                          ring[k][1]/p.viewport.height_mm,z[k],p));
    cell.normal=area_vector(cell.polygon,p);
    double norm=std::sqrt(dot(cell.normal,cell.normal));
    if(norm==0) return;
    for(double& a:cell.normal) a/=norm;
    budget.piece(cell.polygon.size()+path.size());areas.push_back(std::move(cell));
  };
  std::vector<Point2> directions;
  for(std::size_t j=1;j<xy.size();++j) {
    double dx=xy[j][0]-xy[j-1][0],dy=xy[j][1]-xy[j-1][1],length=std::hypot(dx,dy);
    Point2 u=length>0 ? Point2{dx/length,dy/length} : Point2{0,0};directions.push_back(u);
    if(length==0) continue;
    Point2 n{-u[1]*radius,u[0]*radius};
    append({{xy[j-1][0]-n[0],xy[j-1][1]-n[1]}, {xy[j][0]-n[0],xy[j][1]-n[1]},
            {xy[j][0]+n[0],xy[j][1]+n[1]}, {xy[j-1][0]+n[0],xy[j-1][1]+n[1]}},
           {depths[j-1],depths[j],depths[j],depths[j-1]});
  }
  for(std::size_t j=0;j<xy.size();++j) {
    Point2 q=xy[j];double depth=depths[j];
    if(j==0 || j+1==xy.size()) {
      if(line.cap[source.row]==0 || (j==0 ? !source.cap_start:!source.cap_end)) continue;
      auto u=directions[j==0 ? 0:directions.size()-1];double sign=j==0 ? -1:1;
      Point2 n{-u[1]*radius,u[0]*radius},d{u[0]*radius*sign,u[1]*radius*sign};
      append({{q[0]-n[0],q[1]-n[1]},{q[0]+d[0]-n[0],q[1]+d[1]-n[1]},
              {q[0]+d[0]+n[0],q[1]+d[1]+n[1]},{q[0]+n[0],q[1]+n[1]}},
             std::vector<double>(4,depth));
    } else {
      auto a=directions[j-1],b=directions[j];double turn=a[0]*b[1]-a[1]*b[0];
      if(line.join[source.row]==0) {
        append({{q[0]-radius,q[1]-radius},{q[0]+radius,q[1]-radius},
                {q[0]+radius,q[1]+radius},{q[0]-radius,q[1]+radius}},std::vector<double>(4,depth));
      } else if(turn!=0) {
        double sign=turn>0 ? 1:-1;
        Ring2 ring{q,{q[0]+sign*a[1]*radius,q[1]-sign*a[0]*radius}};
        double denominator=1+a[0]*b[0]+a[1]*b[1];
        if(line.join[source.row]==1 && denominator>0) {
          Point2 tip{q[0]+sign*(a[1]+b[1])*radius/denominator,q[1]-sign*(a[0]+b[0])*radius/denominator};
          if(std::hypot(tip[0]-q[0],tip[1]-q[1])<=radius*line.mitre_limit[source.row]) ring.push_back(tip);
        }
        ring.push_back({q[0]+sign*b[1]*radius,q[1]-sign*b[0]*radius});
        append(ring,std::vector<double>(ring.size(),depth));
      }
    }
  }
  std::vector<ProjectedPiece> projected;
  for(const auto& cell:areas) projected.push_back(project_piece(cell,p));
  BoundsIndex<ProjectedPiece> index(projected);
  std::vector<Piece> result;
  for(std::size_t i=0;i<areas.size();++i) {
    std::vector<Piece> cells{areas[i]};
    index.query(projected[i].bounds,0,[&](std::size_t j) {
      if(j>=i || cells.empty()) return;
      const auto& ring=projected[j].ring;
      std::vector<Piece> outside;
      for(Piece& cell:cells) {
        Ring2 remainder=project_piece(cell,p).ring;
        for(std::size_t k=0;k<ring.size() && remainder.size()>=3;++k) {
          auto a=ring[k],b=ring[(k+1)%ring.size()];
          Ring2 in,out;
          auto prev=remainder.back();long double dp=orient2(a,b,prev);
          for(auto cur:remainder) {
            long double dc=orient2(a,b,cur);
            if((dp<0 && dc>0)||(dp>0 && dc<0)) {auto cut=mix2(prev,cur,dp/(dp-dc));in.push_back(cut);out.push_back(cut);}
            if(dc>=0) in.push_back(cur);
            if(dc<=0) out.push_back(cur);
            prev=cur;dp=dc;
          }
          if(out.size()>=3 && ring_area(out)>0) {
            Piece part=cell;part.polygon.clear();
            for(auto q:out) part.polygon.push_back(screen_vertex(.5+.5*p.sx*q[0],.5+.5*p.sy*q[1],
              projected_depth(projected[i],q,p.camera.perspective),p));
            budget.piece(part.polygon.size()+path.size());outside.push_back(std::move(part));
          }
          remainder=std::move(in);
        }
      }
      cells=std::move(outside);
    });
    for(auto& cell:cells) result.push_back(std::move(cell));
  }
  auto planes=clipping_planes(p);
  std::vector<Plane> viewport_planes;
  bool box_clipped=false;
  if(p.camera.clip_data) for(const auto& v:path) for(std::size_t k=0;k<6;++k)
    box_clipped=box_clipped || signed_distance(planes[k],v)<0;
  if(box_clipped) viewport_planes.assign(planes.begin(),planes.begin()+6);
  if(p.camera.clip) viewport_planes.insert(viewport_planes.end(),planes.end()-4,planes.end());
  bool trimmed=false;
  for(auto& cell:result) cell.polygon=clip_polygon(std::move(cell.polygon),viewport_planes,trimmed);
  result.erase(std::remove_if(result.begin(),result.end(),[](const Piece& x){return x.polygon.size()<3;}),result.end());
  if(!trimmed && !source.clipped) for(auto& cell:result) cell.whole_stroke_parts=result.size();
  return result;
}
} }
