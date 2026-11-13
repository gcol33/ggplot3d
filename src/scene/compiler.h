#pragma once
#include "clipping.h"
#include "output.h"
#include "coverage.h"
#include "ordering.h"
#include "stroke.h"
#include "solid.h"

namespace ggplot3d { namespace engine {
static inline CompiledScene compile(const ScenePacket& packet, const Projection& projection,
                                   ResourceBudget& budget) {
  CompiledScene out;
  out.camera = projection.resolved;
  std::vector<Piece> pieces;
  auto emit = [&](const PrimitiveTable& source, Index row, Kind kind,
                  const Polygon& polygon, bool clipped,
                  Vec3 normal = Vec3{0,0,0}, double start = 0, double end = 0,
                  bool arrow_start = false, bool arrow_end = false) {
    budget.piece(polygon.size());
    pieces.push_back({&source, row, kind, polygon, {}, normal, start, end,
                      clipped, arrow_start, arrow_end, pieces.size()});
  };
  const auto planes = clipping_planes(projection);
  std::vector<Index> solid_components;
  std::vector<Vec3> face_normals;
  const auto backfaces=solid_backfaces(packet,projection,solid_components,face_normals);
  const PrimitiveTable* tables[] = {&packet.faces, &packet.polylines, &packet.markers, &packet.text};
  for (int kind = 1; kind <= 4; ++kind) {
    const auto& table = *tables[kind-1];
    for (std::size_t row = 0; row < table.count.size(); ++row) {
      if (row % 4096 == 0) Rcpp::checkUserInterrupt();
      if(kind==1 && backfaces[row]) continue;
      Polygon polygon;
      for (Index j = 0; j < table.count[row]; ++j) {
        Index corner = table.offset[row]+j, v = packet.corners.vertex[corner];
        polygon.push_back({{packet.vertices.x[v], packet.vertices.y[v], packet.vertices.z[v]}, {{corner, 1}}});
      }
      if (kind == 1) {
        Vec3 normal = face_normals[row];
        bool clipped = false;
        polygon = clip_polygon(std::move(polygon), planes, clipped);
        if (polygon.size() >= 3 && table.opacity[row] > 0) {
          Vec3 area = area_vector(polygon, projection);
          if (std::sqrt(dot(area, area)) > projection.resolved.tolerance*projection.resolved.tolerance) {
            emit(table, row, Kind::face, polygon, clipped, normal);
            pieces.back().solid=solid_components[row];
          }
        }
      } else if (kind == 2) {
        bool area_path=polygon.size()>2 && packet.polylines.width_mm[row]>0 && table.partition[row]==1;
        const std::size_t centre_planes=planes.size()-(projection.camera.clip ? 4:0);
        const std::size_t slab_begin=projection.camera.clip_data ? 6:0;
        bool inside_slab=true;
        for(const auto& v:polygon) for(std::size_t k=slab_begin;k<centre_planes;++k)
          inside_slab=inside_slab && signed_distance(planes[k],v.xyz)>=0;
        bool varying=false;
        for(const auto& v:polygon) varying=varying || std::abs(projection.camera_point(v.xyz)[2]-
          projection.camera_point(polygon.front().xyz)[2])>projection.resolved.tolerance;
        if(area_path && varying && table.opacity[row]>0) {
          if(!inside_slab) {
            Polygon run;double cumulative=0,first=0;bool original_start=false;
            auto flush=[&](bool original_end) {
              if(run.size()<2) {run.clear();return;}
              emit(table,row,Kind::polyline,run,true,{0,0,0},first,cumulative,
                   original_start && packet.polylines.arrow_start[row],original_end && packet.polylines.arrow_end[row]);
              Piece source=std::move(pieces.back());pieces.pop_back();
              source.cap_start=original_start;source.cap_end=original_end;
              for(auto& cell:stroke_area(std::move(source),packet,projection,budget)) pieces.push_back(std::move(cell));
              run.clear();
            };
            for(std::size_t j=1;j<polygon.size();++j) {
              const auto& a=polygon[j-1];const auto& b=polygon[j];
              auto slab=clip_segment_range(a,b,planes,slab_begin,centre_planes);
              if(slab.first>slab.second) {flush(false);continue;}
              auto start=interpolate(a,b,slab.first),end=interpolate(a,b,slab.second);
              if(run.empty()) {first=cumulative;original_start=j==1 && slab.first==0;run.push_back(start);}
              run.push_back(end);cumulative+=projected_length(start,end,projection);
              if(slab.second<1 || j+1==polygon.size()) flush(j+1==polygon.size() && slab.second==1);
            }
            continue;
          }
          double length=0;
          for(std::size_t j=1;j<polygon.size();++j) length+=projected_length(polygon[j-1],polygon[j],projection);
          emit(table,row,Kind::polyline,polygon,false,{0,0,0},0,length,
               packet.polylines.arrow_start[row],packet.polylines.arrow_end[row]);
          Piece source=std::move(pieces.back());pieces.pop_back();
          for(auto& cell:stroke_area(std::move(source),packet,projection,budget)) pieces.push_back(std::move(cell));
          continue;
        }
        const bool ordered_partition=table.partition[row]!=1;
        bool complete_path=(polygon.size()>2 || ordered_partition) && packet.polylines.width_mm[row]>0;
        double depth=projection.camera_point(polygon[0].xyz)[2];
        const std::size_t slab_planes=planes.size()-(projection.camera.clip ? 4:0);
        for(const auto& v:polygon) {
          complete_path=complete_path && (ordered_partition ||
            std::abs(projection.camera_point(v.xyz)[2]-depth)<=projection.resolved.tolerance);
          for(std::size_t j=0;j<slab_planes;++j) complete_path=complete_path && signed_distance(planes[j],v.xyz)>=0;
        }
        if(complete_path && table.opacity[row]>0) {
          double length=0;
          for(std::size_t j=1;j<polygon.size();++j) length+=projected_length(polygon[j-1],polygon[j],projection);
          double extent=packet.polylines.width_mm[row]*.5;
          if(packet.polylines.join[row]==static_cast<Index>(Join::mitre)) extent*=packet.polylines.mitre_limit[row];
          Bounds2 bounds{R_PosInf,R_PosInf,R_NegInf,R_NegInf};
          for(const auto& v:polygon) {
            auto s=projection.npc(projection.camera_point(v.xyz));
            for(int k=0;k<2;++k) {bounds[k]=std::min(bounds[k],s[k]);bounds[k+2]=std::max(bounds[k+2],s[k]);}
          }
          bounds[0]-=extent/projection.viewport.width_mm;bounds[2]+=extent/projection.viewport.width_mm;
          bounds[1]-=extent/projection.viewport.height_mm;bounds[3]+=extent/projection.viewport.height_mm;
          Polygon carrier;
          for(auto xy:{Point2{bounds[0],bounds[1]},Point2{bounds[2],bounds[1]},Point2{bounds[2],bounds[3]},Point2{bounds[0],bounds[3]}})
            carrier.push_back(screen_vertex(xy[0],xy[1],depth,projection));
          bool clipped=false;
          std::vector<Plane> screen_planes;
          if(projection.camera.clip) screen_planes.assign(planes.end()-4,planes.end());
          carrier=clip_polygon(std::move(carrier),screen_planes,clipped);
          if(carrier.size()>=3) {
            emit(table,row,Kind::polyline,carrier,clipped,projection.resolved.forward,0,length,
                 packet.polylines.arrow_start[row],packet.polylines.arrow_end[row]);
            pieces.back().anchor=polygon;
            budget.add(192.0*polygon.size());
          }
          continue;
        }
        double cumulative = 0;
        for (std::size_t j = 1; j < polygon.size(); ++j) {
          const Vertex& a = polygon[j-1]; const Vertex& b = polygon[j];
          std::size_t centre_planes=planes.size() -
            (packet.polylines.width_mm[row]>0 && projection.camera.clip ? 4 : 0);
          auto interval=clip_segment_range(a,b,planes,0,centre_planes);
          double start=interval.first,end=interval.second;
          // Dash arclength is measured after near/far clipping, before viewport clipping.
          std::size_t slab_begin=projection.camera.clip_data ? 6 : 0;
          auto slab=clip_segment_range(a,b,planes,slab_begin,slab_begin+2);
          double length = 0, first = cumulative, last = cumulative;
          if (slab.first <= slab.second) {
            Vertex origin=interpolate(a,b,slab.first);
            length=projected_length(origin,interpolate(a,b,slab.second),projection);
            if (start <= end) {
              first+=projected_length(origin,interpolate(a,b,start),projection);
              last+=projected_length(origin,interpolate(a,b,end),projection);
            }
          }
          if (start <= end && table.opacity[row] > 0) {
            Polygon segment{interpolate(a,b,start), interpolate(a,b,end)};
            emit(table, row, Kind::polyline, segment,
              start > 0 || end < 1, {0,0,0}, first, last,
              j == 1 && start == 0 && packet.polylines.arrow_start[row],
              j+1 == polygon.size() && end == 1 && packet.polylines.arrow_end[row]);
            if(packet.polylines.width_mm[row]>0) {
              Piece source=std::move(pieces.back());pieces.pop_back();
              bool caps=packet.polylines.cap[row]!=static_cast<Index>(Cap::butt) &&
                std::abs(projection.camera_point(source.polygon[0].xyz)[2]-
                  projection.camera_point(source.polygon[1].xyz)[2])>projection.resolved.tolerance;
              for(int sector=(caps ? -1:0);sector<=(caps ? 1:0);++sector) {
                Piece piece=source;
                Polygon bounds=stroke_bounds(piece,packet,projection,piece.normal,sector);
                if(!bounds.empty()) {
                  piece.anchor=piece.polygon;
                  std::vector<Plane> screen_planes;
                  if(projection.camera.clip) screen_planes.assign(planes.end()-4,planes.end());
                  piece.polygon=clip_polygon(std::move(bounds),screen_planes,piece.clipped);
                  if(caps && !piece.clipped) piece.whole_stroke_parts=3;
                  piece.clipped=piece.clipped || caps;
                  if(piece.polygon.size()<3) continue;
                } else if(sector!=0) continue;
                if(sector!=0) budget.piece(piece.polygon.size()+piece.anchor.size());
                pieces.push_back(std::move(piece));
              }
            }
          }
          cumulative += length;
        }
      } else {
        bool visible = table.opacity[row] > 0;
        std::size_t anchor_planes = planes.size() - (projection.camera.clip ? 4 : 0);
        for (std::size_t j=0;j<anchor_planes;++j)
          visible = visible && signed_distance(planes[j],polygon[0].xyz)>=0;
        if (visible) {
          emit(table, row, static_cast<Kind>(kind), polygon, false);
          if (kind==3) {
            pieces.back().anchor=polygon;
            pieces.back().polygon=marker_bounds(packet,row,polygon[0],projection,pieces.back().clipped);
            if(pieces.back().polygon.empty()) pieces.pop_back();
          } else if(projection.camera.clip) {
            if(packet.text.width_mm[row]>0 && packet.text.height_mm[row]>0) {
              std::vector<Plane> viewport_planes(planes.end()-4,planes.end());
              Polygon rectangle=clip_polygon(text_bounds(packet,row,polygon[0],projection),
                                             viewport_planes,pieces.back().clipped);
              if(rectangle.size()<3) pieces.pop_back();
              else if(pieces.back().clipped) pieces.back().mask=std::move(rectangle);
            } else {
              auto point=projection.npc(projection.camera_point(polygon[0].xyz));
              if(point[0]<0 || point[0]>1 || point[1]<0 || point[1]>1) pieces.pop_back();
            }
          }
        }
      }
    }
  }
  bool needs_coverage=false;
  for(const Piece& piece:pieces) if(piece.source->partition[piece.row]==1)
    needs_coverage=needs_coverage || piece.kind==Kind::text ||
      piece.kind==Kind::polyline;
  if(needs_coverage) {
    CoverageIndex coverage(pieces,projection);
    pieces.erase(std::remove_if(pieces.begin(),pieces.end(),[&](const Piece& piece) {
      return piece.kind==Kind::text && piece.source->partition[piece.row]==1 &&
        coverage.hidden(piece.polygon[0]);
    }),pieces.end());
    pieces=coverage.resolve_lines(std::move(pieces),packet,budget);
  }
  for(Piece& piece:pieces) if(piece.kind==Kind::text &&
      packet.text.width_mm[piece.row]>0 && packet.text.height_mm[piece.row]>0) {
    piece.anchor=piece.polygon;
    piece.polygon=piece.mask.empty() ? text_bounds(packet,piece.row,piece.anchor[0],projection) :
      std::move(piece.mask);
    piece.mask.clear();
  }
  std::vector<Piece> partitions[3];
  for (Piece& piece : pieces)
    partitions[piece.source->partition[piece.row]].push_back(std::move(piece));
  pieces.clear(); pieces.shrink_to_fit();
  PartitionStats stats;
  partitions[1] = order_scene(std::move(partitions[1]), packet, projection, stats, budget);
#ifdef GGPLOT3D_ENGINE_METRICS
  Rcpp::Rcout << "graph_metrics candidates=" << stats.pairs << " classifications="
              << stats.classifications << " splits=" << stats.splits << " nodes=" << stats.nodes
              << " edges=" << stats.edges << " sccs=" << stats.components
              << " cyclic_sccs=" << stats.cyclic << " largest_scc=" << stats.largest_scc
              << " intersections=" << budget.intersections << " estimated_bytes=" << budget.estimated << '\n';
  Rcpp::Rcout << "scc_sizes";
  for(const auto& size:stats.scc_sizes) Rcpp::Rcout << ' ' << size.first << ':' << size.second;
  Rcpp::Rcout << '\n';
#endif
  for (int partition : {0, 2})
    std::stable_sort(partitions[partition].begin(), partitions[partition].end(),
      [](const Piece& a, const Piece& b) {
        if (a.source->layer_id[a.row] != b.source->layer_id[b.row])
          return a.source->layer_id[a.row] < b.source->layer_id[b.row];
        return a.serial < b.serial;
      });
  for (auto& partition : partitions)
    for (Piece& piece : partition) {
      if(piece.kind==Kind::marker && piece.clipped && piece.polygon.size()>=3) {
        int coverage=marker_coverage(piece,packet,projection);
        if(coverage==0) continue;
        piece.mask_required=coverage==2;
      }
      pieces.push_back(std::move(piece));
    }
  std::size_t kept=0;
  for(std::size_t i=0;i<pieces.size();) {
    std::size_t count=pieces[i].whole_stroke_parts;
    bool complete=count>0 && i+count<=pieces.size();
    for(std::size_t j=1;complete && j<count;++j)
      complete=pieces[i+j].serial==pieces[i].serial && pieces[i+j].whole_stroke_parts==count;
    if(complete) {
      pieces[i].polygon=std::move(pieces[i].anchor);
      pieces[i].anchor.clear();
      pieces[i].clipped=false;
    }
    if(kept!=i) pieces[kept]=std::move(pieces[i]);
    ++kept;i+=complete ? count:1;
  }
  pieces.resize(kept);
  std::vector<std::size_t> storage(pieces.size());
  std::iota(storage.begin(), storage.end(), 0);
  std::stable_sort(storage.begin(), storage.end(), [&](std::size_t a, std::size_t b) {
    return pieces[a].serial < pieces[b].serial;
  });
  out.draw_order.resize(pieces.size());
  std::vector<bool> emitted_start(packet.polylines.count.size(),false),emitted_end(packet.polylines.count.size(),false);
  for (std::size_t i : storage) {
    const Piece& piece = pieces[i];
    bool start=piece.arrow_start && (piece.anchor.empty() || screen_contains(piece.polygon,piece.anchor.front(),projection));
    bool end=piece.arrow_end && (piece.anchor.empty() || screen_contains(piece.polygon,piece.anchor.back(),projection));
    if(piece.kind==Kind::polyline) {
      start=start && !emitted_start[piece.row];end=end && !emitted_end[piece.row];
      emitted_start[piece.row]=emitted_start[piece.row] || start;
      emitted_end[piece.row]=emitted_end[piece.row] || end;
    }
    out.draw_order[i] = out.fragments.count.size();
    append_fragment(out, *piece.source, piece.row, piece.kind,
                    piece.anchor.empty() ? piece.polygon : piece.anchor,
                    projection, piece.clipped, piece.kind==Kind::face ? piece.normal : Vec3{0,0,0}, piece.path_start,
                    piece.path_end,
                    start,end);
    out.fragments.hidden.back()=piece.hidden;
    if (!piece.anchor.empty() && piece.polygon.size()>=3 && piece.clipped && piece.mask_required)
      append_mask(out,piece.polygon,projection);
    if(!piece.mask.empty()) append_mask(out,piece.mask,projection);
  }
  return out;
}


} }
