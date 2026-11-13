#pragma once
#include "projection.h"

namespace ggplot3d { namespace engine {
static inline void append_fragment(CompiledScene& out, const PrimitiveTable& source, Index row, Kind kind,
                     const Polygon& polygon, const Projection& projection, bool clipped,
                     Vec3 normal = {0, 0, 0}, double path_start = 0, double path_end = 0,
                     bool arrow_start = false, bool arrow_end = false) {
  auto& f = out.fragments;
  require(out.vertices.x.size() + polygon.size() <= std::numeric_limits<Index>::max() &&
          f.count.size() < std::numeric_limits<Index>::max(), "fragment index capacity exceeded");
  f.offset.push_back(out.vertices.x.size()); f.count.push_back(polygon.size());
  double lo = R_PosInf, hi = R_NegInf, mean = 0;
  for (const Vertex& vertex : polygon) {
    require(out.weights.vertex.size()+vertex.weights.size()<=std::numeric_limits<Index>::max(),
            "provenance index capacity exceeded");
    Index index = out.vertices.x.size();
    Vec3 camera = projection.camera_point(vertex.xyz);
    auto screen = projection.npc(camera);
    require(std::isfinite(screen[0]) && std::isfinite(screen[1]) && std::isfinite(camera[2]),
            "unrepresentable projected coordinate");
    out.vertices.x.push_back(vertex.xyz[0]); out.vertices.y.push_back(vertex.xyz[1]);
    out.vertices.z.push_back(vertex.xyz[2]); out.vertices.x_camera.push_back(camera[0]);
    out.vertices.y_camera.push_back(camera[1]); out.vertices.depth.push_back(camera[2]);
    out.vertices.x_npc.push_back(screen[0]); out.vertices.y_npc.push_back(screen[1]);
    out.vertices.clip_w.push_back(projection.camera.perspective ? camera[2] : 1);
    for (const auto& weight : vertex.weights) {
      if (weight.second == 0) continue;
      out.weights.vertex.push_back(index); out.weights.corner.push_back(weight.first);
      out.weights.weight.push_back(weight.second);
    }
    lo = std::min(lo, camera[2]); hi = std::max(hi, camera[2]);
    mean += camera[2]/polygon.size();
  }
  f.primitive_id.push_back(source.primitive_id[row]); f.layer_id.push_back(source.layer_id[row]);
  f.source_row.push_back(source.source_row[row]); f.style_id.push_back(source.style_id[row]);
  f.role.push_back(source.role[row]); f.partition.push_back(source.partition[row]);
  f.opacity.push_back(source.opacity[row]); f.fragment_id.push_back(f.fragment_id.size()+1);
  f.kind.push_back(static_cast<Index>(kind)); f.parent_primitive_id.push_back(source.primitive_id[row]);
  f.source_index.push_back(row); f.mask_offset.push_back(0); f.mask_count.push_back(0);
  f.depth_min.push_back(lo); f.depth_max.push_back(hi); f.depth_centroid.push_back(mean);
  f.normal_x.push_back(normal[0]); f.normal_y.push_back(normal[1]); f.normal_z.push_back(normal[2]);
  f.path_start_mm.push_back(path_start); f.path_end_mm.push_back(path_end);
  f.clipped.push_back(clipped); f.hidden.push_back(false);
  f.arrow_start.push_back(arrow_start); f.arrow_end.push_back(arrow_end);
}


} }
