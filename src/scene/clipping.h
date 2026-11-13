#pragma once
#include "projection.h"

namespace ggplot3d { namespace engine {
static inline std::vector<Plane> clipping_planes(const Projection& p) {
  std::vector<Plane> planes;
  if (p.camera.clip_data) {
    for (int k = 0; k < 3; ++k) {
      Vec3 n{0, 0, 0}; n[k] = 1; planes.push_back({n, 0});
      n[k] = -1; planes.push_back({n, 1});
    }
  }
  auto camera_plane = [&](const Vec3& n, double offset) {
    Vec3 origin = p.camera_point({0, 0, 0}), coefficients{};
    for (int k = 0; k < 3; ++k) {
      coefficients[k] = p.scale[k]*(n[0]*p.resolved.screen_right[k] +
        n[1]*p.resolved.screen_up[k] + n[2]*p.resolved.forward[k]);
    }
    planes.push_back({coefficients, dot(n, origin)+offset});
  };
  camera_plane({0, 0, 1}, -p.resolved.near);
  camera_plane({0, 0, -1}, p.resolved.far);
  if (p.camera.clip) {
    double depth = p.camera.perspective ? 1 : 0;
    double offset = p.camera.perspective ? 0 : 1;
    camera_plane({p.sx, 0, depth}, offset); camera_plane({-p.sx, 0, depth}, offset);
    camera_plane({0, p.sy, depth}, offset); camera_plane({0, -p.sy, depth}, offset);
  }
  return planes;
}

static inline Polygon clip_polygon(Polygon polygon, const std::vector<Plane>& planes, bool& clipped) {
  for (const Plane& plane : planes) {
    if (polygon.empty()) break;
    bool inside = true;
    for (const Vertex& v : polygon) inside = inside && signed_distance(plane, v.xyz) >= 0;
    if (inside) continue;
    Polygon out;
    Vertex previous = polygon.back();
    double dp = signed_distance(plane, previous.xyz);
    for (const Vertex& current : polygon) {
      double dc = signed_distance(plane, current.xyz);
      if ((dp < 0 && dc > 0) || (dp > 0 && dc < 0))
        out.push_back(interpolate(previous, current, dp/(dp-dc)));
      if (dc >= 0) out.push_back(current); else clipped = true;
      previous = current; dp = dc;
    }
    polygon = std::move(out);
  }
  return polygon;
}

static inline std::pair<double,double> clip_segment_range(const Vertex& a,const Vertex& b,
                                                         const std::vector<Plane>& planes,
                                                         std::size_t begin,std::size_t end) {
  double first=0,last=1;
  for(std::size_t i=begin;i<end;++i) {
    double da=signed_distance(planes[i],a.xyz),db=signed_distance(planes[i],b.xyz);
    if(da<0 && db<0) return {0,-1};
    if(da<0) first=std::max(first,da/(da-db));
    if(db<0) last=std::min(last,da/(da-db));
  }
  return {first,last};
}

static inline Vec3 area_vector(const Polygon& polygon, const Projection& projection) {
  Vec3 normal{0, 0, 0};
  const Vec3 origin = projection.model(polygon[0].xyz);
  for (std::size_t i = 1; i+1 < polygon.size(); ++i) {
    Vec3 part = cross(subtract(projection.model(polygon[i].xyz), origin),
                      subtract(projection.model(polygon[i+1].xyz), origin));
    for (int k = 0; k < 3; ++k) normal[k] += part[k];
  }
  return normal;
}

static inline Vec3 face_normal(const Polygon& polygon, const Projection& projection) {
  Vec3 normal = area_vector(polygon, projection);
  const Vec3 origin = projection.model(polygon[0].xyz);
  double length = std::sqrt(dot(normal, normal));
  double extent=0;
  for(const Vertex& v:polygon) {
    Vec3 delta=subtract(projection.model(v.xyz),origin);
    extent=std::max(extent,std::sqrt(dot(delta,delta)));
  }
  require(std::isfinite(length) && length > projection.resolved.tolerance*extent, "degenerate face");
  for (double& value : normal) value /= length;
  for (std::size_t i = 0; i < polygon.size(); ++i) {
    Vec3 a = projection.model(polygon[i].xyz);
    Vec3 b = projection.model(polygon[(i+1)%polygon.size()].xyz);
    require(std::abs(dot(subtract(a, origin), normal)) <= projection.resolved.tolerance,
            "faces must be planar");
    Vec3 edge = subtract(b, a);
    require(std::sqrt(dot(edge, edge)) > projection.resolved.tolerance,
            "face rings must have distinct consecutive corners and implicit closure");
    for (const Vertex& vertex : polygon) {
      require(dot(cross(edge, subtract(projection.model(vertex.xyz), a)), normal) >=
              -projection.resolved.tolerance*std::sqrt(dot(edge,edge)), "faces must be convex simple rings");
    }
  }
  return normal;
}


} }
