#pragma once
#include "common.h"

namespace ggplot3d { namespace engine {
static inline double dot(const Vec3& a, const Vec3& b) {
  return a[0]*b[0] + a[1]*b[1] + a[2]*b[2];
}
static inline Vec3 cross(const Vec3& a, const Vec3& b) {
  return {a[1]*b[2]-a[2]*b[1], a[2]*b[0]-a[0]*b[2], a[0]*b[1]-a[1]*b[0]};
}
static inline Vec3 subtract(const Vec3& a, const Vec3& b) {
  return {a[0]-b[0], a[1]-b[1], a[2]-b[2]};
}

struct Projection {
  Camera camera;
  Viewport viewport;
  ResolvedCamera resolved;
  Vec3 scale, target;
  double sx, sy;

  Projection(const Camera& c, const Viewport& v) : camera(c), viewport(v) {
    const double largest = *std::max_element(c.aspect.begin(), c.aspect.end());
    double radius2 = 0;
    for (int k = 0; k < 3; ++k) {
      scale[k] = c.aspect[k] / largest;
      require(scale[k] > 0, "unrepresentable camera aspect");
      resolved.model_scale[k] = scale[k];
      target[k] = c.target[k] * scale[k];
      double extent = std::max(std::abs(target[k]), std::abs(scale[k]-target[k]));
      radius2 += extent * extent;
    }
    const double radius = std::sqrt(radius2);
    require(std::isfinite(radius) && radius > 0, "unrepresentable camera target or aspect");
    resolved.tolerance = 64 * std::numeric_limits<double>::epsilon() * std::max(1.0, radius);
    double az = std::remainder(c.azimuth, 360.0) * pi/180;
    double el = c.elevation * pi/180, roll = std::remainder(c.roll, 360.0) * pi/180;
    Vec3 outward{std::cos(el)*std::cos(az), std::cos(el)*std::sin(az), std::sin(el)};
    Vec3 right{-std::sin(az), std::cos(az), 0};
    Vec3 up{-std::sin(el)*std::cos(az), -std::sin(el)*std::sin(az), std::cos(el)};
    for (int k = 0; k < 3; ++k) {
      resolved.right[k] = std::cos(roll)*right[k] - std::sin(roll)*up[k];
      resolved.up[k] = std::sin(roll)*right[k] + std::cos(roll)*up[k];
      resolved.forward[k] = -outward[k];
    }
    const double ratio = v.width_mm/v.height_mm;
    const double half_angle = std::atan(std::tan(c.fov*pi/360) * std::min(1.0, ratio));
    resolved.distance = c.perspective ?
      (R_IsNA(c.distance) ? radius/std::sin(half_angle) + resolved.tolerance : c.distance) :
      2*radius + 1;
    resolved.near = R_IsNA(c.near) ? std::max(resolved.tolerance,
      resolved.distance - radius - resolved.tolerance) : c.near;
    resolved.far = R_IsNA(c.far) ? resolved.distance + radius + resolved.tolerance : c.far;
    require(std::isfinite(resolved.distance) && resolved.near < resolved.far,
            "resolved near must be less than far");
    resolved.view_height = 2*radius/std::min(1.0, ratio)/c.zoom;
    sy = c.perspective ? c.zoom/std::tan(c.fov*pi/360) : 2/resolved.view_height;
    sx = sy/ratio;
    require(std::isfinite(sx) && std::isfinite(sy) && sx > 0 && sy > 0,
            "unrepresentable projection scale");
    for (int k = 0; k < 3; ++k) resolved.eye[k] = target[k] + resolved.distance*outward[k];
    auto& view = resolved.view;
    for (int k = 0; k < 3; ++k) {
      view[k*4] = resolved.right[k]; view[k*4+1] = resolved.up[k];
      view[k*4+2] = resolved.forward[k];
    }
    view[12] = -dot(resolved.right, resolved.eye);
    view[13] = -dot(resolved.up, resolved.eye);
    view[14] = -dot(resolved.forward, resolved.eye); view[15] = 1;
    auto& proj = resolved.projection;
    proj[0] = sx; proj[5] = sy;
    double range = resolved.far-resolved.near;
    proj[10] = c.perspective ? resolved.far/range : 1/range;
    proj[14] = c.perspective ? -resolved.near*resolved.far/range : -resolved.near/range;
    proj[11] = c.perspective ? 1 : 0; proj[15] = c.perspective ? 0 : 1;
  }

  Vec3 model(const Vec3& p) const { return {p[0]*scale[0], p[1]*scale[1], p[2]*scale[2]}; }
  Vec3 camera_point(const Vec3& p) const {
    Vec3 delta = subtract(model(p), target);
    return {dot(delta, resolved.right), dot(delta, resolved.up),
            resolved.distance + dot(delta, resolved.forward)};
  }
  std::array<double, 2> npc(const Vec3& c) const {
    double w = camera.perspective ? c[2] : 1;
    require(w > 0, "projection requires positive eye distance");
    return {0.5 + 0.5*sx*c[0]/w, 0.5 + 0.5*sy*c[1]/w};
  }
};

struct Vertex {
  Vec3 xyz;
  std::vector<std::pair<Index, double>> weights;
};
using Polygon = std::vector<Vertex>;
struct Plane { Vec3 normal; double offset; };

static inline double signed_distance(const Plane& plane, const Vec3& p) {
  return dot(plane.normal, p) + plane.offset;
}
static inline Vertex interpolate(const Vertex& a, const Vertex& b, double t) {
  Vertex out;
  for (int k = 0; k < 3; ++k) out.xyz[k] = (1-t)*a.xyz[k] + t*b.xyz[k];
  out.weights = a.weights;
  for (auto& weight : out.weights) weight.second *= 1-t;
  for (const auto& weight : b.weights) {
    auto it = std::find_if(out.weights.begin(), out.weights.end(),
      [&](const auto& old) { return old.first == weight.first; });
    if (it == out.weights.end()) out.weights.push_back({weight.first, t*weight.second});
    else it->second += t*weight.second;
  }
  return out;
}


} }
