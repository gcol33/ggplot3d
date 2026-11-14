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
    Vec3 standard_outward{std::cos(el)*std::cos(az), std::cos(el)*std::sin(az), std::sin(el)};
    Vec3 standard_right{-std::sin(az), std::cos(az), 0};
    Vec3 standard_up{-std::sin(el)*std::cos(az), -std::sin(el)*std::sin(az), std::cos(el)};
    // Angles are defined about the vertical axis; a cyclic permutation, a
    // proper rotation, carries the z-up frame onto the chosen vertical axis.
    Vec3 outward{}, right{}, up{};
    for (int k = 0; k < 3; ++k) {
      int axis = (k + c.up_axis + 1) % 3;
      outward[axis] = standard_outward[k]; right[axis] = standard_right[k]; up[axis] = standard_up[k];
    }
    for (int k = 0; k < 3; ++k) {
      resolved.right[k] = std::cos(roll)*right[k] - std::sin(roll)*up[k];
      resolved.up[k] = std::sin(roll)*right[k] + std::cos(roll)*up[k];
      resolved.forward[k] = -outward[k];
    }
    require(!c.perspective || (c.shear[0] == 0 && c.shear[1] == 0), "oblique shear requires a parallel projection");
    for (int k = 0; k < 3; ++k) {
      resolved.ray[k] = resolved.forward[k] - c.shear[0]*resolved.right[k] - c.shear[1]*resolved.up[k];
      resolved.screen_right[k] = resolved.right[k] + c.shear[0]*resolved.forward[k];
      resolved.screen_up[k] = resolved.up[k] + c.shear[1]*resolved.forward[k];
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
    // A parallel eye sits on the sight ray through the target.
    for (int k = 0; k < 3; ++k) resolved.eye[k] = target[k] - resolved.distance*resolved.ray[k];
    auto& view = resolved.view;
    for (int k = 0; k < 3; ++k) {
      view[k*4] = resolved.screen_right[k]; view[k*4+1] = resolved.screen_up[k];
      view[k*4+2] = resolved.forward[k];
    }
    view[12] = -dot(resolved.screen_right, resolved.eye);
    view[13] = -dot(resolved.screen_up, resolved.eye);
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
    return {dot(delta, resolved.screen_right), dot(delta, resolved.screen_up),
            resolved.distance + dot(delta, resolved.forward)};
  }
  std::array<double, 2> npc(const Vec3& c) const {
    double w = camera.perspective ? c[2] : 1;
    require(w > 0, "projection requires positive eye distance");
    return {0.5 + 0.5*sx*c[0]/w, 0.5 + 0.5*sy*c[1]/w};
  }
};

// A clipped vertex is a homogeneous point (xyz, w) of the box after camera pre;
// every vertex the clipper has not produced from input corners has w = 1.
struct Vertex {
  Vec3 xyz;
  std::vector<std::pair<Index, double>> weights;
  double w = 1;
};
using Polygon = std::vector<Vertex>;
// The half space normal . xyz + offset * w >= 0 (Blinn and Newell 1978).
struct Plane { Vec3 normal; double offset; };

static inline double signed_distance(const Plane& plane, const Vertex& v) {
  return dot(plane.normal, v.xyz) + plane.offset*v.w;
}
static inline Vertex interpolate(const Vertex& a, const Vertex& b, double t) {
  Vertex out;
  for (int k = 0; k < 3; ++k) out.xyz[k] = (1-t)*a.xyz[k] + t*b.xyz[k];
  out.w = (1-t)*a.w + t*b.w;
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

// Euclidean box fractions of a clipped vertex. Weights are divided with the
// coordinates, so that sum weight * w over the source corners is one.
static inline Vertex dehomogenised(Vertex v) {
  require(v.w > 0, "unbounded geometry: a point at infinity survives clipping");
  if (v.w != 1) {
    for (double& x : v.xyz) x /= v.w;
    for (auto& weight : v.weights) weight.second /= v.w;
    v.w = 1;
  }
  return v;
}
static inline void dehomogenise(Polygon& polygon) {
  for (Vertex& v : polygon) v = dehomogenised(std::move(v));
}
// True when some vertex lies at infinity or, after camera pre, behind it (w < 0);
// such a chain is divided only after clipping against w >= 0.
static inline bool outside_chart(const Polygon& polygon) {
  for (const Vertex& v : polygon) if (!(v.w > 0)) return true;
  return false;
}

// Packet vertices mapped by the camera pre matrix, still homogeneous; the map
// may send points to w < 0, which the w >= 0 clip removes.
using Point4 = std::array<double, 4>;
static inline std::vector<Point4> homogeneous_points(const Vertices& v, const Projection& p) {
  const auto& m = p.camera.pre;
  bool identity = true;
  for (int i = 0; i < 16; ++i) identity = identity && m[i] == (i % 5 == 0 ? 1 : 0);
  std::vector<Point4> out(v.x.size());
  for (std::size_t i = 0; i < out.size(); ++i) {
    const Point4 x{v.x[i], v.y[i], v.z[i], v.w[i]};
    if (identity) {out[i] = x; continue;}
    for (int r = 0; r < 4; ++r) out[i][r] = m[r]*x[0] + m[4+r]*x[1] + m[8+r]*x[2] + m[12+r]*x[3];
    require(std::isfinite(out[i][0]) && std::isfinite(out[i][1]) && std::isfinite(out[i][2]) &&
            std::isfinite(out[i][3]), "camera pre maps a vertex to a non-finite point");
    require(out[i][3] > 0 || out[i][0] != 0 || out[i][1] != 0 || out[i][2] != 0,
            "camera pre maps a vertex to the zero vector");
  }
  return out;
}
static inline Vertex corner_vertex(const std::vector<Point4>& points, Index corner, Index vertex) {
  const Point4& x = points[vertex];
  return {{x[0], x[1], x[2]}, {{corner, 1}}, x[3]};
}


} }
