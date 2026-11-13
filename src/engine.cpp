#include <Rcpp.h>
#include "scene/contract.h"
#include <algorithm>
#include <cmath>
#include <limits>
#include <numeric>
#include <set>
#include <string>
#include <tuple>

namespace {
using namespace ggplot3d;
using Vec3 = std::array<double, 3>;
constexpr double pi = 3.14159265358979323846;

void require(bool condition, const std::string& message) {
  if (!condition) Rcpp::stop(message);
}

void fields(const Rcpp::List& x, std::initializer_list<const char*> expected) {
  require(x.size() == static_cast<R_xlen_t>(expected.size()), "invalid table schema");
  Rcpp::CharacterVector names = x.names();
  require(names.size() == x.size(), "all fields must be named");
  int i = 0;
  for (const char* name : expected) {
    require(names[i] != NA_STRING && Rcpp::as<std::string>(names[i]) == name,
            std::string("expected field: ") + name);
    ++i;
  }
}

template <int RTYPE, typename T>
std::vector<T> column(const Rcpp::List& table, const char* name, int n = -1) {
  SEXP value = table[name];
  require(TYPEOF(value) == RTYPE && !Rf_isObject(value) && Rf_isNull(Rf_getAttrib(value, R_DimSymbol)),
          std::string("invalid column type: ") + name);
  Rcpp::Vector<RTYPE> x(value);
  require(x.size() <= std::numeric_limits<Index>::max(), "scene exceeds 32-bit index capacity");
  require(n < 0 || x.size() == n, std::string("column length mismatch: ") + name);
  std::vector<T> out;
  out.reserve(x.size());
  for (auto v : x) {
    require(!Rcpp::Vector<RTYPE>::is_na(v) && std::isfinite(static_cast<double>(v)),
            std::string("non-finite column: ") + name);
    out.push_back(static_cast<T>(v));
  }
  return out;
}

std::vector<Index> integers(const Rcpp::List& x, const char* name, int n = -1) {
  return column<INTSXP, Index>(x, name, n);
}
std::vector<double> doubles(const Rcpp::List& x, const char* name, int n = -1) {
  return column<REALSXP, double>(x, name, n);
}
std::vector<bool> logicals(const Rcpp::List& x, const char* name, int n = -1) {
  return column<LGLSXP, bool>(x, name, n);
}
double scalar(const Rcpp::List& x, const char* name, bool automatic = false) {
  SEXP value = x[name];
  require(TYPEOF(value) == REALSXP && Rf_xlength(value) == 1,
          std::string("expected double scalar: ") + name);
  const double v = Rcpp::as<double>(value);
  require(std::isfinite(v) || (automatic && R_IsNA(v)),
          std::string("invalid scalar: ") + name);
  return v;
}
Index integer_scalar(const Rcpp::List& x, const char* name) {
  return integers(x, name, 1)[0];
}

PrimitiveTable read_common(const Rcpp::List& x, int minimum, int nc, int styles,
                           std::set<Index>& ids) {
  PrimitiveTable p;
  p.primitive_id = integers(x, "primitive_id");
  int n = p.primitive_id.size();
  p.layer_id = integers(x, "layer_id", n);
  p.source_row = integers(x, "source_row", n);
  p.style_id = integers(x, "style_id", n);
  p.offset = integers(x, "offset", n);
  p.count = integers(x, "count", n);
  p.role = integers(x, "role", n);
  p.partition = integers(x, "partition", n);
  p.opacity = doubles(x, "opacity", n);
  for (int i = 0; i < n; ++i) {
    require(p.primitive_id[i] > 0 && ids.insert(p.primitive_id[i]).second,
            "primitive_id must be positive and globally unique");
    require(p.layer_id[i] >= 0 && p.source_row[i] >= 0 &&
            p.style_id[i] >= 1 && p.style_id[i] <= styles &&
            p.role[i] >= 0 && p.role[i] <= 3 &&
            p.partition[i] >= 0 && p.partition[i] <= 2 &&
            p.opacity[i] >= 0 && p.opacity[i] <= 1, "invalid primitive metadata");
    require(p.offset[i] >= 1 && p.count[i] >= minimum &&
            static_cast<double>(p.offset[i]) + p.count[i] - 1 <= nc,
            "invalid primitive corner range");
    --p.offset[i];
    --p.style_id[i];
  }
  return p;
}

ScenePacket read_packet(const Rcpp::List& x) {
  fields(x, {"schema_version", "panel_id", "style_count", "vertices", "corners",
             "faces", "polylines", "markers", "text", "footprints", "footprint_vertices"});
  ScenePacket p;
  p.schema_version = integer_scalar(x, "schema_version");
  p.panel_id = integer_scalar(x, "panel_id");
  p.style_count = integer_scalar(x, "style_count");
  require(p.schema_version == scene_schema_version, "unsupported scene schema version");
  require(p.panel_id > 0 && p.style_count >= 0, "invalid panel or style count");
  Rcpp::List v = x["vertices"], c = x["corners"];
  fields(v, {"x", "y", "z"});
  p.vertices.x = doubles(v, "x");
  int nv = p.vertices.x.size();
  p.vertices.y = doubles(v, "y", nv);
  p.vertices.z = doubles(v, "z", nv);
  fields(c, {"vertex", "source_row"});
  p.corners.vertex = integers(c, "vertex");
  int nc = p.corners.vertex.size();
  p.corners.source_row = integers(c, "source_row", nc);
  for (int i = 0; i < nc; ++i) {
    require(p.corners.vertex[i] >= 1 && p.corners.vertex[i] <= nv &&
            p.corners.source_row[i] >= 0, "invalid corner references");
    --p.corners.vertex[i];
  }
  std::set<Index> ids;
  Rcpp::List f = x["faces"], l = x["polylines"], m = x["markers"], t = x["text"];
  fields(f, {"primitive_id", "layer_id", "source_row", "style_id", "offset", "count",
             "role", "partition", "opacity"});
  fields(l, {"primitive_id", "layer_id", "source_row", "style_id", "offset", "count",
             "role", "partition", "opacity", "width_mm", "mitre_limit", "cap", "join",
             "hidden", "arrow_start", "arrow_end"});
  fields(m, {"primitive_id", "layer_id", "source_row", "style_id", "offset", "count",
             "role", "partition", "opacity", "footprint_id"});
  fields(t, {"primitive_id", "layer_id", "source_row", "style_id", "offset", "count",
             "role", "partition", "opacity", "width_mm", "height_mm", "hjust", "vjust", "angle"});
  static_cast<PrimitiveTable&>(p.faces) = read_common(f, 3, nc, p.style_count, ids);
  static_cast<PrimitiveTable&>(p.polylines) = read_common(l, 2, nc, p.style_count, ids);
  static_cast<PrimitiveTable&>(p.markers) = read_common(m, 1, nc, p.style_count, ids);
  static_cast<PrimitiveTable&>(p.text) = read_common(t, 1, nc, p.style_count, ids);
  int nl = p.polylines.count.size(), nm = p.markers.count.size(), nt = p.text.count.size();
  p.polylines.width_mm = doubles(l, "width_mm", nl);
  p.polylines.mitre_limit = doubles(l, "mitre_limit", nl);
  p.polylines.cap = integers(l, "cap", nl);
  p.polylines.join = integers(l, "join", nl);
  p.polylines.hidden = integers(l, "hidden", nl);
  p.polylines.arrow_start = logicals(l, "arrow_start", nl);
  p.polylines.arrow_end = logicals(l, "arrow_end", nl);
  for (int i = 0; i < nl; ++i) {
    require(p.polylines.width_mm[i] >= 0 && p.polylines.mitre_limit[i] >= 1 &&
            p.polylines.cap[i] >= 0 && p.polylines.cap[i] <= 2 &&
            p.polylines.join[i] >= 0 && p.polylines.join[i] <= 2 &&
            p.polylines.hidden[i] >= 0 && p.polylines.hidden[i] <= 2,
            "invalid polyline footprint or policy");
  }
  Rcpp::List fp = x["footprints"], fv = x["footprint_vertices"];
  fields(fp, {"kind", "offset", "count", "radius_x_mm", "radius_y_mm", "angle"});
  fields(fv, {"x_mm", "y_mm", "ring"});
  p.footprint_vertices.x_mm = doubles(fv, "x_mm");
  int nf = p.footprint_vertices.x_mm.size();
  p.footprint_vertices.y_mm = doubles(fv, "y_mm", nf);
  p.footprint_vertices.ring = integers(fv, "ring", nf);
  for (Index ring : p.footprint_vertices.ring) require(ring >= 1, "invalid footprint ring");
  p.footprints.kind = integers(fp, "kind");
  int np = p.footprints.kind.size();
  p.footprints.offset = integers(fp, "offset", np);
  p.footprints.count = integers(fp, "count", np);
  p.footprints.radius_x_mm = doubles(fp, "radius_x_mm", np);
  p.footprints.radius_y_mm = doubles(fp, "radius_y_mm", np);
  p.footprints.angle = doubles(fp, "angle", np);
  for (int i = 0; i < np; ++i) {
    require(p.footprints.kind[i] == 1 || p.footprints.kind[i] == 2, "invalid footprint kind");
    require(p.footprints.radius_x_mm[i] >= 0 && p.footprints.radius_y_mm[i] >= 0,
            "negative footprint radius");
    if (p.footprints.kind[i] == 1) {
      require(p.footprints.count[i] == 0 && p.footprints.offset[i] == 1, "invalid ellipse range");
    } else {
      require(p.footprints.offset[i] >= 1 && p.footprints.count[i] >= 3 &&
              static_cast<double>(p.footprints.offset[i]) + p.footprints.count[i] - 1 <= nf,
              "invalid polygon footprint range");
      std::set<Index> rings;
      Index start = p.footprints.offset[i]-1, end = start+p.footprints.count[i];
      while (start < end) {
        Index ring = p.footprint_vertices.ring[start], stop = start+1;
        while (stop < end && p.footprint_vertices.ring[stop] == ring) ++stop;
        require(stop-start >= 3 && rings.insert(ring).second,
                "footprint rings must be contiguous with at least three vertices");
        start = stop;
      }
    }
    --p.footprints.offset[i];
  }
  p.markers.footprint_id = integers(m, "footprint_id", nm);
  for (int i = 0; i < nm; ++i) {
    require(p.markers.count[i] == 1 && p.markers.footprint_id[i] >= 1 &&
            p.markers.footprint_id[i] <= np, "invalid marker anchor or footprint");
    --p.markers.footprint_id[i];
  }
  p.text.width_mm = doubles(t, "width_mm", nt);
  p.text.height_mm = doubles(t, "height_mm", nt);
  p.text.hjust = doubles(t, "hjust", nt);
  p.text.vjust = doubles(t, "vjust", nt);
  p.text.angle = doubles(t, "angle", nt);
  for (int i = 0; i < nt; ++i) {
    require(p.text.count[i] == 1 && p.text.width_mm[i] >= 0 && p.text.height_mm[i] >= 0,
            "invalid text anchor or measurements");
  }
  return p;
}

Camera read_camera(const Rcpp::List& x) {
  fields(x, {"projection", "azimuth", "elevation", "roll", "distance", "fov", "zoom",
             "target", "aspect", "near", "far", "clip_data", "clip"});
  Camera c;
  SEXP projection = x["projection"];
  require(TYPEOF(projection) == STRSXP && Rf_xlength(projection) == 1 &&
          STRING_ELT(projection, 0) != NA_STRING, "invalid projection");
  std::string mode = Rcpp::as<std::string>(projection);
  require(mode == "orthographic" || mode == "perspective", "invalid projection");
  c.perspective = mode == "perspective";
  c.azimuth = scalar(x, "azimuth"); c.elevation = scalar(x, "elevation");
  c.roll = scalar(x, "roll"); c.fov = scalar(x, "fov"); c.zoom = scalar(x, "zoom");
  require(std::abs(c.elevation) <= 90 && c.fov > 0 && c.fov < 180 && c.zoom > 0,
          "invalid camera angles or zoom");
  if (c.perspective) {
    c.distance = scalar(x, "distance", true);
    require(R_IsNA(c.distance) || c.distance > 0, "invalid perspective distance");
  } else {
    SEXP distance = x["distance"];
    require(TYPEOF(distance) == REALSXP && Rf_xlength(distance) == 1 &&
            Rcpp::as<double>(distance) == R_PosInf, "orthographic distance must be Inf");
    c.distance = R_PosInf;
  }
  auto target = doubles(x, "target", 3), aspect = doubles(x, "aspect", 3);
  for (int k = 0; k < 3; ++k) {
    require(aspect[k] > 0, "camera aspect must be positive");
    c.target[k] = target[k]; c.aspect[k] = aspect[k];
  }
  c.near = scalar(x, "near", true); c.far = scalar(x, "far", true);
  require((R_IsNA(c.near) || c.near > 0) && (R_IsNA(c.far) || c.far > 0) &&
          (R_IsNA(c.near) || R_IsNA(c.far) || c.near < c.far), "invalid clipping distances");
  c.clip_data = logicals(x, "clip_data", 1)[0]; c.clip = logicals(x, "clip", 1)[0];
  return c;
}

double dot(const Vec3& a, const Vec3& b) {
  return a[0]*b[0] + a[1]*b[1] + a[2]*b[2];
}
Vec3 cross(const Vec3& a, const Vec3& b) {
  return {a[1]*b[2]-a[2]*b[1], a[2]*b[0]-a[0]*b[2], a[0]*b[1]-a[1]*b[0]};
}
Vec3 subtract(const Vec3& a, const Vec3& b) {
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

double signed_distance(const Plane& plane, const Vec3& p) {
  return dot(plane.normal, p) + plane.offset;
}
Vertex interpolate(const Vertex& a, const Vertex& b, double t) {
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

std::vector<Plane> clipping_planes(const Projection& p) {
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
      coefficients[k] = p.scale[k]*(n[0]*p.resolved.right[k] +
        n[1]*p.resolved.up[k] + n[2]*p.resolved.forward[k]);
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

Polygon clip_polygon(Polygon polygon, const std::vector<Plane>& planes, bool& clipped) {
  for (const Plane& plane : planes) {
    if (polygon.empty()) break;
    Polygon out;
    Vertex previous = polygon.back();
    double dp = signed_distance(plane, previous.xyz);
    for (const Vertex& current : polygon) {
      double dc = signed_distance(plane, current.xyz);
      if ((dp < 0) != (dc < 0)) out.push_back(interpolate(previous, current, dp/(dp-dc)));
      if (dc >= 0) out.push_back(current); else clipped = true;
      previous = current; dp = dc;
    }
    polygon = std::move(out);
  }
  return polygon;
}

Vec3 area_vector(const Polygon& polygon, const Projection& projection) {
  Vec3 normal{0, 0, 0};
  const Vec3 origin = projection.model(polygon[0].xyz);
  for (std::size_t i = 1; i+1 < polygon.size(); ++i) {
    Vec3 part = cross(subtract(projection.model(polygon[i].xyz), origin),
                      subtract(projection.model(polygon[i+1].xyz), origin));
    for (int k = 0; k < 3; ++k) normal[k] += part[k];
  }
  return normal;
}

Vec3 face_normal(const Polygon& polygon, const Projection& projection) {
  Vec3 normal = area_vector(polygon, projection);
  const Vec3 origin = projection.model(polygon[0].xyz);
  double length = std::sqrt(dot(normal, normal));
  require(length > projection.resolved.tolerance, "degenerate face");
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
              -projection.resolved.tolerance, "faces must be convex simple rings");
    }
  }
  return normal;
}

void append_fragment(CompiledScene& out, const PrimitiveTable& source, Index row, Kind kind,
                     const Polygon& polygon, const Projection& projection, bool clipped,
                     Vec3 normal = {0, 0, 0}, double path_start = 0, double path_end = 0,
                     bool arrow_start = false, bool arrow_end = false) {
  auto& f = out.fragments;
  require(out.vertices.x.size() + polygon.size() <= std::numeric_limits<Index>::max() &&
          f.count.size() < std::numeric_limits<Index>::max(), "fragment index capacity exceeded");
  f.offset.push_back(out.vertices.x.size()); f.count.push_back(polygon.size());
  double lo = R_PosInf, hi = R_NegInf, mean = 0;
  for (const Vertex& vertex : polygon) {
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

CompiledScene compile(const ScenePacket& packet, const Projection& projection) {
  CompiledScene out;
  out.camera = projection.resolved;
  const auto planes = clipping_planes(projection);
  const PrimitiveTable* tables[] = {&packet.faces, &packet.polylines, &packet.markers, &packet.text};
  for (int kind = 1; kind <= 4; ++kind) {
    const auto& table = *tables[kind-1];
    for (std::size_t row = 0; row < table.count.size(); ++row) {
      if (row % 4096 == 0) Rcpp::checkUserInterrupt();
      Polygon polygon;
      for (Index j = 0; j < table.count[row]; ++j) {
        Index corner = table.offset[row]+j, v = packet.corners.vertex[corner];
        polygon.push_back({{packet.vertices.x[v], packet.vertices.y[v], packet.vertices.z[v]}, {{corner, 1}}});
      }
      if (kind == 1) {
        Vec3 normal = face_normal(polygon, projection);
        bool clipped = false;
        polygon = clip_polygon(std::move(polygon), planes, clipped);
        if (polygon.size() >= 3 && table.opacity[row] > 0) {
          Vec3 area = area_vector(polygon, projection);
          if (std::sqrt(dot(area, area)) > projection.resolved.tolerance)
            append_fragment(out, table, row, Kind::face, polygon, projection, clipped, normal);
        }
      } else if (kind == 2) {
        double cumulative = 0;
        for (std::size_t j = 1; j < polygon.size(); ++j) {
          const Vertex& a = polygon[j-1]; const Vertex& b = polygon[j];
          double start = 0, end = 1;
          for (const Plane& plane : planes) {
            double da = signed_distance(plane, a.xyz), db = signed_distance(plane, b.xyz);
            if (da < 0 && db < 0) { end = -1; break; }
            if (da < 0) start = std::max(start, da/(da-db));
            if (db < 0) end = std::min(end, da/(da-db));
          }
          // Dash arclength is measured after near/far clipping, before viewport clipping.
          double near_start = 0, near_end = 1;
          Vec3 ca = projection.camera_point(a.xyz), cb = projection.camera_point(b.xyz);
          for (int sign : {1, -1}) {
            double boundary = sign == 1 ? projection.resolved.near : projection.resolved.far;
            double da = sign*(ca[2]-boundary), db = sign*(cb[2]-boundary);
            if (da < 0 && db < 0) { near_end = -1; break; }
            if (da < 0) near_start = std::max(near_start, da/(da-db));
            if (db < 0) near_end = std::min(near_end, da/(da-db));
          }
          double length = 0, first = cumulative, last = cumulative;
          if (near_start <= near_end) {
            auto pa = projection.npc(projection.camera_point(interpolate(a,b,near_start).xyz));
            auto pb = projection.npc(projection.camera_point(interpolate(a,b,near_end).xyz));
            auto distance_mm = [&](const std::array<double,2>& p) {
              return std::hypot((p[0]-pa[0])*projection.viewport.width_mm,
                                (p[1]-pa[1])*projection.viewport.height_mm);
            };
            length = distance_mm(pb);
            if (start <= end) {
              first += distance_mm(projection.npc(projection.camera_point(interpolate(a,b,start).xyz)));
              last += distance_mm(projection.npc(projection.camera_point(interpolate(a,b,end).xyz)));
            }
          }
          if (start <= end && table.opacity[row] > 0) {
            Polygon segment{interpolate(a,b,start), interpolate(a,b,end)};
            append_fragment(out, table, row, Kind::polyline, segment, projection,
              start > 0 || end < 1, {0,0,0}, first, last,
              j == 1 && start == 0 && packet.polylines.arrow_start[row],
              j+1 == polygon.size() && end == 1 && packet.polylines.arrow_end[row]);
          }
          cumulative += length;
        }
      } else {
        bool visible = table.opacity[row] > 0;
        for (const Plane& plane : planes) visible = visible && signed_distance(plane, polygon[0].xyz) >= 0;
        if (visible) append_fragment(out, table, row, static_cast<Kind>(kind), polygon, projection, false);
      }
    }
  }
  auto& f = out.fragments;
  out.draw_order.resize(f.count.size());
  std::iota(out.draw_order.begin(), out.draw_order.end(), 0);
  std::stable_sort(out.draw_order.begin(), out.draw_order.end(), [&](Index a, Index b) {
    if (f.partition[a] != f.partition[b]) return f.partition[a] < f.partition[b];
    if (f.partition[a] == 1 && f.depth_centroid[a] != f.depth_centroid[b])
      return f.depth_centroid[a] > f.depth_centroid[b];
    if (f.partition[a] != 1 && f.layer_id[a] != f.layer_id[b]) return f.layer_id[a] < f.layer_id[b];
    return std::tie(f.role[a], f.layer_id[a], f.source_row[a], f.fragment_id[a]) <
           std::tie(f.role[b], f.layer_id[b], f.source_row[b], f.fragment_id[b]);
  });
  return out;
}

Rcpp::IntegerVector indices(const std::vector<Index>& x, bool increment = false) {
  Rcpp::IntegerVector out(x.begin(), x.end());
  if (increment) for (int& value : out) ++value;
  return out;
}
Rcpp::LogicalVector booleans(const std::vector<bool>& x) {
  Rcpp::LogicalVector out(x.size());
  for (std::size_t i = 0; i < x.size(); ++i) out[i] = x[i];
  return out;
}
Rcpp::NumericMatrix matrix(const std::array<double, 16>& x) {
  Rcpp::NumericMatrix out(4,4); std::copy(x.begin(), x.end(), out.begin()); return out;
}

Rcpp::List write_result(const CompiledScene& scene, const ScenePacket& packet,
                        const Viewport& viewport) {
  const auto& v = scene.vertices; const auto& f = scene.fragments; const auto& c = scene.camera;
  Rcpp::List vertices = Rcpp::List::create(
    Rcpp::Named("x")=v.x, Rcpp::Named("y")=v.y, Rcpp::Named("z")=v.z,
    Rcpp::Named("x_camera")=v.x_camera, Rcpp::Named("y_camera")=v.y_camera,
    Rcpp::Named("depth")=v.depth, Rcpp::Named("x_npc")=v.x_npc,
    Rcpp::Named("y_npc")=v.y_npc, Rcpp::Named("clip_w")=v.clip_w);
  Rcpp::List fragments;
  fragments["primitive_id"] = indices(f.primitive_id); fragments["layer_id"] = indices(f.layer_id);
  fragments["source_row"] = indices(f.source_row); fragments["style_id"] = indices(f.style_id, true);
  fragments["offset"] = indices(f.offset, true); fragments["count"] = indices(f.count);
  fragments["role"] = indices(f.role); fragments["partition"] = indices(f.partition);
  fragments["opacity"] = Rcpp::wrap(f.opacity); fragments["fragment_id"] = indices(f.fragment_id);
  fragments["kind"] = indices(f.kind); fragments["parent_primitive_id"] = indices(f.parent_primitive_id);
  fragments["source_index"] = indices(f.source_index, true);
  fragments["mask_offset"] = indices(f.mask_offset, true); fragments["mask_count"] = indices(f.mask_count);
  fragments["depth_min"] = Rcpp::wrap(f.depth_min); fragments["depth_max"] = Rcpp::wrap(f.depth_max);
  fragments["depth_centroid"] = Rcpp::wrap(f.depth_centroid);
  fragments["normal_x"] = Rcpp::wrap(f.normal_x); fragments["normal_y"] = Rcpp::wrap(f.normal_y);
  fragments["normal_z"] = Rcpp::wrap(f.normal_z); fragments["path_start_mm"] = Rcpp::wrap(f.path_start_mm);
  fragments["path_end_mm"] = Rcpp::wrap(f.path_end_mm); fragments["clipped"] = booleans(f.clipped);
  fragments["hidden"] = booleans(f.hidden); fragments["arrow_start"] = booleans(f.arrow_start);
  fragments["arrow_end"] = booleans(f.arrow_end);
  Rcpp::List camera = Rcpp::List::create(
    Rcpp::Named("view")=matrix(c.view), Rcpp::Named("projection")=matrix(c.projection),
    Rcpp::Named("eye")=c.eye, Rcpp::Named("right")=c.right, Rcpp::Named("up")=c.up,
    Rcpp::Named("forward")=c.forward, Rcpp::Named("distance")=c.distance,
    Rcpp::Named("model_scale")=c.model_scale,
    Rcpp::Named("near")=c.near, Rcpp::Named("far")=c.far,
    Rcpp::Named("view_height")=c.view_height, Rcpp::Named("tolerance")=c.tolerance);
  return Rcpp::List::create(
    Rcpp::Named("schema_version")=scene_schema_version, Rcpp::Named("panel_id")=packet.panel_id,
    Rcpp::Named("vertices")=vertices, Rcpp::Named("fragments")=fragments,
    Rcpp::Named("weights")=Rcpp::List::create(Rcpp::Named("vertex")=indices(scene.weights.vertex,true),
      Rcpp::Named("corner")=indices(scene.weights.corner,true), Rcpp::Named("weight")=scene.weights.weight),
    Rcpp::Named("masks")=Rcpp::List::create(Rcpp::Named("x_npc")=scene.masks.x_npc,
      Rcpp::Named("y_npc")=scene.masks.y_npc, Rcpp::Named("ring")=indices(scene.masks.ring)),
    Rcpp::Named("draw_order")=indices(scene.draw_order,true), Rcpp::Named("camera")=camera,
    Rcpp::Named("viewport")=Rcpp::List::create(Rcpp::Named("width_mm")=viewport.width_mm,
      Rcpp::Named("height_mm")=viewport.height_mm, Rcpp::Named("dpi")=viewport.dpi),
    Rcpp::Named("capabilities")=Rcpp::List::create(Rcpp::Named("visibility")="centroid",
      Rcpp::Named("exact_visibility")=false, Rcpp::Named("footprint_clipping")=false));
}
}  // namespace

// [[Rcpp::export(rng = false)]]
Rcpp::List scene_compile_cpp(Rcpp::List packet, Rcpp::List camera, Rcpp::List viewport) {
  const ScenePacket scene = read_packet(packet);
  const Camera view = read_camera(camera);
  fields(viewport, {"width_mm", "height_mm", "dpi"});
  const Viewport device{scalar(viewport,"width_mm"), scalar(viewport,"height_mm"), scalar(viewport,"dpi")};
  require(device.width_mm > 0 && device.height_mm > 0 && device.dpi > 0,
          "viewport dimensions and dpi must be positive");
  const Projection projection(view, device);
  return write_result(compile(scene, projection), scene, device);
}
