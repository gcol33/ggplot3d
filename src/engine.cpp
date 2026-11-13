#include "scene/common.h"
#include <set>

namespace ggplot3d { namespace engine {
using namespace ggplot3d;
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
  const std::string nonfinite = std::string("non-finite column: ") + name;
  for (auto v : x) {
    require(!Rcpp::Vector<RTYPE>::is_na(v) && std::isfinite(static_cast<double>(v)),
            nonfinite);
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
  require(TYPEOF(value) == REALSXP && Rf_xlength(value) == 1 &&
          !Rf_isObject(value) && Rf_isNull(Rf_getAttrib(value,R_DimSymbol)),
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
          !Rf_isObject(projection) && Rf_isNull(Rf_getAttrib(projection,R_DimSymbol)) &&
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
            !Rf_isObject(distance) && Rf_isNull(Rf_getAttrib(distance,R_DimSymbol)) &&
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

} }
#include "scene/projection.h"
#include "scene/clipping.h"
#include "scene/output.h"
#include "scene/partition.h"
#include "scene/coverage.h"
#include "scene/compiler.h"
namespace ggplot3d { namespace engine {
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
    Rcpp::Named("capabilities")=Rcpp::List::create(Rcpp::Named("visibility")="bsp",
      Rcpp::Named("exact_visibility")=true, Rcpp::Named("footprint_clipping")=true));
}
} }  // namespace ggplot3d::engine

// [[Rcpp::export(rng = false)]]
Rcpp::List scene_compile_cpp(Rcpp::List packet, Rcpp::List camera, Rcpp::List viewport) {
  using namespace ggplot3d;
  using namespace ggplot3d::engine;
  double limit=3221225472.0;
  SEXP budget_attribute=Rf_getAttrib(viewport,Rf_install("engine_budget_bytes"));
  if(!Rf_isNull(budget_attribute)) {
    require((TYPEOF(budget_attribute)==REALSXP || TYPEOF(budget_attribute)==INTSXP) &&
            Rf_xlength(budget_attribute)==1,"engine_budget_bytes must be a positive finite scalar");
    limit=Rcpp::as<double>(budget_attribute);
    require(std::isfinite(limit) && limit>0,"engine_budget_bytes must be a positive finite scalar");
  }
  std::size_t inputs=0;
  for(const char* name:{"faces","polylines","markers","text"}) {
    if(packet.containsElementNamed(name)) {
      Rcpp::List table=packet[name];
      if(table.containsElementNamed("count")) inputs+=Rf_xlength(table["count"]);
    }
  }
  ResourceBudget budget(inputs,limit);
  budget.add(256.0*inputs);
  for(const char* name:{"vertices","corners"}) if(packet.containsElementNamed(name)) {
    Rcpp::List table=packet[name];
    const char* column=std::string(name)=="vertices" ? "x" : "vertex";
    if(table.containsElementNamed(column)) budget.add((std::string(name)=="vertices" ? 48.0:96.0)*Rf_xlength(table[column]));
  }
  const ScenePacket scene = read_packet(packet);
  const Camera view = read_camera(camera);
  fields(viewport, {"width_mm", "height_mm", "dpi"});
  const Viewport device{scalar(viewport,"width_mm"), scalar(viewport,"height_mm"), scalar(viewport,"dpi")};
  require(device.width_mm > 0 && device.height_mm > 0 && device.dpi > 0,
          "viewport dimensions and dpi must be positive");
  const Projection projection(view, device);
  return write_result(compile(scene, projection,budget), scene, device);
}
