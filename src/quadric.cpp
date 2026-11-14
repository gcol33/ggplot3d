#include "scene/wire.h"
#include "scene/quadric.h"
#include <map>

namespace {
using namespace ggplot3d;
using namespace ggplot3d::engine;

struct QuadricView {
  ScenePacket packet;
  Projection projection;
  QuadricView(Rcpp::List packet_list, Rcpp::List camera, Rcpp::List viewport)
    : packet(read_packet(packet_list)), projection(read_camera(camera),
        Viewport{Rcpp::as<double>(viewport["width_mm"]), Rcpp::as<double>(viewport["height_mm"]),
                 Rcpp::as<double>(viewport["dpi"])}) {}
  QuadricShape shape(int row) const {
    require(row >= 1 && row <= static_cast<int>(packet.quadrics.kind.size()), "invalid quadric row");
    return quadric_shape(packet.quadrics, row-1, projection.camera.pre);
  }
};
}

// Exact outlines of quadric rows (1-based) in npc: rings of the polygon that
// follows the outline within `tolerance_mm`, empty where it is unbounded, and
// the npc images of the axis ends (local z = 0 and 1; the centre of a sphere).
// [[Rcpp::export(rng = false)]]
Rcpp::List quadric_outline_cpp(Rcpp::List packet, Rcpp::List camera, Rcpp::List viewport,
                               Rcpp::IntegerVector rows, double tolerance_mm) {
  QuadricView view(packet, camera, viewport);
  const auto& v = view.projection.viewport;
  std::vector<double> x, y; std::vector<int> ring;
  Rcpp::LogicalVector bounded(rows.size());
  Rcpp::NumericMatrix axis(rows.size(), 4);
  std::fill(axis.begin(), axis.end(), NA_REAL);
  for (R_xlen_t i = 0; i < rows.size(); ++i) {
    const QuadricShape shape = view.shape(rows[i]);
    const auto outline = quadric_outline(shape, view.projection, tolerance_mm);
    bounded[i] = outline.size() >= 3;
    if (!bounded[i]) continue;
    for (int end = 0; end < 2; ++end) {
      std::array<double, 2> xy;
      Point4 local{0, 0, shape.kind == QuadricKind::sphere ? 0.0 : static_cast<double>(end), 1};
      if (quadric_screen_mm(view.projection, transform_point(shape.to_box, local), xy)) {
        axis(i, 2*end) = xy[0]/v.width_mm; axis(i, 2*end+1) = xy[1]/v.height_mm;
      }
    }
    for (const auto& p : outline) {x.push_back(p[0]/v.width_mm); y.push_back(p[1]/v.height_mm); ring.push_back(i+1);}
  }
  return Rcpp::List::create(Rcpp::Named("x_npc")=x, Rcpp::Named("y_npc")=y, Rcpp::Named("ring")=ring,
                            Rcpp::Named("bounded")=bounded, Rcpp::Named("axis")=axis);
}

// Sight rays through npc points against one quadric row: `cast` 0 is the
// visible near (or, with `back`, far) hit of the drawn solid, 1 the infinite
// curved surface, 2 and 3 the end planes. Returns camera depth, part, and the
// model-space position and outward normal of each hit.
// [[Rcpp::export(rng = false)]]
Rcpp::List quadric_cast_cpp(Rcpp::List packet, Rcpp::List camera, Rcpp::List viewport, int row,
                            Rcpp::NumericVector x_npc, Rcpp::NumericVector y_npc, int cast, bool back) {
  QuadricView view(packet, camera, viewport);
  const QuadricShape shape = view.shape(row);
  const RayFrame frame(view.projection);
  const R_xlen_t n = x_npc.size();
  require(y_npc.size() == n && cast >= 0 && cast <= 3, "invalid quadric cast");
  Rcpp::LogicalVector hit(n); Rcpp::NumericVector depth(n, NA_REAL);
  Rcpp::IntegerVector part(n, NA_INTEGER); Rcpp::NumericMatrix normal(n, 3), position(n, 3);
  std::fill(normal.begin(), normal.end(), NA_REAL);
  std::fill(position.begin(), position.end(), NA_REAL);
  for (R_xlen_t i = 0; i < n; ++i) {
    const Ray ray = frame.ray(x_npc[i], y_npc[i]);
    QuadricHit h;
    if (cast == 0) h = quadric_visible_hit(shape, view.projection, ray, back);
    else {
      auto span = quadric_cast(shape, ray, cast == 1 ? QuadricCast::surface :
                               cast == 2 ? QuadricCast::cap_0_plane : QuadricCast::cap_1_plane);
      h = back ? span.exit : span.enter;
    }
    if (!h.valid) continue;
    hit[i] = true; depth[i] = h.t; part[i] = h.part;
    const Vec3 box = ray_point(ray, h.t);
    const Vec3 n3 = quadric_normal(shape, view.projection, box, h.part), m = view.projection.model(box);
    for (int k = 0; k < 3; ++k) {normal(i, k) = n3[k]; position(i, k) = m[k];}
  }
  return Rcpp::List::create(Rcpp::Named("hit")=hit, Rcpp::Named("depth")=depth,
                            Rcpp::Named("part")=part, Rcpp::Named("normal")=normal,
                            Rcpp::Named("position")=position);
}

// Part (0 curved surface, 1 and 2 the end disks) and side (far side TRUE) of
// compiled quadric fragments, from their outward model normals and a box
// point on each.
// [[Rcpp::export(rng = false)]]
Rcpp::List quadric_fragment_parts_cpp(Rcpp::List packet, Rcpp::List camera, Rcpp::List viewport,
                                      Rcpp::IntegerVector rows, Rcpp::NumericMatrix normal,
                                      Rcpp::NumericMatrix point) {
  QuadricView view(packet, camera, viewport);
  const auto& p = view.projection;
  const R_xlen_t n = rows.size();
  require(normal.nrow() == n && point.nrow() == n && normal.ncol() == 3 && point.ncol() == 3,
          "invalid quadric fragment table");
  Rcpp::IntegerVector part(n); Rcpp::LogicalVector back(n);
  for (R_xlen_t i = 0; i < n; ++i) {
    const Vec3 nm{normal(i, 0), normal(i, 1), normal(i, 2)}, box{point(i, 0), point(i, 1), point(i, 2)};
    part[i] = quadric_facet_part(view.shape(rows[i]), p, nm, box);
    const Vec3 sight = p.camera.perspective ? subtract(p.model(box), p.resolved.eye) : p.resolved.ray;
    back[i] = dot(nm, sight) >= 0;
  }
  return Rcpp::List::create(Rcpp::Named("part")=part, Rcpp::Named("back")=back);
}

// Inscribed meshes of every quadric with `segments` points around, placed by
// the quadric transforms alone (camera pre is left to the caller): vertices
// in box fractions, shared by the facets of one quadric so that each mesh is
// closed, and per facet corner the vertex and its outward unit normal
// covector; each facet is a run of `count` corners.
// [[Rcpp::export(rng = false)]]
Rcpp::List quadric_mesh_cpp(Rcpp::List packet, int segments) {
  const ScenePacket scene = read_packet(packet);
  const Mat4 identity{1,0,0,0, 0,1,0,0, 0,0,1,0, 0,0,0,1};
  std::vector<double> x, y, z, nx, ny, nz;
  std::vector<int> vertex, count, quadric;
  for (std::size_t row = 0; row < scene.quadrics.kind.size(); ++row) {
    const QuadricShape shape = quadric_shape(scene.quadrics, row, identity);
    std::map<std::array<double, 3>, int> index;
    for (const auto& facet : quadric_facets(shape, segments, false)) {
      count.push_back(facet.points.size()); quadric.push_back(row+1);
      for (const auto& local : facet.points) {
        const Point4 box = transform_point(shape.to_box, local);
        const Vec3 b{box[0], box[1], box[2]};
        auto found = index.emplace(std::array<double, 3>{local[0], local[1], local[2]}, static_cast<int>(x.size())+1);
        if (found.second) {x.push_back(b[0]); y.push_back(b[1]); z.push_back(b[2]);}
        vertex.push_back(found.first->second);
        const Vec3 n = unit(quadric_covector(shape, b, facet.part));
        nx.push_back(n[0]); ny.push_back(n[1]); nz.push_back(n[2]);
      }
    }
  }
  return Rcpp::List::create(Rcpp::Named("x")=x, Rcpp::Named("y")=y, Rcpp::Named("z")=z, Rcpp::Named("vertex")=vertex,
                            Rcpp::Named("normal_x")=nx, Rcpp::Named("normal_y")=ny, Rcpp::Named("normal_z")=nz,
                            Rcpp::Named("count")=count, Rcpp::Named("quadric")=quadric);
}
