#include <Rcpp.h>
#include "clipper2/clipper.h"
#include <unordered_map>
#include <algorithm>
#include <cmath>
#include <cstdint>
#include <vector>

namespace {
struct GridPaint {
  std::vector<Clipper2Lib::PathsD> groups;
  bool supported = true;
};

Clipper2Lib::PathsD grid_paths(const Rcpp::NumericVector& x, const Rcpp::NumericVector& y,
                             const Rcpp::IntegerVector& id) {
  if (x.size() != y.size() || x.size() != id.size()) Rcpp::stop("Invalid path arrays.");
  Clipper2Lib::PathsD paths;
  std::unordered_map<int, size_t> index;
  for (R_xlen_t i = 0; i < x.size(); ++i) {
    if (!R_finite(x[i]) || !R_finite(y[i]) || id[i] == NA_INTEGER) Rcpp::stop("Invalid path coordinates.");
    auto found = index.emplace(id[i], paths.size());
    if (found.second) paths.emplace_back();
    auto& path = paths[found.first->second];
    Clipper2Lib::PointD point(x[i], y[i]);
    if (path.empty() || path.back() != point) path.push_back(point);
  }
  return paths;
}

bool stroke_supported(const Clipper2Lib::PathD& path, double radius) {
  for (size_t i = 1; i + 1 < path.size(); ++i) {
    double ax = path[i].x - path[i - 1].x, ay = path[i].y - path[i - 1].y;
    double bx = path[i + 1].x - path[i].x, by = path[i + 1].y - path[i].y;
    double first = std::hypot(ax, ay), second = std::hypot(bx, by);
    ax /= first; ay /= first; bx /= second; by /= second;
    double denominator = 1 + ax * bx + ay * by;
    if (denominator <= 0 || radius * std::abs(ax * by - ay * bx) / denominator > std::min(first, second))
      return false;
  }
  return true;
}

static inline Clipper2Lib::PathD arc_path(const Clipper2Lib::PointD& p, double radius,
                                         double start, double sweep, double tolerance) {
  Clipper2Lib::PathD ring;
  if (radius <= 0) return ring;
  double pi = std::acos(-1.0);
  double steps = std::ceil(std::abs(sweep) / (2 * std::acos(std::max(-1.0, 1 - tolerance / radius))));
  if (!R_finite(steps) || steps > std::numeric_limits<int>::max())
    Rcpp::stop("Circular outline exceeds path index capacity.");
  int n = std::max(static_cast<int>(std::ceil(std::abs(sweep) * 8 / pi)), static_cast<int>(steps));
  n = std::max(1, n);
  ring.reserve(n + 1);
  for (int i = 0; i <= n; ++i) {
    double angle = start + sweep * i / n;
    ring.emplace_back(p.x + radius * std::cos(angle), p.y + radius * std::sin(angle));
  }
  return ring;
}

static inline Clipper2Lib::PathD circle_path(const Clipper2Lib::PointD& p, double radius, double tolerance) {
  auto ring = arc_path(p, radius, 0, 2 * std::acos(-1.0), tolerance);
  if (!ring.empty()) ring.pop_back();
  return ring;
}

Clipper2Lib::PathsD stroke_paths(const Clipper2Lib::PathsD& input, double radius,
                                int cap, int join, double mitre, double tolerance) {
  using namespace Clipper2Lib;
  PathsD parts;
  auto append = [&](PathD ring) {
    if (!IsPositive(ring)) std::reverse(ring.begin(), ring.end());
    parts.push_back(std::move(ring));
  };
  for (const auto& path : input) {
    if (path.size() < 2) continue;
    std::vector<PointD> direction;
    for (size_t i = 1; i < path.size(); ++i) {
      const auto& a = path[i - 1]; const auto& b = path[i];
      double dx = b.x - a.x, dy = b.y - a.y, length = std::hypot(dx, dy);
      PointD u(dx / length, dy / length); direction.push_back(u);
      PointD n(-u.y * radius, u.x * radius);
      append({{a.x - n.x, a.y - n.y}, {b.x - n.x, b.y - n.y},
              {b.x + n.x, b.y + n.y}, {a.x + n.x, a.y + n.y}});
    }
    for (size_t i = 0; i < path.size(); ++i) {
      const auto& p = path[i];
      if (i == 0 || i + 1 == path.size()) {
        if (cap == 0) continue;
        const auto& u = direction[i == 0 ? 0 : direction.size() - 1];
        if (cap == 1) {
          double start = std::atan2(u.y, u.x) + (i == 0 ? 0.5 : -0.5) * std::acos(-1.0);
          append(arc_path(p, radius, start, std::acos(-1.0), tolerance));
          continue;
        }
        double sign = i == 0 ? -1 : 1;
        PointD n(-u.y * radius, u.x * radius), d(u.x * radius * sign, u.y * radius * sign);
        append({{p.x - n.x, p.y - n.y}, {p.x + d.x - n.x, p.y + d.y - n.y},
                {p.x + d.x + n.x, p.y + d.y + n.y}, {p.x + n.x, p.y + n.y}});
      } else {
        const auto& a = direction[i - 1]; const auto& b = direction[i];
        double turn = a.x * b.y - a.y * b.x;
        double dot = a.x * b.x + a.y * b.y;
        if (turn == 0) {
          if (join == 0 && dot < 0)
            append(arc_path(p, radius, std::atan2(a.y, a.x) - 0.5 * std::acos(-1.0), std::acos(-1.0), tolerance));
          continue;
        }
        double sign = turn > 0 ? 1 : -1;
        if (join == 0) {
          PathD ring{p};
          auto arc = arc_path(p, radius, std::atan2(-sign * a.x, sign * a.y), std::atan2(turn, dot), tolerance);
          ring.insert(ring.end(), arc.begin(), arc.end());
          append(std::move(ring));
          continue;
        }
        PathD ring{p, {p.x + sign * a.y * radius, p.y - sign * a.x * radius}};
        double denominator = 1 + dot;
        if (join == 1 && denominator > 0) {
          PointD tip(p.x + sign * (a.y + b.y) * radius / denominator,
                     p.y - sign * (a.x + b.x) * radius / denominator);
          if (std::hypot(tip.x - p.x, tip.y - p.y) <= radius * mitre) ring.push_back(tip);
        }
        ring.emplace_back(p.x + sign * b.y * radius, p.y - sign * b.x * radius);
        append(std::move(ring));
      }
    }
  }
  return Union(parts, FillRule::NonZero, 8);
}
}

// Painted outlines intersected with a visible even-odd footprint, in millimetres.
// [[Rcpp::export(rng = false)]]
Rcpp::List grid_clip_paths_cpp(Rcpp::NumericVector x, Rcpp::NumericVector y, Rcpp::IntegerVector id,
                              Rcpp::NumericVector mask_x, Rcpp::NumericVector mask_y, Rcpp::IntegerVector mask_id,
                              double width, int cap, int join, double mitre, double tolerance,
                              SEXP outline = R_NilValue, Rcpp::Nullable<Rcpp::NumericVector> radii = R_NilValue) {
  using namespace Clipper2Lib;
  Rcpp::XPtr<GridPaint> paint(outline == R_NilValue ? Rcpp::XPtr<GridPaint>(new GridPaint(), true) : Rcpp::XPtr<GridPaint>(outline));
  PathsD mask = Union(grid_paths(mask_x, mask_y, mask_id), FillRule::EvenOdd, 8);
  if (outline == R_NilValue) {
    PathsD input = grid_paths(x, y, id);
    if (radii.isNotNull()) {
      Rcpp::NumericVector r(radii);
      if (r.size() != 2 || !R_finite(r[0]) || !R_finite(r[1]) || r[0] < r[1] || r[1] < 0 ||
          input.size() != 1 || input.front().size() != 1 || !R_finite(tolerance) || tolerance <= 0)
        Rcpp::stop("Invalid circular outline parameters.");
      PointD centre = input.front().front();
      PathsD rings{circle_path(centre, r[0], tolerance)};
      if (r[1] > 0) rings.push_back(circle_path(centre, r[1], tolerance));
      paint->groups.push_back(Union(rings, FillRule::EvenOdd, 8));
    } else if (width > 0) {
      if (cap < 0 || cap > 2 || join < 0 || join > 2 || !R_finite(width) || !R_finite(tolerance) || tolerance <= 0)
        Rcpp::stop("Invalid stroke outline parameters.");
      for (const auto& path : input) if (!stroke_supported(path, width / 2)) paint->supported = false;
      for (const auto& path : input) if (paint->supported)
        paint->groups.push_back(stroke_paths({path}, width / 2, cap, join, mitre, tolerance));
    } else paint->groups.push_back(Union(input, FillRule::EvenOdd, 8));
  }
  std::vector<double> xx, yy;
  std::vector<int> rings, path_id;
  int ring_id = 0, paint_id = 0;
  for (const auto& group : paint->groups) {
    PathsD clipped = Intersect(group, mask, FillRule::NonZero, 8);
    if (clipped.empty()) continue;
    ++paint_id;
    for (const auto& ring : clipped) {
      ++ring_id;
      for (const auto& p : ring) {
        xx.push_back(p.x); yy.push_back(p.y); rings.push_back(ring_id); path_id.push_back(paint_id);
      }
    }
  }
  return Rcpp::List::create(Rcpp::Named("x") = xx, Rcpp::Named("y") = yy, Rcpp::Named("id") = rings,
                            Rcpp::Named("path_id") = path_id, Rcpp::Named("outline") = paint,
                            Rcpp::Named("supported") = paint->supported);
}

namespace {
inline double srgb_encode_unit(double v) {
  v = std::min(1.0, std::max(0.0, v));
  return v <= 0.0031308 ? 12.92 * v : 1.055 * std::pow(v, 1 / 2.4) - 0.055;
}
}

// Image of faces with per-vertex linear-light colours, painted in order:
// each face is fan-triangulated and every pixel centre inside a triangle takes
// the linear-light interpolation of its corner colours, sRGB-encoded and
// composited over the earlier faces with the face's alpha. Pixel centres
// outside every face but within `reach` pixels of one take that face's
// colour at the nearest point of its triangle, so an image clipped to the
// faces' outline keeps their colours under its antialiased edge. x and y
// are in pixels from the image's bottom-left corner; id numbers faces 1..n
// over consecutive vertices. Returns a nativeRaster (straight alpha).
// [[Rcpp::export(rng = false)]]
Rcpp::IntegerMatrix smooth_faces_raster_cpp(Rcpp::NumericVector x, Rcpp::NumericVector y, Rcpp::IntegerVector id,
                                            Rcpp::NumericMatrix linear, Rcpp::NumericVector alpha,
                                            int width, int height, double reach) {
  const R_xlen_t n = x.size();
  if (y.size() != n || id.size() != n || linear.nrow() != n || linear.ncol() != 3 || width <= 0 || height <= 0 ||
      !R_finite(reach) || reach < 0)
    Rcpp::stop("Invalid smooth face raster arrays.");
  const std::size_t pixels = static_cast<std::size_t>(width) * height;
  std::vector<double> colour(3 * pixels, 0.0), cover(pixels, 0.0);
  std::vector<int> state(pixels, 0), owner(pixels, 0);
  struct Triangle { R_xlen_t v[3]; int face; };
  std::vector<Triangle> triangles;
  for (R_xlen_t start = 0; start < n;) {
    R_xlen_t end = start;
    while (end < n && id[end] == id[start]) ++end;
    if (id[start] < 1 || id[start] > alpha.size()) Rcpp::stop("Invalid smooth face raster ids.");
    for (R_xlen_t k = start + 1; k + 1 < end; ++k) triangles.push_back({{start, k, k + 1}, id[start]});
    start = end;
  }
  auto paint = [&](bool exact) {
    for (const Triangle& t : triangles) {
      const double x0 = x[t.v[0]], y0 = y[t.v[0]], x1 = x[t.v[1]], y1 = y[t.v[1]], x2 = x[t.v[2]], y2 = y[t.v[2]];
      const double area = (x1 - x0) * (y2 - y0) - (x2 - x0) * (y1 - y0);
      if (!(std::fabs(area) > 1e-12)) continue;
      const double pad = exact ? 0 : reach;
      const int c0 = std::max(0, static_cast<int>(std::floor(std::min({x0, x1, x2}) - pad - 0.5)));
      const int c1 = std::min(width - 1, static_cast<int>(std::ceil(std::max({x0, x1, x2}) + pad - 0.5)));
      const int r0 = std::max(0, static_cast<int>(std::floor(std::min({y0, y1, y2}) - pad - 0.5)));
      const int r1 = std::min(height - 1, static_cast<int>(std::ceil(std::max({y0, y1, y2}) + pad - 0.5)));
      const double e[3] = {std::hypot(x2 - x1, y2 - y1), std::hypot(x0 - x2, y0 - y2), std::hypot(x1 - x0, y1 - y0)};
      const double a = alpha[t.face - 1];
      for (int r = r0; r <= r1; ++r) for (int c = c0; c <= c1; ++c) {
        const double px = c + 0.5, py = r + 0.5;
        double b[3] = {((x1 - px) * (y2 - py) - (x2 - px) * (y1 - py)) / area,
                       ((x2 - px) * (y0 - py) - (x0 - px) * (y2 - py)) / area,
                       ((x0 - px) * (y1 - py) - (x1 - px) * (y0 - py)) / area};
        const bool inside = b[0] >= -1e-12 && b[1] >= -1e-12 && b[2] >= -1e-12;
        const std::size_t pixel = static_cast<std::size_t>(height - 1 - r) * width + c;
        if (exact != inside) {
          if (exact || state[pixel] == 2) continue;
          double outside = 0;
          for (int k = 0; k < 3; ++k) outside = std::max(outside, -b[k] * std::fabs(area) / e[k]);
          if (outside > reach) continue;
        }
        double sum = 0;
        for (int k = 0; k < 3; ++k) { b[k] = std::max(0.0, b[k]); sum += b[k]; }
        double src[3];
        for (int ch = 0; ch < 3; ++ch) {
          double v = 0;
          for (int k = 0; k < 3; ++k) v += b[k] * linear(t.v[k], ch);
          src[ch] = srgb_encode_unit(v / sum);
        }
        double* dst = &colour[3 * pixel];
        if (exact && state[pixel] == 2 && owner[pixel] == t.face) continue;
        if (!exact || state[pixel] != 2) {
          for (int ch = 0; ch < 3; ++ch) dst[ch] = src[ch];
          cover[pixel] = a;
        } else {
          const double out = a + cover[pixel] * (1 - a);
          for (int ch = 0; ch < 3; ++ch) dst[ch] = (a * src[ch] + cover[pixel] * (1 - a) * dst[ch]) / out;
          cover[pixel] = out;
        }
        state[pixel] = exact ? 2 : 1; owner[pixel] = t.face;
      }
    }
  };
  paint(false);
  paint(true);
  Rcpp::IntegerMatrix raster(height, width);
  std::uint32_t* packed = reinterpret_cast<std::uint32_t*>(INTEGER(raster));
  auto byte = [](double v) { return static_cast<std::uint32_t>(std::lround(std::min(1.0, std::max(0.0, v)) * 255)); };
  for (std::size_t pixel = 0; pixel < pixels; ++pixel) {
    const double* c = &colour[3 * pixel];
    packed[pixel] = state[pixel] ? byte(c[0]) | byte(c[1]) << 8 | byte(c[2]) << 16 | byte(cover[pixel]) << 24 : 0u;
  }
  raster.attr("class") = "nativeRaster";
  return raster;
}

namespace {
struct MaskEdge { Clipper2Lib::PointD a, b; };

std::vector<MaskEdge> mask_edges(const Clipper2Lib::PathsD& rings) {
  std::vector<MaskEdge> edges;
  for (const auto& ring : rings) {
    size_t n = ring.size();
    if (n > 1 && ring.front() == ring.back()) --n;
    if (n < 3) continue;
    for (size_t i = 0; i < n; ++i) {
      const auto& a = ring[i]; const auto& b = ring[(i + 1) % n];
      if (a != b) edges.push_back({a, b});
    }
  }
  return edges;
}

bool even_odd_inside(const std::vector<MaskEdge>& edges, double x, double y) {
  bool inside = false;
  for (const auto& e : edges)
    if ((e.a.y > y) != (e.b.y > y) && x < e.a.x + (y - e.a.y) * (e.b.x - e.a.x) / (e.b.y - e.a.y)) inside = !inside;
  return inside;
}

double point_segment_distance(const Clipper2Lib::PointD& p, const Clipper2Lib::PointD& a, const Clipper2Lib::PointD& b) {
  double dx = b.x - a.x, dy = b.y - a.y, length2 = dx * dx + dy * dy;
  double t = length2 > 0 ? std::max(0.0, std::min(1.0, ((p.x - a.x) * dx + (p.y - a.y) * dy) / length2)) : 0;
  return std::hypot(p.x - a.x - t * dx, p.y - a.y - t * dy);
}

double segment_distance(const Clipper2Lib::PointD& a, const Clipper2Lib::PointD& b,
                        const Clipper2Lib::PointD& c, const Clipper2Lib::PointD& d) {
  double ux = b.x - a.x, uy = b.y - a.y, vx = d.x - c.x, vy = d.y - c.y;
  double cross = ux * vy - uy * vx;
  if (cross != 0) {
    double s = ((c.x - a.x) * vy - (c.y - a.y) * vx) / cross, t = ((c.x - a.x) * uy - (c.y - a.y) * ux) / cross;
    if (s >= 0 && s <= 1 && t >= 0 && t <= 1) return 0;
  }
  return std::min(std::min(point_segment_distance(a, c, d), point_segment_distance(b, c, d)),
                  std::min(point_segment_distance(c, a, b), point_segment_distance(d, a, b)));
}

// Whether a segment enters the interior of a triangle, shrunk by a relative
// margin towards its centroid so that segments along its sides do not count.
bool crosses_triangle(const MaskEdge& edge, Clipper2Lib::PointD p, Clipper2Lib::PointD q, Clipper2Lib::PointD r) {
  using Clipper2Lib::PointD;
  PointD centre((p.x + q.x + r.x) / 3, (p.y + q.y + r.y) / 3);
  auto shrink = [&](const PointD& v) { return PointD(centre.x + (v.x - centre.x) * (1 - 1e-6), centre.y + (v.y - centre.y) * (1 - 1e-6)); };
  p = shrink(p); q = shrink(q); r = shrink(r);
  auto side = [](const PointD& a, const PointD& b, const PointD& c) { return (b.x - a.x) * (c.y - a.y) - (b.y - a.y) * (c.x - a.x); };
  auto inside = [&](const PointD& v) {
    double d1 = side(p, q, v), d2 = side(q, r, v), d3 = side(r, p, v);
    return (d1 > 0 && d2 > 0 && d3 > 0) || (d1 < 0 && d2 < 0 && d3 < 0);
  };
  if (inside(edge.a) || inside(edge.b)) return true;
  const PointD corners[3] = {p, q, r};
  for (int i = 0; i < 3; ++i) {
    const PointD& c = corners[i]; const PointD& d = corners[(i + 1) % 3];
    if ((side(c, d, edge.a) > 0) != (side(c, d, edge.b) > 0) && (side(edge.a, edge.b, c) > 0) != (side(edge.a, edge.b, d) > 0)) return true;
  }
  return false;
}


// Distance by which a stroke drawn up to a cut point, with the given line
// end, departs from the same stroke cut along the mask edge through that
// point at an angle with this sine and cosine: the Hausdorff distance
// between the two ends.
double cut_deviation(double radius, int cap, double sine, double cosine) {
  if (sine <= 0) return R_PosInf;
  double cot = cosine / sine;
  if (cap == 1) return radius * std::max(1.0, 1 / sine - 1);
  if (cap == 2) return radius * (1 + cot);
  return radius * cot;
}
}

// Visible intervals of polylines inside an even-odd footprint, in
// millimetres, for drawing a masked stroke as ordinary stroke segments
// (object-space hidden-line removal, Appel 1967). Each interval ends at an
// end of its input path (kind 0) or where it leaves the footprint (kind 1).
// Deviations bound how far a stroke of that width and line end drawn along
// the interval departs from the stroke clipped to the footprint: at each cut
// end (`start_deviation`, `end_deviation`, zero at a path end), and along the
// interval where the footprint boundary passes closer than the stroke's
// reach to its sides, mitre joins or square caps (`lateral`), and by
// footprint that no interval paints: a ring that no interval meets, or
// footprint within the half width of a hidden part of the centreline away
// from its cuts (`uncovered`). With `pieces`, the footprint is one piece of
// a stroke whose other pieces are drawn elsewhere, and `lateral` reports
// only a piece boundary that runs along the centreline.
// [[Rcpp::export(rng = false)]]
Rcpp::List grid_stroke_intervals_cpp(Rcpp::NumericVector x, Rcpp::NumericVector y, Rcpp::IntegerVector id,
                                     Rcpp::NumericVector mask_x, Rcpp::NumericVector mask_y, Rcpp::IntegerVector mask_id,
                                     double width, int cap, int join, double mitre, bool pieces = false) {
  using namespace Clipper2Lib;
  if (!R_finite(width) || width < 0 || cap < 0 || cap > 2 || join < 0 || join > 2)
    Rcpp::stop("Invalid stroke interval parameters.");
  const double radius = width / 2;
  PathsD paths = grid_paths(x, y, id);
  PathsD rings = Union(grid_paths(mask_x, mask_y, mask_id), FillRule::EvenOdd, 8);
  std::vector<MaskEdge> edges = mask_edges(rings);
  std::vector<double> xx, yy, lateral, start_deviation, end_deviation;
  std::vector<int> interval, path_index, start_kind, end_kind;
  int count = 0;
  double beside = 0;
  for (size_t k = 0; k < paths.size(); ++k) {
    const PathD& path = paths[k];
    if (path.size() < 2) continue;
    PathD points; std::vector<size_t> segment_of;
    int opened_kind = 0; double worst = 0, opened = 0, ended = 0;
    std::vector<std::pair<size_t, size_t>> exempt;
    auto close = [&](int kind) {
      double drawn = 0;
      for (size_t s = 1; s < points.size(); ++s) drawn += std::hypot(points[s].x - points[s - 1].x, points[s].y - points[s - 1].y);
      if (points.size() < 2 || drawn <= 1e-9 * (1 + radius)) { points.clear(); segment_of.clear(); exempt.clear(); return; }
      const size_t last = points.size() - 2;
      const double scale = 1e-7;
      for (size_t e = 0; e < edges.size(); ++e) {
        const auto& edge = edges[e];
        // An edge through a path end cuts the stroke there, at its angle to
        // the end segment: the footprint's own square end is a cut at a right
        // angle, an edge along the centreline is no cut at all.
        // Between two pieces of one stroke, such an edge only moves which
        // piece paints the corner beside it, unless it runs along the
        // centreline for half the interval or more.
        auto end_cut = [&](const PointD& p, const PointD& q) {
          double ux = q.x - p.x, uy = q.y - p.y, vx = edge.b.x - edge.a.x, vy = edge.b.y - edge.a.y;
          double norm = std::hypot(ux, uy) * std::hypot(vx, vy);
          if (norm <= 0) return R_PosInf;
          double sine = std::abs(ux * vy - uy * vx) / norm, cosine = std::abs(ux * vx + uy * vy) / norm;
          if (pieces) return radius >= sine * drawn / 2 ? radius : 0.0;
          return cut_deviation(radius, cap, sine, cosine);
        };
        bool through_start = opened_kind == 0 && point_segment_distance(points.front(), edge.a, edge.b) <= scale;
        bool through_end = kind == 0 && point_segment_distance(points.back(), edge.a, edge.b) <= scale;
        if (through_start) worst = std::max(worst, end_cut(points[0], points[1]));
        if (through_end) worst = std::max(worst, end_cut(points[last + 1], points[last]));
        for (size_t s = 0; s <= last; ++s) {
          bool skip = (s == 0 && through_start) || (s == last && through_end);
          for (const auto& ex : exempt) if (ex.first == e && ex.second == segment_of[s]) skip = true;
          if (!skip) worst = std::max(worst, radius - segment_distance(points[s], points[s + 1], edge.a, edge.b));
        }
        if (join == 1) for (size_t s = 1; s <= last; ++s) {
          const auto& a = points[s - 1]; const auto& p = points[s]; const auto& b = points[s + 1];
          double ax = p.x - a.x, ay = p.y - a.y, bx = b.x - p.x, by = b.y - p.y;
          double la = std::hypot(ax, ay), lb = std::hypot(bx, by);
          double half = std::sqrt(std::max(0.0, (1 + (ax * bx + ay * by) / (la * lb)) / 2));
          if (half <= 0 || radius / half > radius * mitre) continue;
          double tip = radius / half;
          if (tip - radius <= scale) continue;
          double sign = ax * by - ay * bx > 0 ? 1 : -1;
          PointD corner(p.x + sign * (ay / la + by / lb) * radius / (2 * half * half), p.y - sign * (ax / la + bx / lb) * radius / (2 * half * half));
          PointD left(p.x + sign * ay / la * radius, p.y - sign * ax / la * radius);
          PointD right(p.x + sign * by / lb * radius, p.y - sign * bx / lb * radius);
          if (crosses_triangle(edge, left, corner, right)) worst = std::max(worst, tip - radius);
        }
      }
      if (cap == 2 && (opened_kind == 0 || kind == 0)) worst = std::max(worst, radius * (std::sqrt(2.0) - 1));
      ++count;
      for (const auto& p : points) { xx.push_back(p.x); yy.push_back(p.y); interval.push_back(count); }
      path_index.push_back(static_cast<int>(k) + 1); start_kind.push_back(opened_kind); end_kind.push_back(kind);
      lateral.push_back(worst); start_deviation.push_back(opened); end_deviation.push_back(ended);
      points.clear(); segment_of.clear(); exempt.clear();
    };
    struct Hidden { PointD a, b; double from, to; size_t segment; };
    struct Window { double at, reach; size_t segment; };
    std::vector<Hidden> hidden; std::vector<Window> windows;
    double travelled = 0;
    for (size_t j = 0; j + 1 < path.size(); ++j) {
      const PointD& a = path[j]; const PointD& b = path[j + 1];
      double ux = b.x - a.x, uy = b.y - a.y, length = std::hypot(ux, uy);
      std::vector<double> crossings;
      for (size_t e = 0; e < edges.size(); ++e) {
        const auto& edge = edges[e];
        double vx = edge.b.x - edge.a.x, vy = edge.b.y - edge.a.y;
        double cross = ux * vy - uy * vx;
        if (cross == 0) continue;
        double t = ((edge.a.x - a.x) * vy - (edge.a.y - a.y) * vx) / cross;
        double s = ((edge.a.x - a.x) * uy - (edge.a.y - a.y) * ux) / cross;
        const double snap = 1e-9;
        if (t < -snap || t > 1 + snap || s < -snap || s > 1 + snap) continue;
        t = t < snap ? 0 : t > 1 - snap ? 1 : t;
        crossings.push_back(t);
      }
      std::sort(crossings.begin(), crossings.end());
      // Crossings closer than the footprint's coordinate precision are one
      // cut: the corner of two footprint edges on the centreline.
      const double merge = 1e-7 / length;
      std::vector<double> cuts{0};
      for (double t : crossings) if (t > cuts.back() + merge) cuts.push_back(t);
      if (cuts.back() < 1 - merge) cuts.push_back(1); else cuts.back() = 1;
      auto at = [&](double t) { return PointD(a.x + t * ux, a.y + t * uy); };
      // The footprint edges through a cut point, within the footprint's
      // coordinate precision, make the cut.
      auto cut_end = [&](double t) {
        double d = 0, reach = 0; bool found = false;
        const PointD point = at(t);
        for (size_t e = 0; e < edges.size(); ++e) {
          const auto& edge = edges[e];
          if (point_segment_distance(point, edge.a, edge.b) > 1e-7) continue;
          double vx = edge.b.x - edge.a.x, vy = edge.b.y - edge.a.y, norm = length * std::hypot(vx, vy);
          double sine = std::abs(ux * vy - uy * vx) / norm, cosine = std::abs(ux * vx + uy * vy) / norm;
          found = true; d = std::max(d, cut_deviation(radius, cap, sine, cosine));
          reach = std::max(reach, sine > 0 ? radius / sine : R_PosInf);
          exempt.emplace_back(e, j);
        }
        windows.push_back({travelled + t * length, found ? reach : R_PosInf, j});
        return found ? d : R_PosInf;
      };
      for (size_t i = 0; i + 1 < cuts.size(); ++i) {
        double lo = cuts[i], hi = cuts[i + 1];
        // A centreline along a boundary between two footprint pieces of one
        // stroke belongs to the piece on its left, so exactly one draws it.
        PointD middle = at((lo + hi) / 2);
        middle.x -= 1e-6 * uy / length; middle.y += 1e-6 * ux / length;
        bool inside = even_odd_inside(edges, middle.x, middle.y);
        if (inside && points.empty()) {
          bool path_start = j == 0 && lo == 0;
          opened_kind = path_start ? 0 : 1;
          worst = 0; ended = 0;
          opened = path_start ? 0 : cut_end(lo);
          points.push_back(at(lo));
        }
        if (inside) {
          points.push_back(at(hi)); segment_of.push_back(j);
          if (j + 2 == path.size() && hi == 1) close(0);
        } else {
          if (!points.empty()) { ended = cut_end(lo); close(1); }
          hidden.push_back({at(lo), at(hi), travelled + lo * length, travelled + hi * length, j});
        }
      }
      travelled += length;
    }
    // Paint of the hidden centreline lies in the footprint only within the
    // reach of a cut along the same straight segment, r / sin(angle), which
    // the cut's own deviation covers; elsewhere the footprint boundary has to
    // keep the stroke's half width from the hidden centreline.
    for (const auto& h : hidden) {
      std::vector<std::pair<double, double>> keep{{h.from, h.to}};
      for (const auto& w : windows) {
        if (w.segment != h.segment) continue;
        std::vector<std::pair<double, double>> next;
        for (const auto& k : keep) {
          if (w.at - w.reach > k.first) next.emplace_back(k.first, std::min(k.second, w.at - w.reach));
          if (w.at + w.reach < k.second) next.emplace_back(std::max(k.first, w.at + w.reach), k.second);
        }
        keep.swap(next);
      }
      double span = h.to - h.from;
      for (const auto& k : keep) {
        if (k.second <= k.first || span <= 0) continue;
        PointD p(h.a.x + (h.b.x - h.a.x) * (k.first - h.from) / span, h.a.y + (h.b.y - h.a.y) * (k.first - h.from) / span);
        PointD q(h.a.x + (h.b.x - h.a.x) * (k.second - h.from) / span, h.a.y + (h.b.y - h.a.y) * (k.second - h.from) / span);
        for (const auto& e : edges) beside = std::max(beside, radius - segment_distance(p, q, e.a, e.b));
      }
    }
  }
  // A footprint ring met by no interval paints a sliver of the stroke beside
  // a hidden centreline, up to the stroke's half width.
  double uncovered = 0;
  for (const auto& ring : rings) {
    std::vector<MaskEdge> own = mask_edges({ring});
    if (own.empty()) continue;
    bool met = false;
    for (size_t i = 0; i < xx.size() && !met; ++i) {
      if (even_odd_inside(own, xx[i], yy[i])) met = true;
      if (i + 1 < xx.size() && interval[i] == interval[i + 1]) {
        PointD a(xx[i], yy[i]), b(xx[i + 1], yy[i + 1]);
        for (const auto& e : own) if (segment_distance(a, b, e.a, e.b) == 0) { met = true; break; }
      }
    }
    if (!met) uncovered = radius;
  }
  uncovered = std::max(uncovered, beside);
  return Rcpp::List::create(Rcpp::Named("x") = xx, Rcpp::Named("y") = yy, Rcpp::Named("id") = interval,
                            Rcpp::Named("uncovered") = uncovered, Rcpp::Named("path") = path_index, Rcpp::Named("start_kind") = start_kind,
                            Rcpp::Named("end_kind") = end_kind, Rcpp::Named("start_deviation") = start_deviation,
                            Rcpp::Named("end_deviation") = end_deviation, Rcpp::Named("lateral") = lateral);
}
