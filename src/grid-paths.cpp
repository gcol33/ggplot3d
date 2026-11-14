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
