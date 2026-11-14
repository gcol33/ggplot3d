#include <Rcpp.h>
#include "clipper2/clipper.h"
#include <unordered_map>

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
