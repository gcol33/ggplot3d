#pragma once
#include "thurston.h"

namespace ggplot3d {
namespace raymarch {
inline double nil_distance(const Vec3 &p) {
  double rho = std::hypot(p[0], p[1]), z = std::abs(p[2]);
  if (z == 0)
    return rho;
  if (rho < 1e-10)
    return z <= 2 * engine::pi ? z : 2 * engine::pi * std::sqrt(z / engine::pi - 1);
  double lo = 0, hi = 2 * engine::pi;
  for (int i = 0; i < 55; ++i) {
    double phi = (lo + hi) / 2, s = std::sin(phi / 2),
           ratio = phi < 1e-3 ? phi / 12 + std::pow(phi, 3) / 360 + std::pow(phi, 5) / 10080
                              : (phi - std::sin(phi)) / (8 * s * s);
    if (phi + rho * rho * ratio < z)
      lo = phi;
    else
      hi = phi;
  }
  double phi = (lo + hi) / 2;
  return std::hypot(phi, rho * phi / (2 * std::sin(phi / 2)));
}
inline double sl_distance(const Vec3 &p) {
  double rho = std::asinh(std::hypot(p[0], p[1])), w = std::abs(p[2]);
  if (w == 0)
    return rho;
  if (rho < 1e-10)
    return w <= 2 * engine::pi
               ? w
               : std::sqrt((w + 2 * engine::pi) * (w + 2 * engine::pi) / 2 - 4 * engine::pi * engine::pi);
  double sh = std::sinh(rho / 2), ch = std::cosh(rho / 2), lo = -engine::pi + std::atan(sh), hi = 0, t = 0;
  for (int i = 0; i < 55; ++i) {
    double phi = (lo + hi) / 2, q = -ch * std::sin(phi) / sh, a = 1 / std::sqrt(1 + q * q), c = q * a,
           delta = c * c - a * a;
    if (std::abs(delta) < 1e-12)
      t = 2 * sh / a;
    else {
      double k = std::sqrt(std::abs(delta));
      t = 2 * (delta > 0 ? std::atan2(k * sh / a, ch * std::cos(phi)) : std::asinh(k * sh / a)) / k;
    }
    if (2 * c * t + 2 * phi > w)
      lo = phi;
    else
      hi = phi;
  }
  return t;
}
inline V4 centered_embedding(Geometry g, const Vec3 &center, const Vec3 &p) {
  int d = product(g) ? 2 : 3, k = curvature(g);
  V4 q = embedding(g, p);
  double r = 0;
  for (int j = 0; j < d; ++j)
    r += center[j] * center[j];
  r = std::sqrt(r);
  if (r > 0) {
    double angle = k == 1 ? 2 * std::atan(r) : 2 * std::atanh(r),
           C = k == 1 ? std::cos(angle) : std::cosh(angle), S = k == 1 ? std::sin(angle) : std::sinh(angle),
           dot = 0;
    for (int j = 0; j < d; ++j)
      dot += center[j] * q[j] / r;
    double along = C * dot - S * q[d], w = k * S * dot + C * q[d];
    for (int j = 0; j < d; ++j)
      q[j] += (along - dot) * center[j] / r;
    q[d] = w;
  }
  if (d == 2)
    q[3] -= center[2];
  return q;
}
struct GeometricField {
  std::shared_ptr<Tape> tape;
  Vec3 lo{}, hi{}, center{}, direction{0, 0, 1}, colour{};
  double radius = 0;
  int shape = -1;
  explicit GeometricField(const Rcpp::List &input) {
    Rcpp::NumericVector rgb = input["rgb"];
    for (int j = 0; j < 3; ++j)
      colour[j] = rgb[j];
    if (Rcpp::as<std::string>(input["kind"]) == "sdf") {
      tape = std::make_shared<Tape>(Rcpp::as<Rcpp::List>(input["tape"]));
      Rcpp::List ranges = input["ranges"];
      for (int j = 0; j < 3; ++j) {
        Rcpp::NumericVector r = ranges[j];
        lo[j] = r[0];
        hi[j] = r[1];
      }
    } else {
      shape = Rcpp::as<int>(input["shape"]);
      radius = Rcpp::as<double>(input["radius"]);
      Rcpp::NumericVector p = input["center"], n = input["direction"];
      for (int j = 0; j < 3; ++j) {
        center[j] = p[j];
        direction[j] = n[j];
      }
    }
  }
  double value(Geometry g, const Vec3 &p, std::vector<Jet> &scratch) const {
    if (tape) {
      Jet j = tape->evaluate(p, scratch);
      double box = -std::numeric_limits<double>::infinity();
      for (int k = 0; k < 3; ++k)
        box = std::max(box, std::max(lo[k] - p[k], p[k] - hi[k]));
      return std::max(j.v, box * tape->lipschitz);
    }
    if (shape == 2) {
      if (curvature(g) && (!product(g) || direction[2] == 0)) {
        V4 q = centered_embedding(g, center, p);
        double dot = direction[0] * q[0] + direction[1] * q[1] + (product(g) ? 0 : direction[2] * q[2]);
        return curvature(g) == 1 ? std::asin(std::max(-1.0, std::min(1.0, dot))) : std::asinh(dot);
      }
      return engine::dot(point(p, center, -1), direction);
    }
    if (curvature(g)) {
      V4 q = centered_embedding(g, center, p);
      int k = curvature(g), d = product(g) ? 2 : 3;
      double base = k == 1 ? std::acos(std::max(-1.0, std::min(1.0, q[d]))) : std::acosh(std::max(1.0, q[d]));
      if (shape == 0)
        return (d == 2 ? std::hypot(base, q[3]) : base) - radius;
      if (d == 2)
        return base - radius;
      double r = std::hypot(q[0], q[1]);
      return (k == 1 ? std::asin(std::min(1.0, r)) : std::asinh(r)) - radius;
    }
    Vec3 q = relative(g, center, p);
    if (shape == 1) {
      if (g == Geometry::sl2r)
        return std::asinh(std::hypot(q[0], q[1])) - radius;
      if (g == Geometry::sol)
        return std::acosh(std::max(1.0, std::cosh(q[2]) + .5 * std::exp(q[2]) * q[1] * q[1])) - radius;
      return std::hypot(q[0], q[1]) - radius;
    }
    if (g == Geometry::nil)
      return nil_distance(q) - radius;
    if (g == Geometry::sl2r)
      return sl_distance(q) - radius;
    if (g == Geometry::sol)
      return std::sqrt(std::exp(-2 * q[2]) * q[0] * q[0] + std::exp(2 * q[2]) * q[1] * q[1] + q[2] * q[2]) -
             radius;
    return norm(q) - radius;
  }
  double bound(Geometry g, const Vec3 &p, double h) const {
    if (tape)
      return tape->lipschitz * speed_bound(g, p, h);
    if (shape == 2 && !(curvature(g) && (!product(g) || direction[2] == 0)))
      return speed_bound(g, p, h);
    if (shape == 0 && g == Geometry::sol) {
      Vec3 q = relative(g, center, p);
      double r = std::hypot(q[0] * std::exp(-q[2]), q[1] * std::exp(q[2]));
      return std::sqrt(2 + std::pow((r + 1) * std::exp(h), 2));
    }
    return 1;
  }
  Vec3 normal(Geometry g, const Vec3 &p, std::vector<Jet> &scratch) const {
    if (tape) {
      Jet j = tape->evaluate(p, scratch);
      double box = -std::numeric_limits<double>::infinity();
      Vec3 n{};
      for (int k = 0; k < 3; ++k)
        for (int side = 0; side < 2; ++side) {
          double f = side ? p[k] - hi[k] : lo[k] - p[k];
          if (f > box) {
            box = f;
            n = {0, 0, 0};
            n[k] = side ? 1 : -1;
          }
        }
      if (box * tape->lipschitz > j.v)
        j.d = n;
      return unit(mv(transpose(frame(g, p)), j.d));
    }
    Vec3 grad{};
    double h = 1e-5;
    for (int k = 0; k < 3; ++k) {
      Vec3 axis{};
      axis[k] = 1;
      Geodesic line(g, p, axis);
      grad[k] = (value(g, line.at(h).p, scratch) - value(g, line.at(-h).p, scratch)) / (2 * h);
    }
    return unit(grad);
  }
  // Spherical chart bounds define a compact cap; its complement contains no field surface.
  double outside_cap(Geometry g, const Vec3 &p) const {
    if (!tape || curvature(g) != 1)
      return 0;
    int d = product(g) ? 2 : 3;
    double r = 0, R = 0;
    for (int j = 0; j < d; ++j) {
      r += p[j] * p[j];
      R += std::max(lo[j] * lo[j], hi[j] * hi[j]);
    }
    return std::max(0.0, 2 * std::atan(std::sqrt(r)) - 2 * std::atan(std::sqrt(R)));
  }
};
struct Portal {
  V4 normal;
  std::array<V4, 4> matrix;
  double offset, lift;
  bool fiber;
  explicit Portal(const Rcpp::List &p) {
    Rcpp::NumericVector n = p["normal"];
    Rcpp::NumericMatrix m = p["matrix"];
    for (int i = 0; i < 4; ++i) {
      normal[i] = n[i];
      for (int j = 0; j < 4; ++j)
        matrix[i][j] = m(i, j);
    }
    offset = Rcpp::as<double>(p["offset"]);
    lift = Rcpp::as<double>(p["lift"]);
    fiber = Rcpp::as<bool>(p["fiber"]);
  }
  double value(Geometry g, const Vec3 &p) const {
    if (fiber) {
      require(product(g) || g == Geometry::sl2r, "Fibre faces require a product geometry or SL2R");
      return normal[3] * p[2] - offset;
    }
    V4 q = embedding(g, p);
    double v = -offset;
    for (int j = 0; j < 4; ++j)
      v += normal[j] * q[j];
    return v;
  }
  Vec3 map(Geometry g, const Vec3 &p) const {
    V4 q = embedding(g, p), r{};
    for (int i = 0; i < 4; ++i)
      for (int j = 0; j < 4; ++j)
        r[i] += matrix[i][j] * q[j];
    double w = lift;
    if (g == Geometry::sl2r)
      w += p[2] + 2 * std::remainder(std::atan2(r[1], r[0]) - std::atan2(q[1], q[0]), 2 * engine::pi);
    if (curvature(g)) {
      int d = product(g) ? 2 : 3, k = curvature(g);
      double length = k * r[d] * r[d];
      for (int j = 0; j < d; ++j)
        length += r[j] * r[j];
      require(std::abs(length - k) < 1e-6 * (1 + std::abs(length)),
              "Portal does not preserve the geometry quadric");
    }
    if (g == Geometry::sl2r)
      require(std::abs(-r[0] * r[0] - r[1] * r[1] + r[2] * r[2] + r[3] * r[3] + 1) < 1e-6,
              "Portal does not preserve SL2R");
    return from_embedding(g, r, w);
  }
  M3 differential(Geometry g, const Vec3 &p) const {
    M3 out{}, inv = inverse(frame(g, map(g, p)));
    double h = 2e-6;
    for (int j = 0; j < 3; ++j) {
      Vec3 axis{};
      axis[j] = 1;
      Geodesic line(g, p, axis);
      Vec3 a = map(g, line.at(h).p), b = map(g, line.at(-h).p), v = mv(inv, point(a, b, -1));
      for (int i = 0; i < 3; ++i)
        out[i][j] = v[i] / (2 * h);
    }
    M3 gram = mm(transpose(out), out);
    for (int i = 0; i < 3; ++i)
      for (int j = 0; j < 3; ++j)
        require(std::abs(gram[i][j] - (i == j)) < 2e-4, "Portal matrix must be a geometry isometry");
    return out;
  }
  double bound(Geometry g, const Vec3 &p, double h) const {
    if (fiber)
      return std::abs(normal[3]) * (g == Geometry::sl2r ? 2 : 1);
    double n = 0;
    for (double v : normal)
      n += v * v;
    n = std::sqrt(n);
    if (g == Geometry::s3 || g == Geometry::s2xe)
      return n;
    if (g == Geometry::h3 || g == Geometry::h2xe) {
      V4 q = embedding(g, p);
      int d = product(g) ? 2 : 3;
      return n * std::sqrt(2) * q[d] * std::exp(h);
    }
    if (g == Geometry::sl2r) {
      V4 q = embedding(g, p);
      double len = 0;
      for (double v : q)
        len += v * v;
      return n * 4 * std::sqrt(len) * std::exp(2 * h);
    }
    return n * speed_bound(g, p, h);
  }
};
struct CurvedHit {
  Hit hit;
  Vec3 colour{}, position{}, incoming{};
  double area = 0;
  int portals = 0, object = -1;
};
inline CurvedHit curved_trace(Geometry g, Vec3 origin, Vec3 direction,
                              const std::vector<GeometricField> &fields, const std::vector<Portal> &portals,
                              double far, double tolerance, int max_steps) {
  CurvedHit out;
  Geodesic observer(g, origin, direction), segment = observer;
  double distance = 0, local = 0;
  std::vector<Jet> scratch;
  for (int step = 0; step < max_steps && distance <= far; ++step) {
    Flow flow = segment.at(local);
    out.hit.steps = step + 1;
    bool teleported = false;
    for (const auto &portal : portals)
      if (portal.value(g, flow.p) >= -tolerance) {
        M3 transform = portal.differential(g, flow.p);
        Vec3 p = portal.map(g, flow.p), u = unit(mv(transform, flow.u));
        segment = Geodesic(g, p, u);
        local = 4 * tolerance;
        distance += local;
        ++out.portals;
        require(out.portals <= max_steps, "Quotient teleportation did not enter the fundamental domain");
        teleported = true;
        break;
      }
    if (teleported)
      continue;
    double advance = std::min(.25, far - distance);
    for (std::size_t i = 0; i < fields.size(); ++i) {
      const auto &field = fields[i];
      double cap = field.outside_cap(g, flow.p);
      if (cap > tolerance) {
        advance = std::min(advance, cap);
        continue;
      }
      double value = field.value(g, flow.p, scratch);
      require(std::isfinite(value), "Non-finite intrinsic field value");
      if (std::abs(value) <= tolerance) {
        out.hit.found = true;
        out.hit.t = distance;
        out.hit.normal = field.normal(g, flow.p, scratch);
        out.position = flow.p;
        out.incoming = flow.u;
        out.colour = field.colour;
        out.object = int(i);
        out.area = area_density(observer, distance);
        return out;
      }
      double horizon = advance;
      if (curvature(g) == 1 && field.tape) {
        double r = product(g) ? std::hypot(flow.p[0], flow.p[1]) : norm(flow.p);
        horizon = std::min(horizon, .5 * (engine::pi - 2 * std::atan(r)));
      }
      advance = std::min(advance, std::abs(value) / field.bound(g, flow.p, horizon));
      advance = std::min(advance, horizon);
    }
    for (const auto &portal : portals)
      advance = std::min(advance, std::max(0.0, -portal.value(g, flow.p)) / portal.bound(g, flow.p, advance));
    if (!(advance > 0) || distance + advance == distance)
      break;
    local += advance;
    distance += advance;
  }
  return out;
}
} // namespace raymarch
} // namespace ggplot3d
