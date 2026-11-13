#pragma once
#include "elliptic.h"
#include <memory>

namespace ggplot3d {
namespace raymarch {
using V4 = std::array<double, 4>;
using M3 = std::array<Vec3, 3>;
inline Jet operator+(const Jet &a, const Jet &b) { return add(a, b, 0); }
inline Jet operator-(const Jet &a, const Jet &b) { return sub(a, b, 0); }
inline Jet operator-(const Jet &a) { return neg(a, a, 0); }
inline Jet operator*(const Jet &a, const Jet &b) { return mul(a, b, 0); }
inline Jet operator*(double a, const Jet &b) { return mul(Jet{a, {0, 0, 0}}, b, 0); }
inline M3 identity3() { return {{{1, 0, 0}, {0, 1, 0}, {0, 0, 1}}}; }
inline Vec3 mv(const M3 &m, const Vec3 &v) {
  Vec3 out{};
  for (int i = 0; i < 3; ++i)
    for (int j = 0; j < 3; ++j)
      out[i] += m[i][j] * v[j];
  return out;
}
inline M3 mm(const M3 &a, const M3 &b) {
  M3 out{};
  for (int i = 0; i < 3; ++i)
    for (int j = 0; j < 3; ++j)
      for (int k = 0; k < 3; ++k)
        out[i][j] += a[i][k] * b[k][j];
  return out;
}
inline M3 transpose(const M3 &a) {
  M3 out;
  for (int i = 0; i < 3; ++i)
    for (int j = 0; j < 3; ++j)
      out[i][j] = a[j][i];
  return out;
}
inline M3 inverse(const M3 &a) {
  Vec3 x = engine::cross(a[1], a[2]), y = engine::cross(a[2], a[0]), z = engine::cross(a[0], a[1]);
  double det = engine::dot(a[0], x);
  require(std::isfinite(det) && std::abs(det) > 1e-30, "Singular geometry chart");
  M3 out = transpose({x, y, z});
  for (auto &row : out)
    for (double &v : row)
      v /= det;
  return out;
}
inline M3 rotation(Vec3 axis, double angle) {
  axis = unit(axis);
  double c = std::cos(angle), s = std::sin(angle);
  M3 out{};
  for (int i = 0; i < 3; ++i)
    for (int j = 0; j < 3; ++j)
      out[i][j] = (i == j ? c : 0) + (1 - c) * axis[i] * axis[j];
  out[0][1] -= s * axis[2];
  out[0][2] += s * axis[1];
  out[1][0] += s * axis[2];
  out[1][2] -= s * axis[0];
  out[2][0] -= s * axis[1];
  out[2][1] += s * axis[0];
  return out;
}
enum class Geometry { e3, s3, h3, s2xe, h2xe, nil, sol, sl2r };
inline Geometry geometry(const std::string &name) {
  static const std::vector<std::pair<std::string, Geometry>> names = {
      {"e3", Geometry::e3},     {"s3", Geometry::s3},   {"h3", Geometry::h3},   {"s2xe", Geometry::s2xe},
      {"h2xe", Geometry::h2xe}, {"nil", Geometry::nil}, {"sol", Geometry::sol}, {"sl2r", Geometry::sl2r}};
  for (const auto &n : names)
    if (n.first == name)
      return n.second;
  throw std::runtime_error("Unknown Thurston geometry");
}
inline bool product(Geometry g) { return g == Geometry::s2xe || g == Geometry::h2xe; }
inline int curvature(Geometry g) {
  return g == Geometry::s3 || g == Geometry::s2xe ? 1 : g == Geometry::h3 || g == Geometry::h2xe ? -1 : 0;
}
template <class T> inline std::array<T, 4> sl_multiply(const std::array<T, 4> &p, const std::array<T, 4> &q) {
  return {p[0] * q[0] - p[1] * q[1] + p[2] * q[2] + p[3] * q[3],
          p[1] * q[0] + p[0] * q[1] + p[3] * q[2] - p[2] * q[3],
          p[2] * q[0] + p[3] * q[1] + p[0] * q[2] - p[1] * q[3],
          p[3] * q[0] - p[2] * q[1] + p[1] * q[2] + p[0] * q[3]};
}
inline V4 sl_point(const Vec3 &p) {
  double z = std::sqrt(1 + p[0] * p[0] + p[1] * p[1]), a = std::sqrt((z + 1) / 2), b = 2 * a,
         c = std::cos(p[2] / 2), s = std::sin(p[2] / 2);
  return {a * c, a * s, (c * p[0] + s * p[1]) / b, (-s * p[0] + c * p[1]) / b};
}
inline Vec3 sl_chart(const V4 &p, double w) {
  return {2 * (p[0] * p[2] - p[1] * p[3]), 2 * (p[0] * p[3] + p[1] * p[2]), w};
}
inline double sl_lift(const V4 &a, const V4 &b, const V4 &ab, double wa, double wb) {
  return wa + wb +
         2 * std::remainder(std::atan2(ab[1], ab[0]) - std::atan2(a[1], a[0]) - std::atan2(b[1], b[0]),
                            2 * engine::pi);
}
inline Vec3 compose(Geometry g, const Vec3 &p, const Vec3 &q) {
  if (g == Geometry::nil)
    return {p[0] + q[0], p[1] + q[1], p[2] + q[2] + (p[0] * q[1] - p[1] * q[0]) / 2};
  if (g == Geometry::sol)
    return {p[0] + std::exp(p[2]) * q[0], p[1] + std::exp(-p[2]) * q[1], p[2] + q[2]};
  if (g == Geometry::sl2r) {
    V4 a = sl_point(p), b = sl_point(q), ab = sl_multiply(a, b);
    return sl_chart(ab, sl_lift(a, b, ab, p[2], q[2]));
  }
  return point(p, q, 1);
}
inline Vec3 relative(Geometry g, const Vec3 &center, const Vec3 &p) {
  Vec3 inv{-center[0], -center[1], -center[2]};
  if (g == Geometry::sol) {
    inv[0] *= std::exp(-center[2]);
    inv[1] *= std::exp(center[2]);
  }
  if (g == Geometry::sl2r) {
    V4 a = sl_point(center);
    a[1] *= -1;
    a[2] *= -1;
    a[3] *= -1;
    inv = sl_chart(a, -center[2]);
  }
  return compose(g, inv, p);
}
inline M3 frame(Geometry g, const Vec3 &p) {
  M3 out = identity3();
  int k = curvature(g);
  if (k) {
    double r = p[0] * p[0] + p[1] * p[1] + (product(g) ? 0 : p[2] * p[2]), scale = (1 + k * r) / 2;
    require(scale > 0, "Hyperbolic points must lie inside the unit ball");
    for (int i = 0; i < (product(g) ? 2 : 3); ++i)
      out[i][i] = scale;
  }
  if (g == Geometry::nil) {
    out[2][0] = -p[1] / 2;
    out[2][1] = p[0] / 2;
  }
  if (g == Geometry::sol) {
    out[0][0] = std::exp(p[2]);
    out[1][1] = std::exp(-p[2]);
  }
  if (g == Geometry::sl2r) {
    V4 q = sl_point(p);
    std::array<V4, 3> basis = {V4{0, 0, .5, 0}, V4{0, 0, 0, .5}, V4{0, .5, 0, 0}};
    for (int j = 0; j < 3; ++j) {
      V4 d = sl_multiply(q, basis[j]);
      out[0][j] = 2 * (d[0] * q[2] + q[0] * d[2] - d[1] * q[3] - q[1] * d[3]);
      out[1][j] = 2 * (d[0] * q[3] + q[0] * d[3] + d[1] * q[2] + q[1] * d[2]);
      out[2][j] = 2 * (q[0] * d[1] - q[1] * d[0]) / (q[0] * q[0] + q[1] * q[1]);
    }
  }
  return out;
}
inline V4 embedding(Geometry g, const Vec3 &p) {
  int k = curvature(g);
  if (!k)
    return g == Geometry::sl2r ? sl_point(p) : V4{p[0], p[1], p[2], 1};
  int d = product(g) ? 2 : 3;
  double r = 0;
  for (int i = 0; i < d; ++i)
    r += p[i] * p[i];
  double den = 1 + k * r;
  V4 out{};
  for (int i = 0; i < d; ++i)
    out[i] = 2 * p[i] / den;
  out[d] = (1 - k * r) / den;
  if (d == 2)
    out[3] = p[2];
  return out;
}
inline V4 embed_vector(Geometry g, const Vec3 &p, const Vec3 &u) {
  int k = curvature(g), d = product(g) ? 2 : 3;
  Vec3 v = mv(frame(g, p), u);
  double r = 0, pv = 0;
  for (int i = 0; i < d; ++i) {
    r += p[i] * p[i];
    pv += p[i] * v[i];
  }
  double den = 1 + k * r;
  V4 out{};
  for (int i = 0; i < d; ++i)
    out[i] = 2 * v[i] / den - 4 * k * p[i] * pv / (den * den);
  out[d] = -4 * k * pv / (den * den);
  if (d == 2)
    out[3] = u[2];
  return out;
}
inline double chart_denominator(Geometry g, const V4 &p) {
  int d = product(g) ? 2 : 3;
  if (curvature(g) == 1 && p[d] < 0) {
    double square = 0;
    for (int j = 0; j < d; ++j)
      square += p[j] * p[j];
    return square / (1 - p[d]);
  }
  return 1 + p[d];
}
inline Vec3 from_embedding(Geometry g, const V4 &p, double lift = 0) {
  if (g == Geometry::sl2r)
    return sl_chart(p, lift);
  if (!curvature(g))
    return {p[0] / p[3], p[1] / p[3], p[2] / p[3]};
  int d = product(g) ? 2 : 3;
  double den = chart_denominator(g, p);
  require(den > 0, "Point is at a stereographic chart pole");
  Vec3 out{};
  for (int i = 0; i < d; ++i)
    out[i] = p[i] / den;
  if (d == 2)
    out[2] = p[3] + lift;
  return out;
}
inline Vec3 from_embed_vector(Geometry g, const V4 &p, const V4 &v) {
  int d = product(g) ? 2 : 3;
  double den = chart_denominator(g, p);
  Vec3 out{};
  for (int i = 0; i < d; ++i)
    out[i] = (v[i] * den - p[i] * v[d]) / (den * den);
  if (d == 2)
    out[2] = v[3];
  return mv(inverse(frame(g, from_embedding(g, p))), out);
}
struct Flow {
  Vec3 p, u;
  M3 transport = identity3();
};
inline double flow_exp(double x) { return std::exp(x); }
inline Jet flow_exp(const Jet &x) { return exponential(x, x, 0); }
template <class T> using GroupState = std::array<T, 7>;
template <class T> inline GroupState<T> group_rhs(Geometry g, const GroupState<T> &state) {
  std::array<T, 4> p{state[0], state[1], state[2], state[3]};
  std::array<T, 3> u{state[4], state[5], state[6]};
  GroupState<T> out{};
  if (g == Geometry::nil) {
    out[0] = u[0];
    out[1] = u[1];
    out[2] = u[2] + .5 * (p[0] * u[1] - p[1] * u[0]);
    out[4] = -u[2] * u[1];
    out[5] = u[2] * u[0];
  } else if (g == Geometry::sol) {
    out[0] = flow_exp(p[2]) * u[0];
    out[1] = flow_exp(-p[2]) * u[1];
    out[2] = u[2];
    out[4] = u[0] * u[2];
    out[5] = -u[1] * u[2];
    out[6] = u[1] * u[1] - u[0] * u[0];
  } else {
    std::array<T, 4> tangent{T{}, .5 * u[2], .5 * u[0], .5 * u[1]};
    auto velocity = sl_multiply(p, tangent);
    for (int j = 0; j < 4; ++j)
      out[j] = velocity[j];
    out[4] = 2.0 * (u[1] * u[2]);
    out[5] = -2.0 * (u[0] * u[2]);
  }
  return out;
}
template <class T>
inline GroupState<T> integrate_group(Geometry g, GroupState<T> state, double t, double max_step) {
  auto advance = [](const GroupState<T> &s, const GroupState<T> &v, double h) {
    GroupState<T> out;
    for (int j = 0; j < 7; ++j)
      out[j] = s[j] + h * v[j];
    return out;
  };
  int steps = std::max(1, int(std::ceil(std::abs(t) / max_step)));
  double h = t / steps;
  for (int i = 0; i < steps; ++i) {
    auto k1 = group_rhs(g, state), k2 = group_rhs(g, advance(state, k1, h / 2));
    auto k3 = group_rhs(g, advance(state, k2, h / 2)), k4 = group_rhs(g, advance(state, k3, h));
    for (int j = 0; j < 7; ++j)
      state[j] = state[j] + (h / 6) * (k1[j] + 2.0 * k2[j] + 2.0 * k3[j] + k4[j]);
  }
  return state;
}
inline Flow sol_short(const Vec3 &initial, double t) {
  GroupState<double> state{0, 0, 0, 1, initial[0], initial[1], initial[2]};
  state = integrate_group(Geometry::sol, state, t, .005);
  return {{state[0], state[1], state[2]}, {state[4], state[5], state[6]}, identity3()};
}
struct Geodesic {
  Geometry g;
  Vec3 origin, initial;
  double a = 0, b = 0, c = 0, k = 0, kp = 0, mu = 0, alpha = 0;
  std::shared_ptr<Elliptic> elliptic;
  std::array<double, 4> at_alpha{};
  Geodesic(Geometry type, Vec3 p, Vec3 u) : g(type), origin(p), initial(unit(u)) {
    frame(g, p);
    a = std::abs(initial[0]);
    b = std::abs(initial[1]);
    c = initial[2];
    if (g == Geometry::sol && elliptic_conditioned() && 1 - 2 * a * b > 1e-14) {
      mu = std::sqrt(1 + 2 * a * b);
      k = std::sqrt(std::max(0.0, (1 - 2 * a * b) / (1 + 2 * a * b)));
      kp = 2 * std::sqrt(a * b / (1 + 2 * a * b));
      elliptic = std::make_shared<Elliptic>(k);
      alpha = elliptic->first(std::atan2(-c, a - b));
      at_alpha = elliptic->evaluate(alpha);
    }
  }
  // Position differences in the elliptic formula amplify roundoff by 1/min(a,b).
  bool elliptic_conditioned() const { return std::min(a, b) >= 1e-3; }
  Flow sol(double t) const {
    if (std::abs(t) < 1e-4 || (a * b > 0 && !elliptic_conditioned()))
      return sol_short(initial, t);
    if (a == 0 && b == 0)
      return {{0, 0, c * t}, initial, identity3()};
    if (a == 0 || b == 0) {
      double sign = a == 0 ? 1 : -1, A = 1 + sign * c, B = 1 - sign * c;
      if (sign * c >= 0)
        B = (a * a + b * b) / A;
      else
        A = (a * a + b * b) / B;
      double positive = A * std::exp(t), negative = B * std::exp(-t), den = (positive + negative) / 2;
      Flow out;
      out.p = {0, 0, sign * std::log(den)};
      out.u = {0, 0, sign * (positive - negative) / (positive + negative)};
      int j = a == 0 ? 1 : 0;
      out.p[j] = initial[j] * std::sinh(t) / den;
      out.u[j] = initial[j] / den;
      return out;
    }
    if (!elliptic) {
      if (c == 0 && a == b)
        return {{initial[0] * t, initial[1] * t, 0}, initial, identity3()};
      return sol_short(initial, t);
    }
    double s = mu * t + alpha;
    auto e = elliptic->evaluate(s);
    double common = (e[3] - at_alpha[3]) / kp - kp * (s - alpha) / 2, delta = k * (e[0] - at_alpha[0]) / kp;
    double sx = std::copysign(1.0, initial[0]), sy = std::copysign(1.0, initial[1]);
    Flow out{{sx * std::sqrt(b / a) * (common + delta), sy * std::sqrt(a / b) * (common - delta),
              .5 * std::log(b / a) + std::asinh(k * e[1] / kp)},
             {sx * std::sqrt(a * b) * (k * e[1] + e[2]) / kp, sy * std::sqrt(a * b) * kp / (k * e[1] + e[2]),
              -k * mu * e[0]},
             identity3()};
    return out;
  }
  Flow at(double t, bool transport = false) const {
    Flow out{origin, initial, identity3()};
    if (t == 0)
      return out;
    int curvature_sign = curvature(g);
    if (curvature_sign) {
      int d = product(g) ? 2 : 3;
      double speed = product(g) ? std::hypot(initial[0], initial[1]) : 1;
      V4 p = embedding(g, origin), v = embed_vector(g, origin, initial), q = p, dq = v;
      double angle = speed * t, C = curvature_sign == 1 ? std::cos(angle) : std::cosh(angle),
             S = speed == 0 ? t : (curvature_sign == 1 ? std::sin(angle) : std::sinh(angle)) / speed;
      for (int i = 0; i <= d; ++i) {
        q[i] = C * p[i] + S * v[i];
        dq[i] = -curvature_sign * speed * speed * S * p[i] + C * v[i];
      }
      if (d == 2) {
        q[3] = origin[2] + initial[2] * t;
        dq[3] = initial[2];
      }
      out.p = from_embedding(g, q);
      out.u = from_embed_vector(g, q, dq);
      if (transport)
        for (int j = 0; j < 3; ++j) {
          Vec3 ej{};
          ej[j] = 1;
          V4 w = embed_vector(g, origin, ej);
          double dot = 0;
          for (int i = 0; i < d; ++i)
            dot += v[i] * w[i];
          dot += curvature_sign * v[d] * w[d];
          if (speed > 0)
            for (int i = 0; i <= d; ++i)
              w[i] += dot * (dq[i] - v[i]) / (speed * speed);
          Vec3 column = from_embed_vector(g, q, w);
          for (int i = 0; i < 3; ++i)
            out.transport[i][j] = column[i];
        }
      return out;
    }
    if (g == Geometry::e3) {
      out.p = point(origin, initial, t);
      return out;
    }
    if (g == Geometry::nil) {
      double ct = c * t,
             sinc = std::abs(ct) < 1e-4 ? 1 - ct * ct / 6 + std::pow(ct, 4) / 120 : std::sin(ct) / ct;
      double cosc = std::abs(ct) < 1e-4 ? ct / 2 - std::pow(ct, 3) / 24 + std::pow(ct, 5) / 720
                                        : (1 - std::cos(ct)) / ct;
      double zterm = std::abs(ct) < 1e-3 ? ct / 6 - std::pow(ct, 3) / 120 + std::pow(ct, 5) / 5040
                                         : (ct - std::sin(ct)) / (ct * ct);
      out.p = compose(g, origin,
                      {t * (initial[0] * sinc - initial[1] * cosc),
                       t * (initial[1] * sinc + initial[0] * cosc), ct + .5 * (1 - c * c) * t * t * zterm});
      out.u = mv(rotation({0, 0, 1}, ct), initial);
      if (transport)
        out.transport = mm(rotation({0, 0, 1}, ct), rotation(initial, -t / 2));
      return out;
    }
    if (g == Geometry::sl2r) {
      double horizontal = std::hypot(initial[0], initial[1]), delta = c * c - horizontal * horizontal,
             kap = std::sqrt(std::abs(delta)), s = t / 2, C = 1;
      if (kap > 1e-8) {
        C = delta > 0 ? std::cos(kap * t / 2) : std::cosh(kap * t / 2);
        s = (delta > 0 ? std::sin(kap * t / 2) : std::sinh(kap * t / 2)) / kap;
      }
      V4 eta{C, -c * s, initial[0] * s, initial[1] * s}, spin{std::cos(c * t), std::sin(c * t), 0, 0},
          q = sl_multiply(eta, spin);
      double phi = std::atan2(-c * s, C);
      if (delta > 0) {
        double theta = kap * t / 2;
        double wrapped = std::atan2(std::sin(theta), std::cos(theta));
        phi -= std::copysign(1.0, c) * (theta - wrapped);
      }
      Vec3 local = sl_chart(q, 2 * c * t + 2 * phi);
      out.p = compose(g, origin, local);
      out.u = mv(rotation({0, 0, 1}, -2 * c * t), initial);
      if (transport)
        out.transport = mm(rotation({0, 0, 1}, -2 * c * t), rotation(initial, t / 2));
      return out;
    }
    out = sol(t);
    out.p = compose(g, origin, out.p);
    if (transport) {
      int full = int(std::floor(std::abs(t) / .05));
      double step = std::copysign(.05, t), root3 = std::sqrt(3.0);
      M3 Q = identity3();
      for (int i = 0; i <= full; ++i) {
        double h = i == full ? t - full * step : step;
        if (h == 0)
          continue;
        Vec3 early = sol(i * step + (.5 - root3 / 6) * h).u;
        Vec3 late = sol(i * step + (.5 + root3 / 6) * h).u;
        Vec3 a{early[1], early[0], 0}, b{late[1], late[0], 0};
        for (int stage = 0; stage < 2; ++stage) {
          double w = .25 + (stage == 0 ? 1 : -1) * root3 / 6;
          Vec3 axis{};
          for (int j = 0; j < 3; ++j)
            axis[j] = w * a[j] + (.5 - w) * b[j];
          double length = norm(axis);
          if (length > 0)
            Q = mm(rotation(axis, length * h), Q);
        }
      }
      out.transport = Q;
    }
    return out;
  }
};
using JacobiState = GroupState<Jet>;
inline double jacobi_area(const Geodesic &ray, double t) {
  Vec3 axis = std::abs(ray.initial[2]) < .9 ? Vec3{0, 0, 1} : Vec3{1, 0, 0};
  Vec3 a = unit(engine::cross(ray.initial, axis)), b = engine::cross(ray.initial, a);
  JacobiState state{};
  state[ray.g == Geometry::sl2r ? 0 : 3].v = 1;
  for (int j = 0; j < 3; ++j)
    state[j + 4] = Jet{ray.initial[j], {a[j], b[j], 0}};
  state = integrate_group(ray.g, state, t, .01);
  Vec3 columns[2];
  for (int k = 0; k < 2; ++k) {
    if (ray.g == Geometry::sl2r) {
      V4 inv{state[0].v, -state[1].v, -state[2].v, -state[3].v};
      V4 derivative{state[0].d[k], state[1].d[k], state[2].d[k], state[3].d[k]};
      V4 relative = sl_multiply(inv, derivative);
      columns[k] = {2 * relative[2], 2 * relative[3], 2 * relative[1]};
    } else {
      Vec3 p{state[0].v, state[1].v, state[2].v}, derivative{state[0].d[k], state[1].d[k], state[2].d[k]};
      columns[k] = mv(inverse(frame(ray.g, p)), derivative);
    }
  }
  return norm(engine::cross(columns[0], columns[1]));
}
inline double area_density(const Geodesic &ray, double t) {
  if (ray.g == Geometry::e3)
    return t * t;
  if (ray.g == Geometry::s3)
    return std::pow(std::sin(t), 2);
  if (ray.g == Geometry::h3)
    return std::pow(std::sinh(t), 2);
  if (product(ray.g)) {
    double a = std::hypot(ray.initial[0], ray.initial[1]);
    if (a == 0)
      return t * t;
    return std::abs(t * (curvature(ray.g) == 1 ? std::sin(a * t) : std::sinh(a * t)) / a);
  }
  return jacobi_area(ray, t);
}
// A coordinate-speed bound on an arc of length h converts chart Lipschitz bounds to safe steps.
inline double speed_bound(Geometry g, const Vec3 &p, double h) {
  if (g == Geometry::nil)
    return std::sqrt(2 + std::pow(1 + (std::hypot(p[0], p[1]) + h) / 2, 2));
  if (g == Geometry::sol)
    return std::exp(std::abs(p[2]) + h);
  if (g == Geometry::sl2r)
    return std::sqrt(std::pow(std::cosh(std::asinh(std::hypot(p[0], p[1])) + h), 2) + 4);
  if (g == Geometry::h3)
    return .5;
  if (g == Geometry::h2xe)
    return 1;
  if (curvature(g) == 1) {
    double r = product(g) ? std::hypot(p[0], p[1]) : norm(p), angle = 2 * std::atan(r) + h;
    if (angle >= engine::pi)
      return std::numeric_limits<double>::infinity();
    double bound = .5 * (1 + std::pow(std::tan(angle / 2), 2));
    return product(g) ? std::max(1.0, bound) : bound;
  }
  return 1;
}
} // namespace raymarch
} // namespace ggplot3d
