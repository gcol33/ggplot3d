#pragma once
#include "kernel.h"

namespace ggplot3d {
namespace raymarch {
template <class F>
inline double simpson(F f, double a, double b, double fa, double fm, double fb, double whole, double eps,
                      int depth) {
  double m = (a + b) / 2, l = (a + m) / 2, r = (m + b) / 2, fl = f(l), fr = f(r);
  double left = (m - a) * (fa + 4 * fl + fm) / 6, right = (b - m) * (fm + 4 * fr + fb) / 6,
         delta = left + right - whole;
  if (depth == 0 || std::abs(delta) < 15 * eps)
    return left + right + delta / 15;
  return simpson(f, a, m, fa, fl, fm, left, eps / 2, depth - 1) +
         simpson(f, m, b, fm, fr, fb, right, eps / 2, depth - 1);
}
template <class F> inline double integral(F f, double a, double b) {
  if (a == b)
    return 0;
  double fa = f(a), fm = f((a + b) / 2), fb = f(b);
  return simpson(f, a, b, fa, fm, fb, (b - a) * (fa + 4 * fm + fb) / 6, 2e-12, 24);
}
struct Elliptic {
  double m, K, E, mean;
  std::vector<double> ratios;
  explicit Elliptic(double modulus) : m(modulus * modulus) {
    double a = 1, b = std::sqrt(1 - m);
    for (int i = 0; i < 32; ++i) {
      double c = (a - b) / 2, next = (a + b) / 2;
      ratios.push_back(c / next);
      b = std::sqrt(a * b);
      a = next;
      if (std::abs(c) < 2e-16 * a)
        break;
    }
    mean = a;
    K = engine::pi / (2 * a);
    E = integral([&](double x) { return std::sqrt(1 - m * std::pow(std::sin(x), 2)); }, 0, engine::pi / 2);
  }
  double first(double phi) const {
    double periods = std::floor((phi + engine::pi / 2) / engine::pi), p = phi - periods * engine::pi;
    return 2 * periods * K +
           integral([&](double x) { return 1 / std::sqrt(1 - m * std::pow(std::sin(x), 2)); }, 0, p);
  }
  // The AGM descent returns the amplitude on one monotone half-period.
  std::array<double, 4> evaluate(double u) const {
    double periods = std::floor((u + K) / (2 * K)), v = u - 2 * periods * K;
    double phi = std::ldexp(mean * v, int(ratios.size()));
    for (auto it = ratios.rbegin(); it != ratios.rend(); ++it)
      phi = (phi + std::asin(std::max(-1.0, std::min(1.0, *it * std::sin(phi))))) / 2;
    double sn = std::sin(phi), cn = std::cos(phi), dn = std::sqrt(1 - m * sn * sn);
    double epsilon = 2 * periods * E +
                     integral([&](double x) { return std::sqrt(1 - m * std::pow(std::sin(x), 2)); }, 0, phi);
    if (std::fmod(std::abs(periods), 2) == 1) {
      sn = -sn;
      cn = -cn;
    }
    return {sn, cn, dn, epsilon};
  }
};
} // namespace raymarch
} // namespace ggplot3d
