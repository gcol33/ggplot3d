#pragma once

#include <array>
#include <cmath>

// Arithmetic on 3-vectors shared by the scene engine, the geometry kernels and
// the ray marcher.
namespace ggplot3d { namespace vec3 {
using Vec3 = std::array<double, 3>;

inline Vec3 add(const Vec3& a, const Vec3& b) { return {a[0]+b[0], a[1]+b[1], a[2]+b[2]}; }
inline Vec3 sub(const Vec3& a, const Vec3& b) { return {a[0]-b[0], a[1]-b[1], a[2]-b[2]}; }
inline Vec3 mul(const Vec3& a, double s) { return {a[0]*s, a[1]*s, a[2]*s}; }
inline double dot(const Vec3& a, const Vec3& b) { return a[0]*b[0] + a[1]*b[1] + a[2]*b[2]; }
inline Vec3 cross(const Vec3& a, const Vec3& b) {
  return {a[1]*b[2]-a[2]*b[1], a[2]*b[0]-a[0]*b[2], a[0]*b[1]-a[1]*b[0]};
}
inline double norm(const Vec3& a) { return std::sqrt(dot(a, a)); }
// A vector of zero (or undefined) length is returned unchanged.
inline Vec3 unit(const Vec3& a) {
  const double n = norm(a);
  return n > 0 ? mul(a, 1/n) : a;
}
} }
