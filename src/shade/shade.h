#pragma once
#include <algorithm>
#include <array>
#include <cmath>
#include <vector>

// Local illumination of one surface point in linear light. Every vector is in
// aspect-scaled model space: normals are unit, `toward` is the unit direction
// from the point to the eye.
namespace ggplot3d { namespace shade {
using Vec3 = std::array<double, 3>;

static inline double dot(const Vec3& a, const Vec3& b) {return a[0]*b[0]+a[1]*b[1]+a[2]*b[2];}

// A directional light: unit direction from the surface toward the light, the
// ambient and diffuse strengths, and the colour transfer of the brightness
// (multiply the colour, or scale its distance from white).
struct Light {
  Vec3 direction{0, 0, 1};
  double ambient = 0.35, diffuse = 0.65;
  bool multiply = true;
};

static inline double transfer(double linear, double brightness, bool multiply) {
  return multiply ? linear*brightness : 1-(1-linear)*brightness;
}

// The normal turned toward the eye, so both sides of a face are lit.
static inline Vec3 facing(Vec3 normal, const Vec3& toward) {
  if (dot(normal, toward) < 0) for (double& x : normal) x = -x;
  return normal;
}

// Lambert brightness (Lambert 1760) of a unit normal turned toward the eye.
static inline double brightness(const Vec3& normal, const Vec3& toward, const Light& light) {
  return light.ambient + light.diffuse*std::max(0.0, dot(facing(normal, toward), light.direction));
}

static inline Vec3 shade(const Vec3& normal, const Vec3& toward, const Vec3& base, const Light& light) {
  const double b = brightness(normal, toward, light);
  return {transfer(base[0], b, light.multiply), transfer(base[1], b, light.multiply),
          transfer(base[2], b, light.multiply)};
}

static inline Vec3 unit(Vec3 x) {
  const double length = std::sqrt(dot(x, x));
  if (length > 0) for (double& v : x) v /= length;
  return x;
}
} }
