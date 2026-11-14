#pragma once
#include <algorithm>
#include <array>
#include <cmath>
#include <string>
#include <vector>

// Local illumination of one surface point in linear light (Blinn-Phong:
// Phong 1975, Blinn 1977). Every vector is in one model space, aspect-scaled:
// normals are unit, `toward` is the unit direction from the point to the eye,
// point lights and positions share the space.
//
// The light terms and their combination are written once, in a subset of C++
// that is also GLSL ES 3.0, inside the macros below. The C++ kernel compiles
// them; shade_glsl() returns the same text, which the viewer compiles as its
// fragment shader. A new light type is one GGPLOT3D_LIGHT_TERMS entry: given
// the unit normal n, the position p, the direction toward the eye v and the
// light's vector lv, colour lc, falloff lf and second colour lg, it sets the
// unit direction toward the light L, the incident colour c, and a = 1 for a
// term that only adds to the ambient light.
#define GGPLOT3D_LIGHT_TERMS(X) \
  X(1, directional, L = lv; c = lc; a = 0.0;) \
  X(2, point, vec3 d = lv - p; real r = max(length(d), 1e-3); L = d / r; c = lc / pow(r, lf); a = 0.0;) \
  X(3, head, L = v; c = lc; a = 0.0;) \
  X(4, hemisphere, L = lv; c = mix(lg, lc, 0.5 + 0.5 * dot(n, lv)); a = 1.0;)

// Accumulates one light into the ambient (amb), diffuse (dif) and Blinn
// specular (spe) sums; ks and sh are the material's specular strength and
// exponent.
#define GGPLOT3D_LIGHT_SUM \
  if (a > 0.5) { amb = amb + c; } else { \
    real nl = max(dot(n, L), 0.0); \
    dif = dif + c * nl; \
    if (nl > 0.0 && ks > 0.0) { vec3 h = normalize(L + v); spe = spe + c * pow(max(dot(n, h), 0.0), sh); } \
  }

// The lit colour from the base colour b: the ambient and diffuse sums scale it
// (multiply, mt = 1) or its distance from white (tint, mt = 0), weighted by the
// material's ka and kd; the specular sum adds a highlight whose colour moves
// from white toward the base colour as sc goes from 1 to 0.
#define GGPLOT3D_LIGHT_FINISH \
  vec3 lit = amb * ka + dif * kd; \
  vec3 sc3 = mix(b, vec3(1.0, 1.0, 1.0), sc); \
  vec3 col = mt > 0.5 ? b * lit : vec3(1.0, 1.0, 1.0) - (vec3(1.0, 1.0, 1.0) - b) * lit; \
  col = col + sc3 * spe * ks;

#define GGPLOT3D_STRINGIFY(...) #__VA_ARGS__
#define GGPLOT3D_XSTRINGIFY(...) GGPLOT3D_STRINGIFY(__VA_ARGS__)

namespace ggplot3d { namespace shade {
using real = double;
struct vec3 {
  real x = 0, y = 0, z = 0;
  vec3() = default;
  vec3(real a, real b, real c) : x(a), y(b), z(c) {}
  real operator[](int k) const {return k == 0 ? x : k == 1 ? y : z;}
};
static inline vec3 operator+(vec3 a, vec3 b) {return {a.x+b.x, a.y+b.y, a.z+b.z};}
static inline vec3 operator-(vec3 a, vec3 b) {return {a.x-b.x, a.y-b.y, a.z-b.z};}
static inline vec3 operator*(vec3 a, vec3 b) {return {a.x*b.x, a.y*b.y, a.z*b.z};}
static inline vec3 operator*(vec3 a, real s) {return {a.x*s, a.y*s, a.z*s};}
static inline vec3 operator*(real s, vec3 a) {return a*s;}
static inline vec3 operator/(vec3 a, real s) {return {a.x/s, a.y/s, a.z/s};}
static inline vec3 operator-(vec3 a) {return {-a.x, -a.y, -a.z};}
static inline real dot(vec3 a, vec3 b) {return a.x*b.x+a.y*b.y+a.z*b.z;}
static inline real length(vec3 a) {return std::sqrt(dot(a, a));}
static inline vec3 normalize(vec3 a) {real l = length(a); return l > 0 ? a/l : a;}
static inline real max(real a, real b) {return a > b ? a : b;}
static inline real pow(real a, real b) {return std::pow(a, b);}
static inline vec3 mix(vec3 a, vec3 b, real t) {return a*(1-t)+b*t;}
static inline vec3 to_vec3(const std::array<double, 3>& a) {return {a[0], a[1], a[2]};}

// A light: kind (a GGPLOT3D_LIGHT_TERMS id), its vector (direction toward the
// light, a position, or the up direction), linear colour times intensity,
// falloff exponent and second colour.
struct Light {
  int kind = 1;
  vec3 vector{0, 0, 1}, colour{1, 1, 1}, ground{0, 0, 0};
  real falloff = 0;
};
struct Lights {
  std::vector<Light> list;
  vec3 ambient{0.35, 0.35, 0.35};
  bool multiply = true;
};
// Strengths of the ambient, diffuse and specular terms, the specular exponent
// and colour reflectance, and an optional colour for the back side of faces.
struct Material {
  real ambient = 1, diffuse = 1, specular = 0, shininess = 10, specular_colour = 1;
  bool has_back = false;
  vec3 back{0, 0, 0};
};

// The lit colour of base colour b at unit normal n, position p, seen along v
// (unit, toward the eye). A normal facing away from the eye is turned toward
// it, and the back colour replaces the base colour there when the material
// has one.
static inline vec3 shade(vec3 n, vec3 p, vec3 v, vec3 b, const Material& m, const Lights& lights) {
  if (dot(n, v) < 0) {
    n = -n;
    if (m.has_back) b = m.back;
  }
  const real ka = m.ambient, kd = m.diffuse, ks = m.specular, sh = m.shininess, sc = m.specular_colour;
  const real mt = lights.multiply ? 1 : 0;
  vec3 amb = lights.ambient, dif(0, 0, 0), spe(0, 0, 0);
  for (const Light& light : lights.list) {
    const vec3 lv = light.vector, lc = light.colour, lg = light.ground;
    const real lf = light.falloff;
    vec3 L(0, 0, 1), c(0, 0, 0);
    real a = 0;
    switch (light.kind) {
#define GGPLOT3D_TERM(id, name, ...) case id: { __VA_ARGS__ } break;
      GGPLOT3D_LIGHT_TERMS(GGPLOT3D_TERM)
#undef GGPLOT3D_TERM
    }
    GGPLOT3D_LIGHT_SUM
  }
  GGPLOT3D_LIGHT_FINISH
  return col;
}

static inline real transfer(real linear, real brightness, bool multiply) {
  return multiply ? linear*brightness : 1-(1-linear)*brightness;
}

// The kernel as a GLSL ES 3.0 function `vec3 shade(vec3 n, vec3 p, vec3 v,
// vec3 b, vec4 material, vec4 material2)` over uniform light arrays of at
// most `max_lights` entries; material = (ka, kd, ks, shininess), material2 =
// (specular colour reflectance, back colour, or a negative red for none).
static inline std::string shade_glsl(int max_lights) {
  std::string terms;
#define GGPLOT3D_TERM(id, name, ...) terms += "case " #id ": { " GGPLOT3D_STRINGIFY(__VA_ARGS__) " } break; ";
  GGPLOT3D_LIGHT_TERMS(GGPLOT3D_TERM)
#undef GGPLOT3D_TERM
  const std::string count = std::to_string(max_lights);
  return std::string("#define real float\n") +
    "uniform int uLightCount, uLightKind[" + count + "]; uniform vec3 uLightVector[" + count + "], uLightColour[" + count +
    "], uLightGround[" + count + "]; uniform float uLightFalloff[" + count + "]; uniform vec3 uAmbient; uniform float uMultiply;\n"
    "vec3 shade(vec3 n, vec3 p, vec3 v, vec3 b, vec4 material, vec4 material2) {\n"
    "  if (dot(n, v) < 0.0) { n = -n; if (material2.y >= 0.0) b = material2.yzw; }\n"
    "  real ka = material.x, kd = material.y, ks = material.z, sh = material.w, sc = material2.x, mt = uMultiply;\n"
    "  vec3 amb = uAmbient, dif = vec3(0.0), spe = vec3(0.0);\n"
    "  for (int i = 0; i < " + count + "; i++) {\n"
    "    if (i >= uLightCount) break;\n"
    "    vec3 lv = uLightVector[i], lc = uLightColour[i], lg = uLightGround[i]; real lf = uLightFalloff[i];\n"
    "    vec3 L = vec3(0.0, 0.0, 1.0), c = vec3(0.0); real a = 0.0;\n"
    "    switch (uLightKind[i]) { " + terms + "}\n"
    "    " GGPLOT3D_XSTRINGIFY(GGPLOT3D_LIGHT_SUM) "\n"
    "  }\n"
    "  " GGPLOT3D_XSTRINGIFY(GGPLOT3D_LIGHT_FINISH) "\n"
    "  return col;\n"
    "}\n";
}
} }
