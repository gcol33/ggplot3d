#pragma once
#include <algorithm>
#include <array>
#include <cmath>
#include <string>
#include <vector>

// Local illumination of one surface point in linear light (Blinn-Phong:
// Phong 1975, Blinn 1977) with colour transfers. Every vector is in one model
// space, aspect-scaled: normals are unit, `toward` is the unit direction from
// the point to the eye, point lights and positions share the space.
//
// The helper functions, light terms and their combination are written once,
// in a subset of C++ that is also GLSL ES 3.0, inside the macros below. The
// C++ kernel compiles them; shade_glsl() returns the same text, which the
// viewer compiles as its fragment shader. A new light type is one
// GGPLOT3D_LIGHT_TERMS entry: given the unit normal n, the position p, the
// direction toward the eye v and the light's vector lv, colour lc, falloff lf
// and second colour lg, it sets the unit direction toward the light L, the
// incident colour c, and a = 1 for a term that only adds to the ambient light.
#define GGPLOT3D_LIGHT_TERMS(X) \
  X(1, directional, L = lv; c = lc; a = 0.0;) \
  X(2, point, vec3 d = lv - p; real r = max(length(d), 1e-3); L = d / r; c = lc / pow(r, lf); a = 0.0;) \
  X(3, head, L = v; c = lc; a = 0.0;) \
  X(4, hemisphere, L = lv; c = mix(lg, lc, 0.5 + 0.5 * dot(n, lv)); a = 1.0;)

// Colour helpers. OKLab follows Ottosson (2020); the sRGB curve is IEC
// 61966-2-1. shade_fit() keeps an OKLab colour's lightness and hue and
// scales its chroma down until the colour lies in the linear sRGB cube.
// shade_level() is the signed response of a light value x (cosine of the
// angle between the normal and the light) around the neutral cosine c0, as in
// ggcube's ramp_smooth(); shade_band() counts the explicit band breaks
// (cosines, packed in two vec4s) that x lies at or beyond.
#define GGPLOT3D_SHADE_FUNCTIONS(FN) \
  FN real shade_cbrt(real x) { return x < 0.0 ? -pow(-x, 1.0 / 3.0) : pow(x, 1.0 / 3.0); } \
  FN vec3 shade_oklab(vec3 c) { \
    real l = shade_cbrt(0.4122214708 * c.x + 0.5363325363 * c.y + 0.0514459929 * c.z); \
    real m = shade_cbrt(0.2119034982 * c.x + 0.6806995451 * c.y + 0.1073969566 * c.z); \
    real s = shade_cbrt(0.0883024619 * c.x + 0.2817188376 * c.y + 0.6299787005 * c.z); \
    return vec3(0.2104542553 * l + 0.7936177850 * m - 0.0040720468 * s, \
                1.9779984951 * l - 2.4285922050 * m + 0.4505937099 * s, \
                0.0259040371 * l + 0.7827717662 * m - 0.8086757660 * s); } \
  FN vec3 shade_linear(vec3 c) { \
    real l = c.x + 0.3963377774 * c.y + 0.2158037573 * c.z; \
    real m = c.x - 0.1055613458 * c.y - 0.0638541728 * c.z; \
    real s = c.x - 0.0894841775 * c.y - 1.2914855480 * c.z; \
    l = l * l * l; m = m * m * m; s = s * s * s; \
    return vec3(4.0767416621 * l - 3.3077115913 * m + 0.2309699292 * s, \
                -1.2684380046 * l + 2.6097574011 * m - 0.3413193965 * s, \
                -0.0041960863 * l - 0.7034186147 * m + 1.7076147010 * s); } \
  FN bool shade_inside(vec3 c) { \
    return min(min(c.x, c.y), c.z) >= -1e-5 && max(max(c.x, c.y), c.z) <= 1.0 + 1e-5; } \
  FN vec3 shade_fit(vec3 lab) { \
    lab.x = clamp(lab.x, 0.0, 1.0); \
    vec3 c = shade_linear(lab); \
    if (!shade_inside(c)) { \
      real lo = 0.0; real hi = 1.0; \
      for (int i = 0; i < 24; i++) { \
        real k = 0.5 * (lo + hi); \
        if (shade_inside(shade_linear(vec3(lab.x, lab.y * k, lab.z * k)))) lo = k; else hi = k; \
      } \
      c = shade_linear(vec3(lab.x, lab.y * lo, lab.z * lo)); \
    } \
    return vec3(clamp(c.x, 0.0, 1.0), clamp(c.y, 0.0, 1.0), clamp(c.z, 0.0, 1.0)); } \
  FN real shade_encode1(real x) { x = clamp(x, 0.0, 1.0); return x <= 0.0031308 ? 12.92 * x : 1.055 * pow(x, 1.0 / 2.4) - 0.055; } \
  FN real shade_decode1(real x) { return x <= 0.04045 ? x / 12.92 : pow((x + 0.055) / 1.055, 2.4); } \
  FN vec3 shade_encode(vec3 c) { return vec3(shade_encode1(c.x), shade_encode1(c.y), shade_encode1(c.z)); } \
  FN vec3 shade_decode(vec3 c) { return vec3(shade_decode1(c.x), shade_decode1(c.y), shade_decode1(c.z)); } \
  FN real shade_overlay1(real e, real i) { return e <= 0.5 ? 2.0 * i * e : 1.0 - 2.0 * (1.0 - i) * (1.0 - e); } \
  FN real shade_level(real x, real c0) { \
    real t = x >= c0 ? (x - c0) / max(1.0 - c0, 1e-9) : (x - c0) / max(1.0 + c0, 1e-9); \
    return clamp(t, -1.0, 1.0); } \
  FN real shade_break(vec4 a, vec4 b, int i) { \
    return i == 0 ? a.x : i == 1 ? a.y : i == 2 ? a.z : i == 3 ? a.w : i == 4 ? b.x : i == 5 ? b.y : i == 6 ? b.z : b.w; } \
  FN real shade_band(real x, vec4 a, vec4 b, int count) { \
    real band = 0.0; \
    for (int i = 0; i < 8; i++) { if (i < count && x <= shade_break(a, b, i) + 1e-6) band = band + 1.0; } \
    return band; }

// Locals the kernel body reads. Material: ka, kd, ks, sh (strengths and
// specular exponent), sc (specular colour reflectance), st (style: 0
// standard, 1 toon, 2 Gooch, 3 normals), ss (1 for a quantised specular
// spot), nb (number of toon bands, or the normal frame: 0 model, 1 camera,
// 2 light), nk (number of explicit toon breaks), pa, pb (Gooch cool colour
// and alpha, warm colour and beta; or the toon break cosines). Lights: tm
// (transfer: 0 multiply, 1 tint, 2 lightness, 3 tone, 4 overlay, 5 soft),
// r0, r1 (moves at full shadow and full highlight), c0 (neutral cosine),
// tsh, thi (tone endpoint colours), fs, fr, fu, ft (model scale and camera
// right, up and toward axes). xs is a fixed light value in [-1, 1] that
// replaces n . L for every light when it is at least -1 (swatch sampling),
// and no specular term is added then.
//
// Accumulates one light: ambient (amb), diffuse (dif) and Blinn specular
// (spe) sums, the weighted signed level (lsum) and Gooch blend (gsum) with
// their weight (wsum), and the first light direction (l0).
#define GGPLOT3D_LIGHT_SUM \
  if (a > 0.5) { amb = amb + c; } else { \
    real x = xs >= -1.0 ? xs : dot(n, L); \
    real nl = max(x, 0.0); \
    real lvl = shade_level(x, c0); \
    if (st > 0.5 && st < 1.5) { \
      if (nk > 0.5) { \
        real band = shade_band(x, pa, pb, int(nk + 0.5)); \
        nl = 1.0 - band / max(nk, 1.0); \
        lvl = 2.0 * nl - 1.0; \
      } else { \
        real nbands = max(nb, 2.0); \
        nl = min(floor(nl * nbands), nbands - 1.0) / (nbands - 1.0); \
        real bk = min(floor((lvl + 1.0) * 0.5 * nbands), nbands - 1.0); \
        lvl = -1.0 + 2.0 * bk / (nbands - 1.0); \
      } \
    } \
    real w = dot(c, vec3(0.2126, 0.7152, 0.0722)); \
    if (wl < 0.5) { l0 = L; wl = 1.0; } \
    lsum = lsum + w * lvl; gsum = gsum + w * 0.5 * (1.0 + x); wsum = wsum + w; \
    dif = dif + c * nl; \
    if (xs < -1.0 && max(dot(n, L), 0.0) > 0.0 && ks > 0.0) { \
      vec3 h = normalize(L + v); \
      real g = pow(max(dot(n, h), 0.0), sh); \
      if (ss > 0.5) g = g >= 0.5 ? 1.0 : 0.0; \
      spe = spe + c * g; \
    } \
  }

// The lit colour from the base colour b. Standard and toon styles pass the
// ambient and diffuse sums, weighted by ka and kd, through the transfer:
// multiply scales b, tint scales its distance from white; the signed
// transfers move b by the level (weighted mean over the lights, scaled by
// kd): OKLab lightness toward 0 or 1 by up to the fraction r0 (shadow) or r1
// (highlight), an OKLab mix toward the tone colours by those fractions, or
// matplotlib's overlay and soft-light blends of the encoded colour with an
// intensity of 0.5 plus half the move; a move within 1e-7 of zero keeps b
// exactly. Gooch blends the cool and warm colours (each plus alpha or beta
// times b) by the mean of (1 + n . L) / 2 (Gooch et al. 1998). The specular
// sum adds a highlight whose colour moves from white toward b as sc goes from
// 1 to 0. Normals encode the unit normal in the selected frame as (n + 1) / 2
// in encoded sRGB and take no light.
#define GGPLOT3D_LIGHT_FINISH \
  vec3 lit = amb * ka + dif * kd; \
  vec3 sc3 = mix(b, vec3(1.0, 1.0, 1.0), sc); \
  real level = wsum > 0.0 ? clamp(kd * lsum / wsum, -1.0, 1.0) : 0.0; \
  real move = level < 0.0 ? -level * r0 : level * r1; \
  vec3 col = b; \
  if (st > 2.5) { \
    vec3 q = n; \
    if (nb > 1.5) { \
      vec3 z = wl > 0.5 ? l0 : v; \
      vec3 xa = fr - z * dot(fr, z); \
      if (dot(xa, xa) < 1e-12) xa = fu - z * dot(fu, z); \
      xa = normalize(xa); \
      vec3 ya = vec3(z.y * xa.z - z.z * xa.y, z.z * xa.x - z.x * xa.z, z.x * xa.y - z.y * xa.x); \
      q = vec3(dot(n, xa), dot(n, ya), dot(n, z)); \
    } else if (nb > 0.5) { q = vec3(dot(n, fr), dot(n, fu), dot(n, ft)); } \
    else { q = normalize(n * fs); } \
    col = shade_decode(vec3(0.5 * q.x + 0.5, 0.5 * q.y + 0.5, 0.5 * q.z + 0.5)); \
  } else { \
    if (st > 1.5) { \
      real t = wsum > 0.0 ? gsum / wsum : 0.5; \
      col = mix(vec3(pa.x, pa.y, pa.z) + b * pa.w, vec3(pb.x, pb.y, pb.z) + b * pb.w, t); \
    } else if (tm < 0.5) { col = b * lit; } \
    else if (tm < 1.5) { col = vec3(1.0, 1.0, 1.0) - (vec3(1.0, 1.0, 1.0) - b) * lit; } \
    else if (abs(move) <= 1e-7) { col = b; } \
    else if (tm < 2.5) { vec3 lab = shade_oklab(b); lab.x = move < 0.0 ? lab.x * (1.0 + move) : lab.x + move * (1.0 - lab.x); col = shade_fit(lab); } \
    else if (tm < 3.5) { col = shade_fit(mix(shade_oklab(b), shade_oklab(level < 0.0 ? tsh : thi), clamp(abs(move), 0.0, 1.0))); } \
    else { \
      real i = 0.5 + 0.5 * (level < 0.0 ? -clamp(abs(move), 0.0, 1.0) : clamp(abs(move), 0.0, 1.0)); \
      vec3 e = shade_encode(b); \
      vec3 o = tm < 4.5 ? vec3(shade_overlay1(e.x, i), shade_overlay1(e.y, i), shade_overlay1(e.z, i)) : \
        e * 2.0 * i + e * e * (1.0 - 2.0 * i); \
      col = shade_decode(o); \
    } \
    col = col + sc3 * spe * ks; \
  }

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
struct vec4 {
  real x = 0, y = 0, z = 0, w = 0;
  vec4() = default;
  vec4(real a, real b, real c, real d) : x(a), y(b), z(c), w(d) {}
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
static inline real min(real a, real b) {return a < b ? a : b;}
static inline real clamp(real x, real lo, real hi) {return x < lo ? lo : x > hi ? hi : x;}
static inline real abs(real x) {return std::fabs(x);}
static inline real floor(real x) {return std::floor(x);}
static inline real pow(real a, real b) {return std::pow(a, b);}
static inline vec3 mix(vec3 a, vec3 b, real t) {return a*(1-t)+b*t;}
static inline vec3 to_vec3(const std::array<double, 3>& a) {return {a[0], a[1], a[2]};}

#define GGPLOT3D_CPP_FN static inline
GGPLOT3D_SHADE_FUNCTIONS(GGPLOT3D_CPP_FN)
#undef GGPLOT3D_CPP_FN

// A light: kind (a GGPLOT3D_LIGHT_TERMS id), its vector (direction toward the
// light, a position, or the up direction), linear colour times intensity,
// falloff exponent and second colour.
struct Light {
  int kind = 1;
  vec3 vector{0, 0, 1}, colour{1, 1, 1}, ground{0, 0, 0};
  real falloff = 0;
};
// Lights with the ambient term, the colour transfer (see the locals above),
// and the frame of normal colouring: model scale and camera axes.
struct Lights {
  std::vector<Light> list;
  vec3 ambient{0.35, 0.35, 0.35};
  int transfer = 0;
  real r0 = 0, r1 = 0, neutral = 0.7071067811865476;
  vec3 shadow{0, 0, 0}, highlight{1, 1, 1};
  vec3 scale{1, 1, 1}, right{1, 0, 0}, up{0, 1, 0}, toward{0, 0, 1};
};
// Strengths of the ambient, diffuse and specular terms, the specular exponent
// and colour reflectance, an optional colour for the back side of faces, and
// the style with its parameters (see the locals above).
struct Material {
  real ambient = 1, diffuse = 1, specular = 0, shininess = 10, specular_colour = 1;
  bool has_back = false;
  vec3 back{0, 0, 0};
  real style = 0, steps = 0, bands = 0, breaks = 0;
  vec4 a{0, 0, 0, 0}, b{0, 0, 0, 0};
};

// The lit colour of base colour b at unit normal n, position p, seen along v
// (unit, toward the eye). A normal facing away from the eye is turned toward
// it, and the back colour replaces the base colour there when the material
// has one. A light value xs >= -1 replaces n . L (see the locals above).
static inline vec3 shade(vec3 n, vec3 p, vec3 v, vec3 b, const Material& m, const Lights& lights, real xs = -2,
                         real occlusion = 1) {
  if (dot(n, v) < 0) {
    n = -n;
    if (m.has_back) b = m.back;
  }
  const real ka = m.ambient, kd = m.diffuse, ks = m.specular, sh = m.shininess, sc = m.specular_colour;
  const real st = m.style, ss = m.steps, nb = m.bands, nk = m.breaks;
  const vec4 pa = m.a, pb = m.b;
  const real tm = lights.transfer, r0 = lights.r0, r1 = lights.r1, c0 = lights.neutral;
  const vec3 tsh = lights.shadow, thi = lights.highlight;
  const vec3 fs = lights.scale, fr = lights.right, fu = lights.up, ft = lights.toward;
  vec3 amb = lights.ambient, dif(0, 0, 0), spe(0, 0, 0), l0(0, 0, 1);
  real lsum = 0, gsum = 0, wsum = 0, wl = 0;
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
  return col * clamp(occlusion, 0.0, 1.0);
}

static inline real transfer(real linear, real brightness, bool multiply) {
  return multiply ? linear*brightness : 1-(1-linear)*brightness;
}

// The kernel as a GLSL ES 3.0 function `vec3 shade(vec3 n, vec3 p, vec3 v,
// vec3 b, vec4 material, vec4 material2, vec4 style, vec4 pa, vec4 pb)` over
// uniform light arrays of at most `max_lights` entries; material = (ka, kd,
// ks, shininess), material2 = (specular colour reflectance, back colour, or a
// negative red for none), style = (style, quantised specular, bands or normal
// frame, explicit breaks), pa and pb the style parameters, uTransfer =
// (transfer, r0, r1, neutral cosine).
static inline std::string shade_glsl(int max_lights) {
  std::string terms;
#define GGPLOT3D_TERM(id, name, ...) terms += "case " #id ": { " GGPLOT3D_STRINGIFY(__VA_ARGS__) " } break; ";
  GGPLOT3D_LIGHT_TERMS(GGPLOT3D_TERM)
#undef GGPLOT3D_TERM
  const std::string count = std::to_string(max_lights);
  return std::string("#define real float\n") +
    "uniform int uLightCount, uLightKind[" + count + "]; uniform vec3 uLightVector[" + count + "], uLightColour[" + count +
    "], uLightGround[" + count + "]; uniform float uLightFalloff[" + count + "]; uniform vec3 uAmbient;\n"
    "uniform vec4 uTransfer; uniform vec3 uToneShadow, uToneHighlight, uFrameScale, uFrameRight, uFrameUp, uFrameToward;\n"
    GGPLOT3D_XSTRINGIFY(GGPLOT3D_SHADE_FUNCTIONS()) "\n"
    "vec3 shade(vec3 n, vec3 p, vec3 v, vec3 b, vec4 material, vec4 material2, vec4 style, vec4 pa, vec4 pb) {\n"
    "  if (dot(n, v) < 0.0) { n = -n; if (material2.y >= 0.0) b = material2.yzw; }\n"
    "  real ka = material.x, kd = material.y, ks = material.z, sh = material.w, sc = material2.x;\n"
    "  real st = style.x, ss = style.y, nb = style.z, nk = style.w, xs = -2.0;\n"
    "  real tm = uTransfer.x, r0 = uTransfer.y, r1 = uTransfer.z, c0 = uTransfer.w;\n"
    "  vec3 tsh = uToneShadow, thi = uToneHighlight, fs = uFrameScale, fr = uFrameRight, fu = uFrameUp, ft = uFrameToward;\n"
    "  vec3 amb = uAmbient, dif = vec3(0.0), spe = vec3(0.0), l0 = vec3(0.0, 0.0, 1.0);\n"
    "  real lsum = 0.0, gsum = 0.0, wsum = 0.0, wl = 0.0;\n"
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
