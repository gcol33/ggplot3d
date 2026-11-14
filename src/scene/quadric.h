#pragma once
#include "clipping.h"

// Analytic quadrics (Sigg, Weyrich, Botsch and Gross 2006): a unit sphere, or a
// unit cylinder or cone along local z in [0, 1], placed by a homogeneous map.
// Every computation works on the implicit form F(X) = X' Q X of the local
// homogeneous point X, so an affine or projective camera pre composes onto the
// placement exactly: rays, outlines and proxies are pulled back through one
// 4 x 4 matrix.
namespace ggplot3d { namespace engine {
using Mat4 = std::array<double, 16>;

static inline Point4 transform_point(const Mat4& m, const Point4& x) {
  Point4 out;
  for (int r = 0; r < 4; ++r) out[r] = m[r]*x[0] + m[4+r]*x[1] + m[8+r]*x[2] + m[12+r]*x[3];
  return out;
}
// m' x: carries a covector (plane, gradient) of the image space back to the source.
static inline Point4 transpose_point(const Mat4& m, const Point4& x) {
  Point4 out;
  for (int c = 0; c < 4; ++c) out[c] = m[c*4]*x[0] + m[c*4+1]*x[1] + m[c*4+2]*x[2] + m[c*4+3]*x[3];
  return out;
}
static inline Mat4 multiply(const Mat4& a, const Mat4& b) {
  Mat4 out{};
  for (int c = 0; c < 4; ++c) for (int r = 0; r < 4; ++r)
    for (int k = 0; k < 4; ++k) out[c*4+r] += a[k*4+r]*b[c*4+k];
  return out;
}
// Inverse by cofactors; the caller guarantees an invertible matrix.
static inline Mat4 inverse(const Mat4& m) {
  Mat4 v;
  v[0] = m[5]*m[10]*m[15]-m[5]*m[11]*m[14]-m[9]*m[6]*m[15]+m[9]*m[7]*m[14]+m[13]*m[6]*m[11]-m[13]*m[7]*m[10];
  v[4] = -m[4]*m[10]*m[15]+m[4]*m[11]*m[14]+m[8]*m[6]*m[15]-m[8]*m[7]*m[14]-m[12]*m[6]*m[11]+m[12]*m[7]*m[10];
  v[8] = m[4]*m[9]*m[15]-m[4]*m[11]*m[13]-m[8]*m[5]*m[15]+m[8]*m[7]*m[13]+m[12]*m[5]*m[11]-m[12]*m[7]*m[9];
  v[12] = -m[4]*m[9]*m[14]+m[4]*m[10]*m[13]+m[8]*m[5]*m[14]-m[8]*m[6]*m[13]-m[12]*m[5]*m[10]+m[12]*m[6]*m[9];
  v[1] = -m[1]*m[10]*m[15]+m[1]*m[11]*m[14]+m[9]*m[2]*m[15]-m[9]*m[3]*m[14]-m[13]*m[2]*m[11]+m[13]*m[3]*m[10];
  v[5] = m[0]*m[10]*m[15]-m[0]*m[11]*m[14]-m[8]*m[2]*m[15]+m[8]*m[3]*m[14]+m[12]*m[2]*m[11]-m[12]*m[3]*m[10];
  v[9] = -m[0]*m[9]*m[15]+m[0]*m[11]*m[13]+m[8]*m[1]*m[15]-m[8]*m[3]*m[13]-m[12]*m[1]*m[11]+m[12]*m[3]*m[9];
  v[13] = m[0]*m[9]*m[14]-m[0]*m[10]*m[13]-m[8]*m[1]*m[14]+m[8]*m[2]*m[13]+m[12]*m[1]*m[10]-m[12]*m[2]*m[9];
  v[2] = m[1]*m[6]*m[15]-m[1]*m[7]*m[14]-m[5]*m[2]*m[15]+m[5]*m[3]*m[14]+m[13]*m[2]*m[7]-m[13]*m[3]*m[6];
  v[6] = -m[0]*m[6]*m[15]+m[0]*m[7]*m[14]+m[4]*m[2]*m[15]-m[4]*m[3]*m[14]-m[12]*m[2]*m[7]+m[12]*m[3]*m[6];
  v[10] = m[0]*m[5]*m[15]-m[0]*m[7]*m[13]-m[4]*m[1]*m[15]+m[4]*m[3]*m[13]+m[12]*m[1]*m[7]-m[12]*m[3]*m[5];
  v[14] = -m[0]*m[5]*m[14]+m[0]*m[6]*m[13]+m[4]*m[1]*m[14]-m[4]*m[2]*m[13]-m[12]*m[1]*m[6]+m[12]*m[2]*m[5];
  v[3] = -m[1]*m[6]*m[11]+m[1]*m[7]*m[10]+m[5]*m[2]*m[11]-m[5]*m[3]*m[10]-m[9]*m[2]*m[7]+m[9]*m[3]*m[6];
  v[7] = m[0]*m[6]*m[11]-m[0]*m[7]*m[10]-m[4]*m[2]*m[11]+m[4]*m[3]*m[10]+m[8]*m[2]*m[7]-m[8]*m[3]*m[6];
  v[11] = -m[0]*m[5]*m[11]+m[0]*m[7]*m[9]+m[4]*m[1]*m[11]-m[4]*m[3]*m[9]-m[8]*m[1]*m[7]+m[8]*m[3]*m[5];
  v[15] = m[0]*m[5]*m[10]-m[0]*m[6]*m[9]-m[4]*m[1]*m[10]+m[4]*m[2]*m[9]+m[8]*m[1]*m[6]-m[8]*m[2]*m[5];
  const double determinant = m[0]*v[0] + m[1]*v[4] + m[2]*v[8] + m[3]*v[12];
  require(determinant != 0 && std::isfinite(determinant), "quadric placement is not invertible after camera pre");
  for (double& x : v) x /= determinant;
  return v;
}

// Surface parts: the curved surface, and the end disks at local z = 0 and 1.
enum QuadricPart : int { quadric_surface = 0, quadric_cap_0 = 1, quadric_cap_1 = 2 };

struct QuadricShape {
  QuadricKind kind;
  // Radius at local z = 1 relative to z = 0; 1 for spheres and cylinders.
  double ratio = 1;
  bool cap[2] = {false, false};
  Mat4 to_box, to_local;
};

static inline QuadricShape quadric_shape(const Quadrics& q, Index row, const Mat4& pre) {
  QuadricShape s;
  s.kind = static_cast<QuadricKind>(q.kind[row]);
  s.ratio = s.kind == QuadricKind::cone ? q.radius_ratio[row] : 1;
  if (s.kind != QuadricKind::sphere) {
    s.cap[0] = q.cap_0[row] == 1;
    s.cap[1] = q.cap_1[row] == 1 && s.ratio > 0;
  }
  s.to_box = multiply(pre, q.transform[row]);
  s.to_local = inverse(s.to_box);
  return s;
}

// An end without a disk exposes the inside of the surface.
static inline bool quadric_open(const QuadricShape& s) {
  return s.kind != QuadricKind::sphere && (!s.cap[0] || (!s.cap[1] && s.ratio > 0));
}

// F(X) for the local homogeneous point X: negative inside the infinite
// surface. A cone's radius 1 + (ratio - 1) z is homogenised to w + c z.
static inline double quadric_form(const QuadricShape& s, const Point4& a, const Point4& b) {
  if (s.kind == QuadricKind::sphere) return a[0]*b[0] + a[1]*b[1] + a[2]*b[2] - a[3]*b[3];
  const double c = s.ratio - 1;
  return a[0]*b[0] + a[1]*b[1] - (a[3] + c*a[2])*(b[3] + c*b[2]);
}
// Q X, half the gradient of F.
static inline Point4 quadric_gradient(const QuadricShape& s, const Point4& x) {
  if (s.kind == QuadricKind::sphere) return {x[0], x[1], x[2], -x[3]};
  const double c = s.ratio - 1, r = x[3] + c*x[2];
  return {x[0], x[1], -c*r, -r};
}
static inline bool quadric_inside(const QuadricShape& s, const Point4& x) {
  if (!(std::abs(x[3]) > 0)) return false;
  if (quadric_form(s, x, x) > 0) return false;
  if (s.kind == QuadricKind::sphere) return true;
  const double z = x[2]/x[3];
  return z >= 0 && z <= 1;
}

// Corners of the local bounding box, homogeneous.
static inline std::array<Point4, 8> quadric_corners(const QuadricShape& s) {
  const double r = std::max(1.0, s.ratio);
  const double z0 = s.kind == QuadricKind::sphere ? -1 : 0;
  std::array<Point4, 8> out;
  for (int i = 0; i < 8; ++i)
    out[i] = {(i & 1) ? r : -r, (i & 2) ? r : -r, (i & 4) ? 1 : z0, 1};
  return out;
}
static inline Point4 quadric_centre(const QuadricShape& s) {
  return {0, 0, s.kind == QuadricKind::sphere ? 0 : 0.5, 1};
}

static inline Vertex box_vertex(const Point4& x) { return {{x[0], x[1], x[2]}, {}, x[3]}; }

// Box and depth planes only: the screen planes do not bound a closed body.
static inline std::vector<Plane> quadric_clipping_planes(const Projection& p) {
  auto planes = clipping_planes(p);
  if (p.camera.clip) planes.resize(planes.size()-4);
  return planes;
}

static inline Point4 quadric_eye(const Projection& p) {
  const auto& c = p.resolved;
  if (p.camera.perspective)
    return {c.eye[0]/p.scale[0], c.eye[1]/p.scale[1], c.eye[2]/p.scale[2], 1};
  return {c.ray[0]/p.scale[0], c.ray[1]/p.scale[1], c.ray[2]/p.scale[2], 0};
}

static inline bool quadric_contains_eye(const QuadricShape& s, const Projection& p) {
  return p.camera.perspective && quadric_inside(s, transform_point(s.to_local, quadric_eye(p)));
}

// The far side of a quadric is drawn where it can show: through translucency,
// an open end, a cut by the clipping planes, or from an eye inside it.
static inline bool quadric_shows_back(const QuadricShape& s, const Projection& p, double opacity) {
  if (opacity < 1 || quadric_open(s) || quadric_contains_eye(s, p)) return true;
  const auto planes = quadric_clipping_planes(p);
  for (const auto& corner : quadric_corners(s)) {
    Vertex v = box_vertex(transform_point(s.to_box, corner));
    for (const auto& plane : planes) if (signed_distance(plane, v) < 0) return true;
  }
  return false;
}

// Sight ray of a normalised device point: the box point at camera depth t is
// origin + t * direction.
struct Ray { Vec3 origin, direction; };
struct RayFrame {
  const Projection* p;
  std::array<double, 9> inverse;
  explicit RayFrame(const Projection& projection) : p(&projection) {
    const auto& c = projection.resolved;
    const Vec3 a = c.screen_right, b = c.screen_up, f = c.forward;
    const double d = dot(a, cross(b, f));
    require(d != 0 && std::isfinite(d), "singular camera frame");
    const Vec3 x = cross(b, f), y = cross(f, a), z = cross(a, b);
    for (int k = 0; k < 3; ++k) {inverse[k*3] = x[k]/d; inverse[k*3+1] = y[k]/d; inverse[k*3+2] = z[k]/d;}
  }
  Vec3 model_offset(const Vec3& camera) const {
    Vec3 out{};
    for (int k = 0; k < 3; ++k) out[k] = inverse[k*3]*camera[0] + inverse[k*3+1]*camera[1] + inverse[k*3+2]*camera[2];
    return out;
  }
  Ray ray(double u, double v) const {
    const double x = (2*u-1)/p->sx, y = (2*v-1)/p->sy;
    const bool perspective = p->camera.perspective;
    const Vec3 start = model_offset({perspective ? 0 : x, perspective ? 0 : y, -p->resolved.distance});
    const Vec3 step = model_offset({perspective ? x : 0, perspective ? y : 0, 1});
    Ray r;
    for (int k = 0; k < 3; ++k) {
      r.origin[k] = (p->target[k] + start[k])/p->scale[k];
      r.direction[k] = step[k]/p->scale[k];
    }
    return r;
  }
};

struct QuadricHit { double t = R_NaN; int part = quadric_surface; bool valid = false; };
struct QuadricInterval { QuadricHit enter, exit; };
enum class QuadricCast { solid, surface, cap_0_plane, cap_1_plane };

static inline void quadric_roots(double a, double b, double c, std::vector<double>& out) {
  // a t^2 + 2 b t + c = 0
  const double scale = std::max({std::abs(a), std::abs(b), std::abs(c)});
  if (!(scale > 0)) return;
  if (std::abs(a) <= 1e-14*scale) {
    if (b != 0) out.push_back(-c/(2*b));
    return;
  }
  const double disc = b*b - a*c;
  if (disc < 0) return;
  const double q = -(b + (b >= 0 ? 1 : -1)*std::sqrt(disc));
  out.push_back(q/a);
  if (q != 0) out.push_back(c/q);
}

// Where the sight ray enters and leaves the quadric: the solid (surface and
// end planes), the infinite surface alone, or one end plane. Boundary
// candidates are the roots of each constraint; the inside of each span
// between them is decided at its midpoint, so cones whose sight lines cross
// both nappes and projective pre maps need no special cases.
static inline QuadricInterval quadric_cast(const QuadricShape& s, const Ray& ray, QuadricCast mode) {
  const Point4 a = transform_point(s.to_local, {ray.origin[0], ray.origin[1], ray.origin[2], 1});
  const Point4 b = transform_point(s.to_local, {ray.direction[0], ray.direction[1], ray.direction[2], 0});
  QuadricInterval out;
  auto at = [&](double t) { return Point4{a[0]+t*b[0], a[1]+t*b[1], a[2]+t*b[2], a[3]+t*b[3]}; };
  if (mode == QuadricCast::cap_0_plane || mode == QuadricCast::cap_1_plane) {
    const bool top = mode == QuadricCast::cap_1_plane;
    const double p0 = top ? a[3]-a[2] : a[2], p1 = top ? b[3]-b[2] : b[2];
    if (p1 == 0) return out;
    out.enter = {-p0/p1, top ? quadric_cap_1 : quadric_cap_0, true};
    out.exit = out.enter;
    return out;
  }
  struct Candidate { double t; int part; };
  std::vector<Candidate> candidates;
  std::vector<double> roots;
  quadric_roots(quadric_form(s, b, b), quadric_form(s, a, b), quadric_form(s, a, a), roots);
  for (double t : roots) if (std::isfinite(t)) candidates.push_back({t, quadric_surface});
  const bool slab = mode == QuadricCast::solid && s.kind != QuadricKind::sphere;
  if (slab) {
    if (b[2] != 0) candidates.push_back({-a[2]/b[2], quadric_cap_0});
    if (b[3] != b[2]) candidates.push_back({-(a[3]-a[2])/(b[3]-b[2]), quadric_cap_1});
  }
  if (b[3] != 0) candidates.push_back({-a[3]/b[3], -1});
  std::sort(candidates.begin(), candidates.end(), [](const Candidate& x, const Candidate& y) {
    return x.t < y.t || (x.t == y.t && x.part < y.part);
  });
  auto inside = [&](double t) {
    const Point4 x = at(t);
    if (!(std::abs(x[3]) > 0)) return false;
    if (quadric_form(s, x, x) > 0) return false;
    if (!slab) return true;
    const double z = x[2]/x[3];
    return z >= 0 && z <= 1;
  };
  const std::size_t n = candidates.size();
  bool first = true;
  for (std::size_t i = 0; i <= n; ++i) {
    double lo = i == 0 ? R_NegInf : candidates[i-1].t, hi = i == n ? R_PosInf : candidates[i].t;
    if (lo == hi) continue;
    double mid = i == 0 ? hi - 1 - std::abs(hi) : (i == n ? lo + 1 + std::abs(lo) : 0.5*(lo+hi));
    if (n == 0) mid = 0;
    if (!inside(mid)) continue;
    if (first) {
      out.enter = std::isfinite(lo) ? QuadricHit{lo, candidates[i-1].part, candidates[i-1].part >= 0} : QuadricHit{};
      first = false;
    }
    out.exit = std::isfinite(hi) ? QuadricHit{hi, candidates[i].part, candidates[i].part >= 0} : QuadricHit{};
  }
  return out;
}

static inline Vec3 ray_point(const Ray& ray, double t) {
  return {ray.origin[0]+t*ray.direction[0], ray.origin[1]+t*ray.direction[1], ray.origin[2]+t*ray.direction[2]};
}

// Outward normal covector of a part at a box point: the box-space gradient of
// F (curved surface) or of the end-plane coordinate, unnormalised.
static inline Vec3 quadric_covector(const QuadricShape& s, const Vec3& box, int part) {
  const Point4 x = transform_point(s.to_local, {box[0], box[1], box[2], 1});
  Point4 g;
  if (part == quadric_surface) g = quadric_gradient(s, x);
  else {
    // Gradient of -z/w (z = 0 end) or z/w (z = 1 end): outward across the end plane.
    const double sign = part == quadric_cap_0 ? -1 : 1;
    g = {0, 0, sign/x[3], -sign*x[2]/(x[3]*x[3])};
  }
  const Point4 covector = transpose_point(s.to_local, g);
  return {covector[0], covector[1], covector[2]};
}
static inline Vec3 unit(Vec3 n) {
  const double length = std::sqrt(dot(n, n));
  if (length > 0) for (double& v : n) v /= length;
  return n;
}
// Outward unit normal in aspect-scaled model space.
static inline Vec3 quadric_normal(const QuadricShape& s, const Projection& p, const Vec3& box, int part) {
  const Vec3 g = quadric_covector(s, box, part);
  return unit({g[0]/p.scale[0], g[1]/p.scale[1], g[2]/p.scale[2]});
}

// Part of the surface a stand-in facet covers, from its outward model normal
// and a point on it: end facets lie in the planes local z = 0 and 1.
static inline int quadric_facet_part(const QuadricShape& s, const Projection& p, const Vec3& normal, const Vec3& box) {
  if (s.kind == QuadricKind::sphere) return quadric_surface;
  const Vec3 g{normal[0]*p.scale[0], normal[1]*p.scale[1], normal[2]*p.scale[2]};
  const Point4 plane = transpose_point(s.to_box, {g[0], g[1], g[2], -dot(g, box)});
  const Vec3 local = unit({plane[0], plane[1], plane[2]});
  if (std::abs(local[2]) < 1 - 1e-6) return quadric_surface;
  const Point4 x = transform_point(s.to_local, {box[0], box[1], box[2], 1});
  return x[2]/x[3] < 0.5 ? quadric_cap_0 : quadric_cap_1;
}

// Box-plane, near and far clipping of a hit, as for every other primitive.
static inline bool quadric_hit_kept(const Projection& p, const Vec3& box, double t) {
  if (!(t >= p.resolved.near && t <= p.resolved.far)) return false;
  if (p.camera.clip_data)
    for (double v : box) if (v < 0 || v > 1) return false;
  return true;
}

// The visible surface hit of a sight ray on the near (front) or far (back)
// side: where the ray enters or leaves the solid through a drawn part.
static inline QuadricHit quadric_visible_hit(const QuadricShape& s, const Projection& p, const Ray& ray, bool back) {
  auto span = quadric_cast(s, ray, QuadricCast::solid);
  QuadricHit hit = back ? span.exit : span.enter;
  if (!hit.valid) return QuadricHit{};
  if (hit.part != quadric_surface && !s.cap[hit.part-1]) return QuadricHit{};
  if (!quadric_hit_kept(p, ray_point(ray, hit.t), hit.t)) return QuadricHit{};
  return hit;
}

// Screen position in millimetres of a box point, or false where it is not in
// front of the eye.
static inline bool quadric_screen_mm(const Projection& p, const Point4& box, std::array<double, 2>& out) {
  if (!(box[3] > 0)) return false;
  const Vec3 c = p.camera_point({box[0]/box[3], box[1]/box[3], box[2]/box[3]});
  if (p.camera.perspective && !(c[2] > p.resolved.near)) return false;
  const auto xy = p.npc(c);
  out = {xy[0]*p.viewport.width_mm, xy[1]*p.viewport.height_mm};
  return std::isfinite(out[0]) && std::isfinite(out[1]);
}

// Whether the whole bounding box lies in front of the eye at positive w, so
// that its image is bounded and the outline is a closed convex curve.
static inline bool quadric_bounded(const QuadricShape& s, const Projection& p) {
  std::array<double, 2> xy;
  for (const auto& corner : quadric_corners(s))
    if (!quadric_screen_mm(p, transform_point(s.to_box, corner), xy)) return false;
  return true;
}

static inline std::vector<std::array<double, 2>> convex_hull(std::vector<std::array<double, 2>> points) {
  std::sort(points.begin(), points.end());
  points.erase(std::unique(points.begin(), points.end()), points.end());
  if (points.size() < 3) return points;
  std::vector<std::array<double, 2>> hull(2*points.size());
  std::size_t k = 0;
  auto turn = [](const std::array<double, 2>& o, const std::array<double, 2>& a, const std::array<double, 2>& b) {
    return (a[0]-o[0])*(b[1]-o[1]) - (a[1]-o[1])*(b[0]-o[0]);
  };
  for (std::size_t i = 0; i < points.size(); ++i) {
    while (k >= 2 && turn(hull[k-2], hull[k-1], points[i]) <= 0) --k;
    hull[k++] = points[i];
  }
  for (std::size_t i = points.size()-1, t = k+1; i > 0; --i) {
    while (k >= t && turn(hull[k-2], hull[k-1], points[i-1]) <= 0) --k;
    hull[k++] = points[i-1];
  }
  hull.resize(k-1);
  return hull;
}

// Local circles whose images bound the outline: the contour generator of a
// sphere (its intersection with the polar plane of the eye, Eberly 2003), or
// the two end circles of a cylinder or cone, whose convex hull is the image
// of the solid. Each circle is centre + radius (cos a u + sin a v).
struct LocalCircle { Vec3 centre, u, v; double radius; };
static inline std::vector<LocalCircle> quadric_outline_circles(const QuadricShape& s, const Projection& p) {
  std::vector<LocalCircle> out;
  if (s.kind == QuadricKind::sphere) {
    const Point4 e = transform_point(s.to_local, quadric_eye(p));
    const Vec3 n{e[0], e[1], e[2]};
    const double length2 = dot(n, n);
    if (!(length2 > e[3]*e[3])) return out;
    const double length = std::sqrt(length2);
    Vec3 axis{n[0]/length, n[1]/length, n[2]/length};
    Vec3 helper = std::abs(axis[0]) < 0.9 ? Vec3{1, 0, 0} : Vec3{0, 1, 0};
    Vec3 u = cross(axis, helper);
    const double lu = std::sqrt(dot(u, u));
    for (double& x : u) x /= lu;
    const Vec3 v = cross(axis, u);
    const double offset = e[3]/length;
    out.push_back({{axis[0]*offset, axis[1]*offset, axis[2]*offset}, u, v, std::sqrt(1-offset*offset)});
  } else {
    out.push_back({{0, 0, 0}, {1, 0, 0}, {0, 1, 0}, 1});
    out.push_back({{0, 0, 1}, {1, 0, 0}, {0, 1, 0}, s.ratio});
  }
  return out;
}

// Exact outline in millimetres: images of the outline circles sampled so the
// chord error stays below `tolerance_mm`, then their convex hull. Empty where
// the image is unbounded.
static inline std::vector<std::array<double, 2>> quadric_outline(const QuadricShape& s, const Projection& p,
                                                                 double tolerance_mm) {
  std::vector<std::array<double, 2>> points;
  if (!quadric_bounded(s, p)) return points;
  for (const auto& circle : quadric_outline_circles(s, p)) {
    auto sample = [&](int n, std::vector<std::array<double, 2>>& out) {
      for (int i = 0; i < n; ++i) {
        const double a = 2*pi*i/n, ca = std::cos(a)*circle.radius, sa = std::sin(a)*circle.radius;
        Point4 x{circle.centre[0]+ca*circle.u[0]+sa*circle.v[0], circle.centre[1]+ca*circle.u[1]+sa*circle.v[1],
                 circle.centre[2]+ca*circle.u[2]+sa*circle.v[2], 1};
        std::array<double, 2> xy;
        if (quadric_screen_mm(p, transform_point(s.to_box, x), xy)) out.push_back(xy);
      }
    };
    std::vector<std::array<double, 2>> coarse;
    sample(16, coarse);
    double mx = 0, my = 0, radius = 0;
    for (const auto& q : coarse) {mx += q[0]/coarse.size(); my += q[1]/coarse.size();}
    for (const auto& q : coarse) radius = std::max(radius, std::hypot(q[0]-mx, q[1]-my));
    const int n = static_cast<int>(std::min(4096.0, std::max(16.0, std::ceil(pi*std::sqrt(radius/(2*tolerance_mm))))));
    sample(n, points);
  }
  return convex_hull(std::move(points));
}

// Facets of a polyhedral stand-in in local coordinates, wound
// counterclockwise seen from outside, with the part each facet covers.
// Circumscribed facets lie outside the surface, so the front facets of the
// stand-in cover the exact outline; inscribed vertices lie on the surface.
// Meridians sit half a step off the local axes and a sphere has an odd number
// of latitude bands, so planes through the centre along the axes cut facets
// instead of running along their edges.
struct QuadricFacet { std::vector<Point4> points; int part; };
static inline std::vector<QuadricFacet> quadric_facets(const QuadricShape& s, int segments, bool circumscribed) {
  std::vector<QuadricFacet> out;
  const int n = std::max(3, segments);
  if (s.kind == QuadricKind::sphere) {
    const int rings = std::max(3, (n/2) | 1);
    auto vertex = [&](int j, int i) {
      if (j == 0 || j == rings) return Point4{0, 0, j == 0 ? 1.0 : -1.0, 1};
      const double phi = pi*j/rings, theta = 2*pi*((i % n)+0.5)/n;
      return Point4{std::sin(phi)*std::cos(theta), std::sin(phi)*std::sin(theta), std::cos(phi), 1};
    };
    for (int j = 0; j < rings; ++j) for (int i = 0; i < n; ++i) {
      QuadricFacet f{{vertex(j, i), vertex(j+1, i)}, quadric_surface};
      if (j+1 < rings) f.points.push_back(vertex(j+1, i+1));
      if (j > 0) f.points.push_back(vertex(j, i+1));
      out.push_back(std::move(f));
    }
    if (circumscribed) {
      double nearest = 1;
      for (const auto& f : out) {
        Vec3 normal{0, 0, 0}, centroid{0, 0, 0};
        for (std::size_t k = 0; k < f.points.size(); ++k) {
          const auto& a = f.points[k]; const auto& b = f.points[(k+1)%f.points.size()];
          normal[0] += (a[1]-b[1])*(a[2]+b[2]); normal[1] += (a[2]-b[2])*(a[0]+b[0]);
          normal[2] += (a[0]-b[0])*(a[1]+b[1]);
          for (int c = 0; c < 3; ++c) centroid[c] += a[c]/f.points.size();
        }
        nearest = std::min(nearest, std::abs(dot(normal, centroid))/std::sqrt(dot(normal, normal)));
      }
      for (auto& f : out) for (auto& x : f.points) for (int c = 0; c < 3; ++c) x[c] /= nearest;
    }
    return out;
  }
  const double grow = circumscribed ? 1/std::cos(pi/n) : 1;
  auto ring = [&](int i, double z, double radius) {
    const double theta = 2*pi*((i % n)+0.5)/n;
    return Point4{grow*radius*std::cos(theta), grow*radius*std::sin(theta), z, 1};
  };
  for (int i = 0; i < n; ++i) {
    QuadricFacet f{{ring(i, 0, 1), ring(i+1, 0, 1)}, quadric_surface};
    if (s.ratio > 0) {f.points.push_back(ring(i+1, 1, s.ratio)); f.points.push_back(ring(i, 1, s.ratio));}
    else f.points.push_back({0, 0, 1, 1});
    out.push_back(std::move(f));
  }
  if (s.cap[0]) {
    QuadricFacet f{{}, quadric_cap_0};
    for (int i = n; i > 0; --i) f.points.push_back(ring(i, 0, 1));
    out.push_back(std::move(f));
  }
  if (s.cap[1]) {
    QuadricFacet f{{}, quadric_cap_1};
    for (int i = 0; i < n; ++i) f.points.push_back(ring(i, 1, s.ratio));
    out.push_back(std::move(f));
  }
  return out;
}

// Radial error of the circumscribed stand-in with `segments` points around,
// in local radius units.
static inline double quadric_facet_error(const QuadricShape& s, int segments) {
  const double c = std::cos(pi/segments);
  return s.kind == QuadricKind::sphere ? 1/(c*c) - 1 : (1/c - 1)*std::max(1.0, s.ratio);
}

static constexpr int quadric_segment_steps[] = {8, 12, 16, 24, 32, 48, 64, 96, 128};

// Fewest segments whose stand-in departs from the surface by less than half a
// device pixel on screen. A local unit is measured on screen at the centre,
// and enlarged by the ratio of the centre's depth to the nearest corner's
// under perspective.
static inline int quadric_segments(const QuadricShape& s, const Projection& p) {
  const int finest = quadric_segment_steps[sizeof(quadric_segment_steps)/sizeof(int)-1];
  const double tolerance = 12.7/p.viewport.dpi;
  std::array<double, 2> centre, xy;
  const Point4 c = quadric_centre(s);
  const Point4 box = transform_point(s.to_box, c);
  if (!quadric_screen_mm(p, box, centre) || !quadric_bounded(s, p)) return finest;
  double unit = 0;
  for (int k = 0; k < 3; ++k) for (double sign : {-1.0, 1.0}) {
    Point4 x = c; x[k] += sign;
    if (!quadric_screen_mm(p, transform_point(s.to_box, x), xy)) return finest;
    unit = std::max(unit, std::hypot(xy[0]-centre[0], xy[1]-centre[1]));
  }
  if (p.camera.perspective) {
    const double depth = p.camera_point({box[0]/box[3], box[1]/box[3], box[2]/box[3]})[2];
    double nearest = depth;
    for (const auto& corner : quadric_corners(s)) {
      const Point4 x = transform_point(s.to_box, corner);
      nearest = std::min(nearest, p.camera_point({x[0]/x[3], x[1]/x[3], x[2]/x[3]})[2]);
    }
    unit *= depth/nearest;
  }
  for (int n : quadric_segment_steps) if (quadric_facet_error(s, n)*unit <= tolerance) return n;
  return finest;
}

// Common primitive columns of the quadrics, so that their stand-in pieces
// travel through ordering and output like other primitives.
static inline PrimitiveTable quadric_primitives(const Quadrics& q) {
  PrimitiveTable t;
  const std::size_t n = q.primitive_id.size();
  t.primitive_id = q.primitive_id; t.layer_id = q.layer_id; t.source_row = q.source_row;
  t.style_id = q.style_id; t.partition = q.partition; t.opacity = q.opacity;
  t.offset.assign(n, 0); t.count.assign(n, 0); t.role.assign(n, static_cast<Index>(Role::fill));
  return t;
}
} }
