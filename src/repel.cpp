#include <Rcpp.h>
#include <algorithm>
#include <chrono>
#include <cmath>
#include <cstdint>
#include <random>
#include <vector>

// Force-directed label placement on the page, in millimetres. Each label is an
// axis-aligned box around a centre. Overlapping boxes push each other apart
// across their shorter overlap, and every data point pushes boxes out of its
// disc. An overlapping box also gets a smaller push in a random direction,
// larger when the pushes on it mostly cancel, which frees a box held between
// opposite pushes. A moving box keeps a fraction of its
// previous velocity, which damps oscillation. A box that overlaps nothing is
// pulled towards its own point, a step at a time, until it touches the point's
// disc; a step that would make it overlap is not taken. The loop stops as
// soon as nothing overlaps, or after max_iter iterations or max_time seconds.
namespace {

struct Vec { double x, y; };

// Overlaps below a micrometre are contact, not overlap.
constexpr double contact = 1e-3;

class Uniform {
 public:
  explicit Uniform(std::uint32_t seed) : engine_(seed) {}
  double operator()() { return (engine_() + 0.5) / 4294967296.0; }
  Vec direction() {
    double angle = 6.283185307179586 * (*this)();
    return {std::cos(angle), std::sin(angle)};
  }
 private:
  std::mt19937 engine_;
};

struct Box {
  Vec c, h;
  double left() const { return c.x - h.x; }
  double right() const { return c.x + h.x; }
  double bottom() const { return c.y - h.y; }
  double top() const { return c.y + h.y; }
};

bool boxes_overlap(const Box& a, const Box& b, double* ox, double* oy) {
  *ox = a.h.x + b.h.x - std::fabs(a.c.x - b.c.x);
  *oy = a.h.y + b.h.y - std::fabs(a.c.y - b.c.y);
  return *ox > contact && *oy > contact;
}

// Penetration of a disc into a box, and the unit direction that moves the box
// out of it; zero when they are apart. A centre inside the box leaves through
// the nearest edge.
double disc_overlap(const Box& b, Vec p, double r, Vec* out, Uniform& rng) {
  Vec q{std::clamp(p.x, b.left(), b.right()), std::clamp(p.y, b.bottom(), b.top())};
  double dx = q.x - p.x, dy = q.y - p.y;
  double d = std::sqrt(dx * dx + dy * dy);
  if (d > 0) {
    if (d >= r - contact) return 0;
    *out = {dx / d, dy / d};
    return r - d;
  }
  double ex = b.h.x - std::fabs(p.x - b.c.x), ey = b.h.y - std::fabs(p.y - b.c.y);
  bool across = ex < ey;
  double side = across ? b.c.x - p.x : b.c.y - p.y;
  double sign = side > 0 ? 1.0 : side < 0 ? -1.0 : (rng() < 0.5 ? -1.0 : 1.0);
  *out = across ? Vec{sign, 0} : Vec{0, sign};
  return std::min(ex, ey) + r;
}

void clamp_box(Box& b, const double* bounds) {
  for (int axis = 0; axis < 2; ++axis) {
    double lo = bounds[2 * axis], hi = bounds[2 * axis + 1];
    double& c = axis == 0 ? b.c.x : b.c.y;
    double h = axis == 0 ? b.h.x : b.h.y;
    c = hi - lo < 2 * h ? 0.5 * (lo + hi) : std::clamp(c, lo + h, hi - h);
  }
}

}  // namespace

// anchors: m x 2 data points; radius: m disc radii; owner: n one-based anchor
// rows of the labels; start: n x 2 initial centres; half: n x 2 half sizes;
// bounds: xmin, xmax, ymin, ymax; direction 0 both, 1 x, 2 y.
// [[Rcpp::export(rng = false)]]
Rcpp::List repel_labels_cpp(Rcpp::NumericMatrix anchors, Rcpp::NumericVector radius,
                            Rcpp::IntegerVector owner, Rcpp::NumericMatrix start,
                            Rcpp::NumericMatrix half, Rcpp::NumericVector bounds,
                            double force, double force_pull, int max_iter, double max_time,
                            int max_overlaps, int direction, double seed, bool jitter) {
  const int m = anchors.nrow(), n = start.nrow();
  if (radius.size() != m || owner.size() != n || half.nrow() != n || bounds.size() != 4)
    Rcpp::stop("Label repulsion inputs have inconsistent sizes.");
  Uniform rng(static_cast<std::uint32_t>(std::fmod(std::fabs(seed), 4294967296.0)));
  std::vector<Vec> points(m);
  for (int j = 0; j < m; ++j) points[j] = {anchors(j, 0), anchors(j, 1)};
  std::vector<Box> boxes(n);
  std::vector<int> own(n);
  for (int i = 0; i < n; ++i) {
    own[i] = owner[i] - 1;
    if (own[i] < 0 || own[i] >= m) Rcpp::stop("Label owners must index the anchors.");
    boxes[i] = {{start(i, 0), start(i, 1)}, {half(i, 0), half(i, 1)}};
    if (jitter) {
      Vec u = rng.direction();
      double scale = 1e-3 * (boxes[i].h.x + boxes[i].h.y);
      boxes[i].c.x += u.x * scale; boxes[i].c.y += u.y * scale;
    }
    clamp_box(boxes[i], bounds.begin());
  }

  // A label that overlaps more than max_overlaps labels kept before it and
  // other data points at its start is not drawn.
  std::vector<char> hidden(n, 0);
  double ox, oy;
  Vec u;
  for (int i = 0; i < n; ++i) {
    int count = 0;
    for (int k = 0; k < i; ++k) if (!hidden[k] && boxes_overlap(boxes[i], boxes[k], &ox, &oy)) ++count;
    for (int j = 0; j < m; ++j) if (j != own[i] && disc_overlap(boxes[i], points[j], radius[j], &u, rng) > 0) ++count;
    hidden[i] = count > max_overlaps;
  }

  const double decay = 0.6, step = 0.5;
  std::vector<Vec> velocity(n, {0, 0}), push(n);
  std::vector<double> depth(n);
  auto move = [&](int i, Vec& v) {
    if (direction == 2) v.x = 0;
    if (direction == 1) v.y = 0;
    Vec before = boxes[i].c;
    boxes[i].c.x += v.x; boxes[i].c.y += v.y;
    clamp_box(boxes[i], bounds.begin());
    v = {boxes[i].c.x - before.x, boxes[i].c.y - before.y};
  };
  auto collides = [&](int i) {
    for (int k = 0; k < n; ++k) if (k != i && !hidden[k] && boxes_overlap(boxes[i], boxes[k], &ox, &oy)) return true;
    for (int j = 0; j < m; ++j) if (disc_overlap(boxes[i], points[j], radius[j], &u, rng) > 0) return true;
    return false;
  };
  auto clock = std::chrono::steady_clock::now();
  int iteration = 0;
  bool overlapping = true;
  for (; iteration < max_iter; ++iteration) {
    if (std::isfinite(max_time) && (iteration & 63) == 0 &&
        std::chrono::duration<double>(std::chrono::steady_clock::now() - clock).count() > max_time) break;
    overlapping = false;
    std::fill(push.begin(), push.end(), Vec{0, 0});
    std::fill(depth.begin(), depth.end(), 0.0);
    for (int i = 0; i < n; ++i) {
      if (hidden[i]) continue;
      for (int k = i + 1; k < n; ++k) {
        if (hidden[k] || !boxes_overlap(boxes[i], boxes[k], &ox, &oy)) continue;
        bool across = ox < oy;
        double side = across ? boxes[i].c.x - boxes[k].c.x : boxes[i].c.y - boxes[k].c.y;
        double sign = side > 0 ? 1.0 : side < 0 ? -1.0 : (rng() < 0.5 ? -1.0 : 1.0);
        Vec d = across ? Vec{sign, 0} : Vec{0, sign};
        double overlap = std::min(ox, oy);
        double magnitude = 0.5 * force * overlap;
        push[i].x += d.x * magnitude; push[i].y += d.y * magnitude;
        push[k].x -= d.x * magnitude; push[k].y -= d.y * magnitude;
        depth[i] += magnitude; depth[k] += magnitude;
      }
      for (int j = 0; j < m; ++j) {
        double overlap = disc_overlap(boxes[i], points[j], radius[j], &u, rng);
        if (overlap <= 0) continue;
        push[i].x += u.x * force * overlap; push[i].y += u.y * force * overlap;
        depth[i] += force * overlap;
      }
    }
    for (int i = 0; i < n; ++i) overlapping = overlapping || depth[i] > 0;
    if (!overlapping) break;
    for (int i = 0; i < n; ++i) {
      if (hidden[i] || depth[i] <= 0) continue;
      Vec kick = rng.direction();
      double net = std::sqrt(push[i].x * push[i].x + push[i].y * push[i].y);
      double shake = net < 0.5 * depth[i] ? 0.05 * force * (boxes[i].h.x + boxes[i].h.y) : 0.1 * depth[i];
      Vec& v = velocity[i];
      v.x = decay * v.x + step * (push[i].x + kick.x * shake);
      v.y = decay * v.y + step * (push[i].y + kick.y * shake);
      move(i, v);
    }
    for (int i = 0; i < n; ++i) {
      if (hidden[i] || depth[i] > 0) continue;
      velocity[i] = {0, 0};
      Vec a = points[own[i]];
      Vec q{std::clamp(a.x, boxes[i].left(), boxes[i].right()), std::clamp(a.y, boxes[i].bottom(), boxes[i].top())};
      double gap = std::sqrt((q.x - a.x) * (q.x - a.x) + (q.y - a.y) * (q.y - a.y)) - radius[own[i]];
      Vec d{a.x - boxes[i].c.x, a.y - boxes[i].c.y};
      double norm = std::sqrt(d.x * d.x + d.y * d.y);
      if (gap <= contact || norm <= 1e-12) continue;
      double pull = std::min(0.1 * force_pull, 0.5) * gap / norm;
      Box before = boxes[i];
      Vec step_pull{d.x * pull, d.y * pull};
      move(i, step_pull);
      if (collides(i)) boxes[i] = before;
    }
  }

  Rcpp::NumericVector x(n), y(n);
  Rcpp::LogicalVector excluded(n);
  for (int i = 0; i < n; ++i) { x[i] = boxes[i].c.x; y[i] = boxes[i].c.y; excluded[i] = hidden[i] != 0; }
  return Rcpp::List::create(Rcpp::Named("x") = x, Rcpp::Named("y") = y,
                            Rcpp::Named("hidden") = excluded, Rcpp::Named("iterations") = iteration,
                            Rcpp::Named("overlapping") = overlapping);
}
