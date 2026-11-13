#ifndef GGPLOT3D_GEOMETRY_EARCUT_H
#define GGPLOT3D_GEOMETRY_EARCUT_H
#include <array>
#include <vector>
#include <algorithm>
#include <cmath>
#include <limits>

// Triangulation of polygons with holes by ear clipping, after the earcut
// algorithm of Mapbox (github.com/mapbox/earcut): holes are joined to their
// outer ring by bridges found with a leftward ray, then ears are clipped,
// with passes that remove collinear points, cure local self-intersections
// and split the remainder along a valid diagonal. Rings nest by the even-odd
// rule. Outer rings are traversed counterclockwise, so every triangle is
// counterclockwise.
namespace ggplot3d_geometry { namespace earcut {

struct Node { int i; double x, y; int prev = -1, next = -1; bool steiner = false; };

class Triangulator {
 public:
  std::vector<std::array<int, 3>> triangles;

  // Rings as lists of point indices; ring 0 is the outer ring, the rest holes.
  void run(const std::vector<double>& x, const std::vector<double>& y, const std::vector<std::vector<int>>& rings) {
    if (rings.empty()) return;
    int outer = linked_list(x, y, rings[0], true);
    if (outer < 0 || next(outer) == prev(outer)) return;
    if (rings.size() > 1) outer = eliminate_holes(x, y, rings, outer);
    linked(outer, 0);
  }

 private:
  std::vector<Node> nodes;

  int next(int p) const { return nodes[p].next; }
  int prev(int p) const { return nodes[p].prev; }
  double area(int p, int q, int r) const {
    const Node &a = nodes[p], &b = nodes[q], &c = nodes[r];
    return (b.y - a.y) * (c.x - b.x) - (b.x - a.x) * (c.y - b.y);
  }
  bool equals(int p, int q) const { return nodes[p].x == nodes[q].x && nodes[p].y == nodes[q].y; }

  int insert(int i, double x, double y, int last) {
    Node n; n.i = i; n.x = x; n.y = y;
    const int k = static_cast<int>(nodes.size());
    if (last < 0) { n.prev = n.next = k; }
    else { n.next = nodes[last].next; n.prev = last; nodes[nodes[last].next].prev = k; nodes[last].next = k; }
    nodes.push_back(n);
    return k;
  }
  void remove(int p) { nodes[next(p)].prev = prev(p); nodes[prev(p)].next = next(p); }

  int linked_list(const std::vector<double>& x, const std::vector<double>& y, const std::vector<int>& ring, bool clockwise) {
    double sum = 0;
    for (std::size_t k = 0, j = ring.size() - 1; k < ring.size(); j = k++)
      sum += (x[ring[j]] - x[ring[k]]) * (y[ring[k]] + y[ring[j]]);
    int last = -1;
    if (clockwise == (sum > 0)) for (int i : ring) last = insert(i, x[i], y[i], last);
    else for (auto it = ring.rbegin(); it != ring.rend(); ++it) last = insert(*it, x[*it], y[*it], last);
    if (last >= 0 && equals(last, next(last))) { const int n = next(last); remove(last); last = n; }
    return last;
  }

  int filter_points(int start, int end = -1) {
    if (start < 0) return start;
    if (end < 0) end = start;
    int p = start;
    bool again;
    do {
      again = false;
      if (!nodes[p].steiner && (equals(p, next(p)) || area(prev(p), p, next(p)) == 0)) {
        remove(p);
        p = end = prev(p);
        if (p == next(p)) break;
        again = true;
      } else {
        p = next(p);
      }
    } while (again || p != end);
    return end;
  }

  void linked(int ear, int pass) {
    if (ear < 0) return;
    int stop = ear;
    while (prev(ear) != next(ear)) {
      const int a = prev(ear), c = next(ear);
      if (is_ear(ear)) {
        triangles.push_back({nodes[a].i, nodes[ear].i, nodes[c].i});
        remove(ear);
        ear = next(c); stop = next(c);
        continue;
      }
      ear = c;
      if (ear == stop) {
        if (pass == 0) linked(filter_points(ear), 1);
        else if (pass == 1) linked(cure_local_intersections(filter_points(ear)), 2);
        else split(ear);
        break;
      }
    }
  }

  bool point_in_triangle(double ax, double ay, double bx, double by, double cx, double cy, double px, double py) const {
    return (cx - px) * (ay - py) >= (ax - px) * (cy - py) &&
           (ax - px) * (by - py) >= (bx - px) * (ay - py) &&
           (bx - px) * (cy - py) >= (cx - px) * (by - py);
  }

  bool is_ear(int ear) const {
    const int a = prev(ear), b = ear, c = next(ear);
    if (area(a, b, c) >= 0) return false;
    const Node &A = nodes[a], &B = nodes[b], &C = nodes[c];
    const double x0 = std::min({A.x, B.x, C.x}), y0 = std::min({A.y, B.y, C.y});
    const double x1 = std::max({A.x, B.x, C.x}), y1 = std::max({A.y, B.y, C.y});
    for (int p = next(c); p != a; p = next(p)) {
      const Node& P = nodes[p];
      if (P.x >= x0 && P.x <= x1 && P.y >= y0 && P.y <= y1 && !(P.x == A.x && P.y == A.y) &&
          point_in_triangle(A.x, A.y, B.x, B.y, C.x, C.y, P.x, P.y) && area(prev(p), p, next(p)) >= 0) return false;
    }
    return true;
  }

  static int sign(double v) { return v > 0 ? 1 : v < 0 ? -1 : 0; }
  bool on_segment(int p, int q, int r) const {
    const Node &P = nodes[p], &Q = nodes[q], &R = nodes[r];
    return Q.x <= std::max(P.x, R.x) && Q.x >= std::min(P.x, R.x) && Q.y <= std::max(P.y, R.y) && Q.y >= std::min(P.y, R.y);
  }
  bool intersects(int p1, int q1, int p2, int q2) const {
    const int o1 = sign(area(p1, q1, p2)), o2 = sign(area(p1, q1, q2));
    const int o3 = sign(area(p2, q2, p1)), o4 = sign(area(p2, q2, q1));
    if (o1 != o2 && o3 != o4) return true;
    if (o1 == 0 && on_segment(p1, p2, q1)) return true;
    if (o2 == 0 && on_segment(p1, q2, q1)) return true;
    if (o3 == 0 && on_segment(p2, p1, q2)) return true;
    if (o4 == 0 && on_segment(p2, q1, q2)) return true;
    return false;
  }
  bool locally_inside(int a, int b) const {
    return area(prev(a), a, next(a)) < 0 ? area(a, b, next(a)) >= 0 && area(a, prev(a), b) >= 0
                                         : area(a, b, prev(a)) < 0 || area(a, next(a), b) < 0;
  }
  bool intersects_polygon(int a, int b) const {
    int p = a;
    do {
      if (nodes[p].i != nodes[a].i && nodes[next(p)].i != nodes[a].i && nodes[p].i != nodes[b].i &&
          nodes[next(p)].i != nodes[b].i && intersects(p, next(p), a, b)) return true;
      p = next(p);
    } while (p != a);
    return false;
  }
  bool middle_inside(int a, int b) const {
    int p = a; bool inside = false;
    const double px = (nodes[a].x + nodes[b].x) / 2, py = (nodes[a].y + nodes[b].y) / 2;
    do {
      const Node &P = nodes[p], &N = nodes[next(p)];
      if (((P.y > py) != (N.y > py)) && N.y != P.y && (px < (N.x - P.x) * (py - P.y) / (N.y - P.y) + P.x)) inside = !inside;
      p = next(p);
    } while (p != a);
    return inside;
  }
  bool valid_diagonal(int a, int b) const {
    return nodes[next(a)].i != nodes[b].i && nodes[prev(a)].i != nodes[b].i && !intersects_polygon(a, b) &&
      ((locally_inside(a, b) && locally_inside(b, a) && middle_inside(a, b) &&
        (area(prev(a), a, prev(b)) != 0 || area(a, prev(b), b) != 0)) ||
       (equals(a, b) && area(prev(a), a, next(a)) > 0 && area(prev(b), b, next(b)) > 0));
  }

  int cure_local_intersections(int start) {
    int p = start;
    do {
      const int a = prev(p), b = next(next(p));
      if (!equals(a, b) && intersects(a, p, next(p), b) && locally_inside(a, b) && locally_inside(b, a)) {
        triangles.push_back({nodes[a].i, nodes[p].i, nodes[b].i});
        const int n = next(p);
        remove(p); remove(n);
        p = start = b;
      }
      p = next(p);
    } while (p != start);
    return filter_points(p);
  }

  // Joins a and b by a diagonal; returns the copy of b on the split-off ring.
  int split_polygon(int a, int b) {
    const int a2 = insert(nodes[a].i, nodes[a].x, nodes[a].y, -1);
    const int b2 = insert(nodes[b].i, nodes[b].x, nodes[b].y, -1);
    const int an = next(a), bp = prev(b);
    nodes[a].next = b; nodes[b].prev = a;
    nodes[a2].next = an; nodes[an].prev = a2;
    nodes[b2].next = a2; nodes[a2].prev = b2;
    nodes[bp].next = b2; nodes[b2].prev = bp;
    return b2;
  }

  void split(int start) {
    int a = start;
    do {
      int b = next(next(a));
      while (b != prev(a)) {
        if (nodes[a].i != nodes[b].i && valid_diagonal(a, b)) {
          int c = split_polygon(a, b);
          a = filter_points(a, next(a));
          c = filter_points(c, next(c));
          linked(a, 0);
          linked(c, 0);
          return;
        }
        b = next(b);
      }
      a = next(a);
    } while (a != start);
  }

  int leftmost(int start) const {
    int p = start, best = start;
    do {
      if (nodes[p].x < nodes[best].x || (nodes[p].x == nodes[best].x && nodes[p].y < nodes[best].y)) best = p;
      p = next(p);
    } while (p != start);
    return best;
  }

  bool sector_contains_sector(int m, int p) const {
    return area(prev(m), m, prev(p)) < 0 && area(next(p), m, next(m)) < 0;
  }

  int hole_bridge(int hole, int outer) const {
    int p = outer, m = -1;
    const double hx = nodes[hole].x, hy = nodes[hole].y;
    double qx = -std::numeric_limits<double>::infinity();
    do {
      const Node &P = nodes[p], &N = nodes[next(p)];
      if (hy <= P.y && hy >= N.y && N.y != P.y) {
        const double x = P.x + (hy - P.y) * (N.x - P.x) / (N.y - P.y);
        if (x <= hx && x > qx) {
          qx = x;
          m = P.x < N.x ? p : next(p);
          if (x == hx) return m;
        }
      }
      p = next(p);
    } while (p != outer);
    if (m < 0) return -1;
    const int stop = m;
    const double mx = nodes[m].x, my = nodes[m].y;
    double best = std::numeric_limits<double>::infinity();
    p = m;
    do {
      const Node& P = nodes[p];
      if (hx >= P.x && P.x >= mx && hx != P.x &&
          point_in_triangle(hy < my ? hx : qx, hy, mx, my, hy < my ? qx : hx, hy, P.x, P.y)) {
        const double tan = std::abs(hy - P.y) / (hx - P.x);
        if (locally_inside(p, hole) &&
            (tan < best || (tan == best && (P.x > nodes[m].x || (P.x == nodes[m].x && sector_contains_sector(m, p)))))) {
          m = p; best = tan;
        }
      }
      p = next(p);
    } while (p != stop);
    return m;
  }

  int eliminate_holes(const std::vector<double>& x, const std::vector<double>& y,
                      const std::vector<std::vector<int>>& rings, int outer) {
    std::vector<int> queue;
    for (std::size_t r = 1; r < rings.size(); ++r) {
      const int list = linked_list(x, y, rings[r], false);
      if (list < 0) continue;
      if (list == next(list)) nodes[list].steiner = true;
      queue.push_back(leftmost(list));
    }
    std::sort(queue.begin(), queue.end(), [this](int a, int b) { return nodes[a].x < nodes[b].x; });
    for (int hole : queue) {
      const int bridge = hole_bridge(hole, outer);
      if (bridge < 0) continue;
      const int reverse = split_polygon(bridge, hole);
      filter_points(reverse, next(reverse));
      outer = filter_points(bridge, next(bridge));
    }
    return outer;
  }
};

static inline double ring_signed_area(const std::vector<double>& x, const std::vector<double>& y, const std::vector<int>& ring) {
  double sum = 0;
  for (std::size_t k = 0, j = ring.size() - 1; k < ring.size(); j = k++) sum += x[ring[j]] * y[ring[k]] - x[ring[k]] * y[ring[j]];
  return sum / 2;
}

static inline bool inside_ring(double px, double py, const std::vector<double>& x, const std::vector<double>& y, const std::vector<int>& ring) {
  bool inside = false;
  for (std::size_t k = 0, j = ring.size() - 1; k < ring.size(); j = k++) {
    const double xk = x[ring[k]], yk = y[ring[k]], xj = x[ring[j]], yj = y[ring[j]];
    if ((yk > py) != (yj > py) && px < (xj - xk) * (py - yk) / (yj - yk) + xk) inside = !inside;
  }
  return inside;
}

// Triangles of the region inside an odd number of rings. A ring nested at
// even depth is an outer ring; its holes are the rings one level deeper that
// it contains.
static inline std::vector<std::array<int, 3>> triangulate_rings(const std::vector<double>& x, const std::vector<double>& y,
                                                                 const std::vector<std::vector<int>>& rings) {
  const std::size_t n = rings.size();
  std::vector<int> depth(n, 0);
  std::vector<std::vector<bool>> contains(n, std::vector<bool>(n, false));
  for (std::size_t i = 0; i < n; ++i) {
    if (rings[i].empty()) continue;
    const int probe = rings[i][0];
    for (std::size_t j = 0; j < n; ++j) {
      if (i == j || rings[j].size() < 3) continue;
      if (inside_ring(x[probe], y[probe], x, y, rings[j])) { contains[j][i] = true; ++depth[i]; }
    }
  }
  std::vector<std::array<int, 3>> out;
  for (std::size_t i = 0; i < n; ++i) {
    if (depth[i] % 2 != 0 || rings[i].size() < 3 || ring_signed_area(x, y, rings[i]) == 0) continue;
    std::vector<std::vector<int>> group{rings[i]};
    for (std::size_t j = 0; j < n; ++j) {
      if (contains[i][j] && depth[j] == depth[i] + 1 && rings[j].size() >= 3) group.push_back(rings[j]);
    }
    Triangulator t;
    t.run(x, y, group);
    out.insert(out.end(), t.triangles.begin(), t.triangles.end());
  }
  return out;
}

} }
#endif
