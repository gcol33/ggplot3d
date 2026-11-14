#pragma once
#include <algorithm>
#include <cmath>
#include <map>
#include <tuple>
#include <utility>
#include <vector>

// Hatching strokes of a triangle mesh (Praun et al. 2001; Hertzmann and
// Zorin 2000): each family of lines is the intersection of the mesh with the
// planes d . p = spacing * k for integer k. Line k has rank 0 when k is a
// multiple of 4, 1 when it is otherwise even and 2 when odd, so the lines of
// rank below r nest inside those of rank below r + 1, as in a tonal art map. A
// triangle keeps the lines whose rank is below its level for that family.
// Pieces are chained across shared edges into polylines: the crossing on an
// edge is computed from its two vertices in a fixed order, so neighbouring
// triangles produce the same point.
namespace ggplot3d { namespace hatch {

struct Lines {
  std::vector<double> x, y, z;
  std::vector<int> line, family, face;
};

static inline int rank_of(long k) {
  const long m = ((k % 4) + 4) % 4;
  return m == 0 ? 0 : m == 2 ? 1 : 2;
}

// xyz: vertex positions (n x 3, column major), faces: 0-based vertex indices
// (m x 3, column major), directions: unit plane normals (f x 3), levels:
// lines drawn per face and family (m x f, column major, 0 to 3).
static inline Lines lines(const double* xyz, int n, const int* faces, int m, const double* directions, int families,
                          double spacing, const int* levels) {
  Lines out;
  struct Node { int a, b; long k; };
  for (int f = 0; f < families; ++f) {
    const double d[3] = {directions[f], directions[f + families], directions[f + 2 * families]};
    std::vector<double> h(n);
    for (int i = 0; i < n; ++i) h[i] = (xyz[i] * d[0] + xyz[i + n] * d[1] + xyz[i + 2 * n] * d[2]) / spacing;
    std::map<std::tuple<int, int, long>, int> index;
    std::vector<Node> nodes;
    std::vector<std::vector<int>> adjacent;
    std::vector<int> seg_a, seg_b, seg_face;
    auto node_of = [&](int a, int b, long k) {
      if (a > b) std::swap(a, b);
      const auto key = std::make_tuple(a, b, k);
      auto it = index.find(key);
      if (it != index.end()) return it->second;
      const int id = static_cast<int>(nodes.size());
      nodes.push_back({a, b, k});
      adjacent.emplace_back();
      index.emplace(key, id);
      return id;
    };
    for (int t = 0; t < m; ++t) {
      const int level = levels[t + f * m];
      if (level <= 0) continue;
      const int v[3] = {faces[t], faces[t + m], faces[t + 2 * m]};
      const double lo = std::min(h[v[0]], std::min(h[v[1]], h[v[2]]));
      const double hi = std::max(h[v[0]], std::max(h[v[1]], h[v[2]]));
      if (!std::isfinite(lo) || !std::isfinite(hi)) continue;
      for (long k = static_cast<long>(std::ceil(lo)); k <= static_cast<long>(std::floor(hi)); ++k) {
        if (rank_of(k) >= level) continue;
        int found[2]; int count = 0;
        for (int e = 0; e < 3 && count < 2; ++e) {
          const int a = v[e], b = v[(e + 1) % 3];
          if ((h[a] >= k) != (h[b] >= k)) found[count++] = node_of(a, b, k);
        }
        if (count != 2 || found[0] == found[1]) continue;
        const int s = static_cast<int>(seg_a.size());
        seg_a.push_back(found[0]); seg_b.push_back(found[1]); seg_face.push_back(t);
        adjacent[found[0]].push_back(s); adjacent[found[1]].push_back(s);
      }
    }
    auto point = [&](int id, double* p) {
      const Node& nd = nodes[id];
      const double t = (nd.k - h[nd.a]) / (h[nd.b] - h[nd.a]);
      for (int c = 0; c < 3; ++c) p[c] = xyz[nd.a + c * n] + t * (xyz[nd.b + c * n] - xyz[nd.a + c * n]);
    };
    std::vector<char> used(seg_a.size(), 0);
    auto walk = [&](int start_node, int start_seg) {
      const int line = out.line.empty() ? 1 : out.line.back() + 1;
      double p[3];
      int node = start_node, seg = start_seg;
      point(node, p);
      out.x.push_back(p[0]); out.y.push_back(p[1]); out.z.push_back(p[2]);
      out.line.push_back(line); out.family.push_back(f + 1); out.face.push_back(seg_face[seg]);
      while (seg >= 0 && !used[seg]) {
        used[seg] = 1;
        node = seg_a[seg] == node ? seg_b[seg] : seg_a[seg];
        point(node, p);
        out.x.push_back(p[0]); out.y.push_back(p[1]); out.z.push_back(p[2]);
        out.line.push_back(line); out.family.push_back(f + 1); out.face.push_back(seg_face[seg]);
        int next = -1;
        for (int s : adjacent[node]) if (!used[s]) { next = s; break; }
        seg = next;
      }
    };
    for (std::size_t i = 0; i < nodes.size(); ++i) {
      if (adjacent[i].size() != 1) continue;
      const int s = adjacent[i][0];
      if (!used[s]) walk(static_cast<int>(i), s);
    }
    for (std::size_t s = 0; s < seg_a.size(); ++s) if (!used[s]) walk(seg_a[s], static_cast<int>(s));
  }
  return out;
}
} }
