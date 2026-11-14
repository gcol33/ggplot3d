#pragma once
#include "projection.h"
#include "budget.h"
#include <cstdint>
#include <cstring>
#include <map>

namespace ggplot3d { namespace engine {

struct Piece {
  const PrimitiveTable* source;
  Index row;
  Kind kind;
  Polygon polygon;
  Polygon anchor;
  Vec3 normal;
  double path_start, path_end;
  bool clipped, arrow_start, arrow_end;
  std::size_t serial;
  bool hidden = false;
  Polygon mask;
  bool mask_required = true;
  std::size_t whole_stroke_parts = 0;
  bool cap_start = true, cap_end = true;
  Index solid = -1;
  Index seam_group = 0;
  // Pieces of one primitive with the same nonzero patch form one smooth
  // surface seen from one side (a quadric part, front or back), which a
  // backend paints in one operation.
  int patch = 0;
  bool behind_halo = false;
  // Hash of the source primitive's geometry, the order of stable cuts.
  std::uint64_t key = 0;
  std::vector<Piece> members;
};

struct SplitPlane { Vec3 normal, origin; };

// splitmix64 (Steele, Lea and Flood 2014) over the bits of a value sequence.
static inline std::uint64_t mix_key(std::uint64_t key, double value) {
  std::uint64_t bits;
  if (value == 0) value = 0;
  std::memcpy(&bits, &value, sizeof bits);
  key += bits + 0x9E3779B97F4A7C15ull;
  key = (key ^ (key >> 30)) * 0xBF58476D1CE4E5B9ull;
  key = (key ^ (key >> 27)) * 0x94D049BB133111EBull;
  return key ^ (key >> 31);
}

static inline void budget_split(const Piece& piece,ResourceBudget& budget) {
  budget.piece(piece.polygon.size()+piece.anchor.size()+piece.mask.size()+1);
  for(const Polygon* polygon:{&piece.polygon,&piece.anchor}) for(const auto& vertex:*polygon)
    if(vertex.weights.size()>2) budget.add(64.0*(vertex.weights.size()-2));
}

static inline double plane_distance(const SplitPlane& plane, const Vertex& vertex,
                                    const Projection& projection) {
  const Vec3 p = projection.model(vertex.xyz);
  long double value = 0;
  for (int k = 0; k < 3; ++k)
    value += static_cast<long double>(plane.normal[k]) *
      (static_cast<long double>(p[k]) - plane.origin[k]);
  return static_cast<double>(value);
}

static inline int side(double distance, double tolerance) {
  return distance > tolerance ? 1 : (distance < -tolerance ? -1 : 0);
}

static inline int classify(const Piece& piece, const SplitPlane& plane,
                           const Projection& projection) {
  int mask = 0;
  for (const Vertex& v : piece.polygon) {
    int s = side(plane_distance(plane, v, projection), projection.resolved.tolerance);
    if (s > 0) mask |= 1;
    if (s < 0) mask |= 2;
  }
  return mask;
}

static inline SplitPlane supporting_plane(const Piece& piece, const Projection& p) {
  Vec3 normal = piece.normal;
  if (piece.kind != Kind::face && dot(normal,normal)==0) {
    normal = p.resolved.forward;
    if (piece.polygon.size() == 2) {
      Vec3 direction = subtract(p.model(piece.polygon[1].xyz), p.model(piece.polygon[0].xyz));
      normal = cross(direction, p.resolved.right);
      Vec3 alternate = cross(direction, p.resolved.up);
      if (dot(alternate, alternate) > dot(normal, normal)) normal = alternate;
      double length = std::sqrt(dot(normal, normal));
      if (length > 0) for (double& x : normal) x /= length;
      else normal = p.resolved.forward;
    }
  }
  return {normal, p.model(piece.polygon[0].xyz)};
}

static inline double projected_length(const Vertex& a, const Vertex& b, const Projection& p) {
  auto x = p.npc(p.camera_point(a.xyz)), y = p.npc(p.camera_point(b.xyz));
  return std::hypot((x[0]-y[0])*p.viewport.width_mm, (x[1]-y[1])*p.viewport.height_mm);
}

static inline std::pair<Piece, Piece> split_piece(Piece piece, const SplitPlane& plane,
                                                const Projection& projection) {
  Piece positive = piece, negative = piece;
  positive.polygon.clear(); negative.polygon.clear();
  positive.clipped = negative.clipped = true;
  positive.whole_stroke_parts = negative.whole_stroke_parts = 0;
  if (piece.polygon.size() == 2) {
    const Vertex& a = piece.polygon[0]; const Vertex& b = piece.polygon[1];
    double da = plane_distance(plane, a, projection), db = plane_distance(plane, b, projection);
    Vertex cut = interpolate(a, b, da/(da-db));
    double phase = piece.path_start + projected_length(a, cut, projection);
    Piece* first = da > 0 ? &positive : &negative;
    Piece* last = da > 0 ? &negative : &positive;
    first->polygon = {a, cut}; last->polygon = {cut, b};
    if(piece.anchor.size()==2) {
      double screen_t=projected_length(a,cut,projection)/projected_length(a,b,projection);
      double t=screen_t;
      if(projection.camera.perspective) {
        double da=projection.camera_point(piece.anchor[0].xyz)[2],db=projection.camera_point(piece.anchor[1].xyz)[2];
        t=(screen_t/db)/((1-screen_t)/da+screen_t/db);
      }
      Vertex source_cut=interpolate(piece.anchor[0],piece.anchor[1],t);
      first->anchor={piece.anchor[0],source_cut};last->anchor={source_cut,piece.anchor[1]};
    }
    first->path_end = last->path_start = phase;
    first->arrow_end = last->arrow_start = false;
  } else {
    const Vertex* a = &piece.polygon.back();
    double da = plane_distance(plane, *a, projection);
    int sa = side(da, projection.resolved.tolerance);
    for (const Vertex& b : piece.polygon) {
      double db = plane_distance(plane, b, projection);
      int sb = side(db, projection.resolved.tolerance);
      if (sa*sb < 0) {
        Vertex cut = interpolate(*a, b, da/(da-db));
        positive.polygon.push_back(cut); negative.polygon.push_back(std::move(cut));
      }
      if (sb >= 0) positive.polygon.push_back(b);
      if (sb <= 0) negative.polygon.push_back(b);
      a = &b; da = db; sa = sb;
    }
  }
  return {std::move(positive), std::move(negative)};
}

struct PartitionStats {
  std::size_t candidates = 0, classifications = 0, splits = 0, nodes = 0;
  std::size_t pairs = 0, edges = 0, components = 0, cyclic = 0, largest_scc = 0;
  std::map<std::size_t,std::size_t> scc_sizes;
};

static inline SplitPlane choose_plane(const std::vector<Piece>& pieces,
                                      const Projection& projection, PartitionStats& stats) {
  const std::size_t n = pieces.size();
  SplitPlane best = supporting_plane(pieces.front(), projection);
  double best_score = std::numeric_limits<double>::infinity();
  auto score = [&](const SplitPlane& plane, bool spatial) {
    std::size_t positive = 0, negative = 0, split = 0, coplanar = 0;
    ++stats.candidates;
    for (const Piece& piece : pieces) {
      int c = classify(piece, plane, projection); ++stats.classifications;
      positive += (c & 1) != 0; negative += (c & 2) != 0;
      split += c == 3; coplanar += c == 0;
    }
    if (spatial && std::max(positive, negative) >= n*0.85) return;
    if (!spatial && coplanar == 0) return;
    double value = 8.0*split + std::max(positive, negative);
    if (value < best_score) { best_score = value; best = plane; }
  };
  const std::size_t samples = std::min<std::size_t>(8, n);
  for (std::size_t j = 0; j < samples; ++j) {
    score(supporting_plane(pieces[j*n/samples], projection), false);
    if (best_score == 0) return best;
  }
  if (n > 32) {
    Vec3 lo{R_PosInf,R_PosInf,R_PosInf}, hi{R_NegInf,R_NegInf,R_NegInf};
    std::vector<Vec3> centres; centres.reserve(n);
    for (const Piece& piece : pieces) {
      Vec3 centre{};
      for (const Vertex& v : piece.polygon) {
        Vec3 m = projection.model(v.xyz);
        for (int k = 0; k < 3; ++k) centre[k] += m[k]/piece.polygon.size();
      }
      centres.push_back(centre);
      for (int k = 0; k < 3; ++k) { lo[k] = std::min(lo[k],centre[k]); hi[k] = std::max(hi[k],centre[k]); }
    }
    int axis = 0;
    for (int k = 1; k < 3; ++k) if (hi[k]-lo[k] > hi[axis]-lo[axis]) axis = k;
    auto middle = centres.begin()+n/2;
    std::nth_element(centres.begin(), middle, centres.end(),
      [axis](const Vec3& a, const Vec3& b) { return a[axis] < b[axis]; });
    Vec3 normal{}; normal[axis] = 1;
    score({normal, *middle}, true);
  }
  require(std::isfinite(best_score), "no numerically representable BSP supporting plane");
  return best;
}

static inline bool tie_less(const Piece& a, const Piece& b) {
  const auto& x = *a.source; const auto& y = *b.source;
  return std::tie(x.role[a.row],x.layer_id[a.row],x.source_row[a.row],a.serial) <
    std::tie(y.role[b.row],y.layer_id[b.row],y.source_row[b.row],b.serial);
}

// The face whose plane cuts a set in a stable partition: the one first in a
// fixed order of the primitives, which is a hash of their geometry. This is
// Paterson and Yao's (1990) autopartition with a random order, drawn from the
// geometry so that it is the same in every frame; the cuts of a set then
// depend on its members alone, never on the camera, and a member that joins or
// leaves changes only the cuts of the cells it reaches first.
static inline const Piece* stable_splitter(const std::vector<Piece>& pieces) {
  const Piece* first = nullptr;
  for (const Piece& piece : pieces)
    if (piece.kind == Kind::face && piece.members.empty() &&
        (!first || std::tie(piece.key, piece.serial) < std::tie(first->key, first->serial))) first = &piece;
  return first;
}

// Back-to-front order of pieces by recursive partition (Fuchs, Kedem and
// Naylor 1980). A stable partition cuts on the planes of its stable splitters
// while the set holds a face.
static inline std::vector<Piece> partition_scene(std::vector<Piece> pieces,
                                                const Projection& projection,
                                                PartitionStats& stats, ResourceBudget& budget,
                                                bool stable = false) {
  struct Work { std::vector<Piece> pieces; bool terminal; };
  std::vector<Work> stack;
  stack.push_back({std::move(pieces), false});
  std::vector<Piece> ordered;
  while (!stack.empty()) {
    Work work = std::move(stack.back()); stack.pop_back();
    if (work.pieces.empty()) continue;
    if (work.terminal || work.pieces.size() == 1) {
      std::stable_sort(work.pieces.begin(), work.pieces.end(), tie_less);
      for (Piece& piece : work.pieces) ordered.push_back(std::move(piece));
      continue;
    }
    if (stats.nodes++ % 256 == 0) Rcpp::checkUserInterrupt();
    const Piece* splitter = stable ? stable_splitter(work.pieces) : nullptr;
    const SplitPlane plane = splitter ? supporting_plane(*splitter, projection) :
      choose_plane(work.pieces, projection, stats);
    std::vector<Piece> positive, negative, coplanar;
    for (Piece& piece : work.pieces) {
      int c = classify(piece, plane, projection);
      if (c == 0) coplanar.push_back(std::move(piece));
      else if (c == 1) positive.push_back(std::move(piece));
      else if (c == 2) negative.push_back(std::move(piece));
      else {
        budget_split(piece,budget);
        auto halves = split_piece(std::move(piece), plane, projection); ++stats.splits;
        positive.push_back(std::move(halves.first)); negative.push_back(std::move(halves.second));
      }
    }
    double eye_side = projection.camera.perspective ?
      dot(plane.normal, subtract(projection.resolved.eye, plane.origin)) :
      -dot(plane.normal, projection.resolved.ray);
    auto& near = eye_side >= 0 ? positive : negative;
    auto& far = eye_side >= 0 ? negative : positive;
    stack.push_back({std::move(near), false});
    stack.push_back({std::move(coplanar), true});
    stack.push_back({std::move(far), false});
  }
  return ordered;
}

} }
