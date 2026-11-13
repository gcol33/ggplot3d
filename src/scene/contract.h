#ifndef GGPLOT3D_SCENE_CONTRACT_H
#define GGPLOT3D_SCENE_CONTRACT_H

#include <array>
#include <cstdint>
#include <vector>

namespace ggplot3d {

inline constexpr int scene_schema_version = 1;
using Index = std::int32_t;
enum class Kind : Index { face = 1, polyline = 2, marker = 3, text = 4 };
enum class Role : Index { fill = 0, stroke = 1, marker = 2, text = 3 };
enum class Partition : Index { underlay = 0, depth = 1, overlay = 2 };
enum class FootprintKind : Index { ellipse = 1, polygon = 2 };
enum class Hidden : Index { hide = 0, dashed = 1, show = 2 };
enum class Cap : Index { butt = 0, round = 1, square = 2 };
enum class Join : Index { round = 0, mitre = 1, bevel = 2 };

struct Vertices {
  std::vector<double> x, y, z;
};

// Indices and offsets are zero-based in C++; the R wire format is one-based.
struct Corners {
  std::vector<Index> vertex, source_row;
};

struct PrimitiveTable {
  std::vector<Index> primitive_id, layer_id, source_row, style_id;
  std::vector<Index> offset, count, role, partition;
  std::vector<double> opacity;
};

struct Faces : PrimitiveTable {};
struct Polylines : PrimitiveTable {
  std::vector<double> width_mm, mitre_limit;
  std::vector<Index> cap, join, hidden;
  std::vector<bool> arrow_start, arrow_end;
};
struct Markers : PrimitiveTable {
  std::vector<Index> footprint_id;
};
struct TextAnchors : PrimitiveTable {
  std::vector<double> width_mm, height_mm, hjust, vjust, angle;
};

// Polygon rings use the even-odd rule; ellipses use radii and angle only.
struct Footprints {
  std::vector<Index> kind, offset, count;
  std::vector<double> radius_x_mm, radius_y_mm, angle;
};
struct FootprintVertices {
  std::vector<double> x_mm, y_mm;
  std::vector<Index> ring;
};

struct ScenePacket {
  Index schema_version = scene_schema_version, panel_id = 1, style_count = 0;
  Vertices vertices;
  Corners corners;
  Faces faces;
  Polylines polylines;
  Markers markers;
  TextAnchors text;
  Footprints footprints;
  FootprintVertices footprint_vertices;
};

struct Camera {
  bool perspective = false, clip_data = true, clip = true;
  double azimuth = -60, elevation = 30, roll = 0;
  // Vertical model axis (0 x, 1 y, 2 z) and the oblique screen shear per unit depth.
  int up_axis = 2;
  std::array<double, 2> shear{0, 0};
  double distance, fov = 35, zoom = 1, near, far;
  std::array<double, 3> target{0.5, 0.5, 0.5}, aspect{1, 1, 1};
};
struct Viewport {
  double width_mm, height_mm, dpi = 144;
};
struct ResolvedCamera {
  std::array<double, 16> view{}, projection{};
  // right, up and forward are orthonormal; ray is the parallel sight direction
  // (forward unless oblique); screen_right and screen_up are the sheared screen axes.
  std::array<double, 3> eye{}, right{}, up{}, forward{}, ray{}, screen_right{}, screen_up{};
  std::array<double, 3> model_scale{};
  double distance, near, far, view_height, tolerance;
};

struct ProjectedVertices : Vertices {
  std::vector<double> x_camera, y_camera, depth, x_npc, y_npc, clip_w;
};

// Each output vertex is an affine combination of corners of its source primitive.
struct VertexWeights {
  std::vector<Index> vertex, corner;
  std::vector<double> weight;
};

struct Fragments : PrimitiveTable {
  std::vector<Index> fragment_id, kind, parent_primitive_id, source_index;
  std::vector<Index> mask_offset, mask_count;
  std::vector<double> depth_min, depth_max, depth_centroid;
  std::vector<double> normal_x, normal_y, normal_z;
  std::vector<double> path_start_mm, path_end_mm;
  std::vector<bool> clipped, hidden, arrow_start, arrow_end;
  std::vector<Index> seam_group;
};
struct ClipVertices {
  std::vector<double> x_npc, y_npc;
  std::vector<Index> ring;
};
struct CompiledScene {
  ProjectedVertices vertices;
  VertexWeights weights;
  Fragments fragments;
  ClipVertices masks;
  std::vector<Index> draw_order;
  ResolvedCamera camera;
};

}  // namespace ggplot3d
#endif
