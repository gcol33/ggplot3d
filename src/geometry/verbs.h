#ifndef GGPLOT3D_GEOMETRY_VERBS_H
#define GGPLOT3D_GEOMETRY_VERBS_H
#include "processing.h"
#include "heat.h"
namespace ggplot3d_geometry {
namespace halfedge {

// Mesh verbs by name, as called from mesh_process_cpp().
using Verb=Result(*)(const Soup&,const Rcpp::List&);
static inline const std::map<std::string,Verb>& verbs() {
  static const std::map<std::string,Verb> table{
    {"normals",normals},{"curvature",curvature},{"subdivide",subdivide},
    {"smooth",smooth},{"decimate",decimate},{"remesh",remesh},
    {"geodesic_distance",geodesic_distance},{"logmap",logmap}
  };
  return table;
}
}
}
#endif
