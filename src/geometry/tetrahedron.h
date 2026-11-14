#ifndef GGPLOT3D_GEOMETRY_TETRAHEDRON_H
#define GGPLOT3D_GEOMETRY_TETRAHEDRON_H
#include <array>
namespace ggplot3d_geometry {

// Triangles of a linear scalar level in a tetrahedron. Edge returns a welded
// crossing index; triangle receives cyclic indices and a high/low direction.
template<class High,class Edge,class Triangle>
static inline void tetrahedron_level(const std::array<int,4>& vertices,High high,Edge edge,Triangle triangle) {
  int hi[4],lo[4],nh=0,nl=0;
  for(int a:vertices) { if(high(a)) hi[nh++]=a; else lo[nl++]=a; }
  if(!nh||!nl) return;
  if(nh==1||nl==1) {
    int a=nh==1?hi[0]:lo[0]; const int* others=nh==1?lo:hi;
    triangle(edge(a,others[0]),edge(a,others[1]),edge(a,others[2]),hi[0],lo[0]);
  } else {
    int a=edge(hi[0],lo[0]),b=edge(hi[0],lo[1]),c=edge(hi[1],lo[0]),d=edge(hi[1],lo[1]);
    triangle(a,b,d,hi[0],lo[0]); triangle(a,d,c,hi[0],lo[0]);
  }
}
}
#endif
