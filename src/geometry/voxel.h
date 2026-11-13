#ifndef GGPLOT3D_GEOMETRY_VOXEL_H
#define GGPLOT3D_GEOMETRY_VOXEL_H
#include <vector>
#include <array>
namespace ggplot3d_geometry {
namespace voxel {

// A rectangle of voxel faces in the lattice: normal along axis, facing side
// (+1 or -1), lying in the lattice plane `plane`, spanning [u0, u1) x [v0, v1)
// in the axes (axis + 1) % 3 and (axis + 2) % 3.
struct Rectangle { int axis, side, plane, u0, v0, u1, v1, material; };

// Visible faces of a dense voxel grid. material holds 0 for empty cells and
// a positive material id otherwise (x fastest). A face of a filled cell is
// visible when the neighbour is empty or a translucent cell of a different
// material, so faces between opaque cells and between cells of one material
// are culled. Visible faces of one material are merged per slice and
// direction into maximal rectangles, first along u then along v (greedy
// meshing, Lysenko 2012).
static inline std::vector<Rectangle> greedy_faces(const int n[3],const std::vector<int>& material,
                                                  const std::vector<char>& translucent,bool greedy) {
  auto at=[&](int q[3]) {
    for(int a=0;a<3;++a) if(q[a]<0||q[a]>=n[a]) return 0;
    return material[q[0]+n[0]*(q[1]+n[1]*q[2])];
  };
  std::vector<Rectangle> out;
  for(int d=0;d<3;++d) {
    int u=(d+1)%3,v=(d+2)%3;
    std::vector<int> mask(n[u]*n[v]);
    for(int side=-1;side<=1;side+=2) for(int slice=0;slice<n[d];++slice) {
      for(int b=0;b<n[v];++b) for(int a=0;a<n[u];++a) {
        int q[3]; q[d]=slice; q[u]=a; q[v]=b;
        int m=at(q),visible=0;
        if(m) {
          q[d]+=side; int other=at(q);
          if(!other||(other!=m&&translucent[other])) visible=m;
        }
        mask[a+n[u]*b]=visible;
      }
      int plane=side>0?slice+1:slice;
      for(int b=0;b<n[v];++b) for(int a=0;a<n[u];) {
        int m=mask[a+n[u]*b];
        if(!m) { ++a; continue; }
        int w=1,h=1;
        if(greedy) {
          while(a+w<n[u]&&mask[a+w+n[u]*b]==m) ++w;
          bool grow=true;
          while(grow&&b+h<n[v]) {
            for(int k=0;k<w;++k) if(mask[a+k+n[u]*(b+h)]!=m) { grow=false; break; }
            if(grow) ++h;
          }
        }
        for(int y=0;y<h;++y) for(int x=0;x<w;++x) mask[a+x+n[u]*(b+y)]=0;
        out.push_back({d,side,plane,a,b,a+w,b+h,m});
        a+=w;
      }
    }
  }
  return out;
}
}
}
#endif
