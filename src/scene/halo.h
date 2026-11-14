#pragma once
#include "common.h"

namespace ggplot3d { namespace engine {

// Halo exemptions between polylines. A haloed line leaves a gap where
// another line passes behind it (Appel, Rohlf and Stein 1979); two lines
// that end at the same vertex meet there instead, so neither cuts the other
// near their common end. Lines meet when an end of one has the same packet
// position as an end of the other.
class HaloJoins {
  std::vector<std::array<double,8>> ends;

  static bool same(const double* a,const double* b) {
    for(int k=0;k<4;++k) if(std::abs(a[k]-b[k])>1e-12*std::max(1.0,std::abs(a[k]))) return false;
    return true;
  }

public:
  explicit HaloJoins(const ScenePacket& packet) {
    const Polylines& line=packet.polylines;
    const Vertices& v=packet.vertices;
    ends.resize(line.offset.size());
    for(std::size_t row=0;row<line.offset.size();++row) {
      if(line.count[row]<1) continue;
      Index corners[2]={line.offset[row],line.offset[row]+line.count[row]-1};
      for(int e=0;e<2;++e) {
        Index i=packet.corners.vertex[corners[e]];
        double w=v.w.empty() ? 1:v.w[i];
        ends[row][4*e]=v.x[i];ends[row][4*e+1]=v.y[i];ends[row][4*e+2]=v.z[i];ends[row][4*e+3]=w;
      }
    }
  }

  // Whether polyline rows a and b leave each other uncut: the same line, or
  // two lines with a common end.
  bool joined(Index a,Index b) const {
    if(a==b) return true;
    if(a<0 || b<0 || static_cast<std::size_t>(std::max(a,b))>=ends.size()) return false;
    for(int i=0;i<2;++i) for(int j=0;j<2;++j)
      if(same(ends[a].data()+4*i,ends[b].data()+4*j)) return true;
    return false;
  }
};

} }
