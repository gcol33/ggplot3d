#include <Rcpp.h>
#include "geometry/tetrahedron.h"
#include <map>
#include <vector>
#include <algorithm>
using namespace Rcpp;

// [[Rcpp::export]]
List geometry_section_mesh_cpp(NumericMatrix positions,NumericVector signed_distance,IntegerMatrix cells) {
  using Point=std::array<double,3>;
  using Face=std::array<int,3>;
  std::vector<Point> points;
  std::vector<Face> faces;
  std::vector<int> source;
  std::vector<bool> volume;
  std::map<std::pair<int,int>,int> crossing;
  auto vertex=[&](int a,int b) {
    if(signed_distance[a]==0) b=a;
    else if(signed_distance[b]==0) a=b;
    if(a>b) std::swap(a,b);
    auto key=std::make_pair(a,b); auto found=crossing.find(key);
    if(found!=crossing.end()) return found->second;
    double fraction=a==b?0:signed_distance[a]/(signed_distance[a]-signed_distance[b]);
    Point point;
    for(int k=0;k<3;++k) point[k]=positions(a,k)+fraction*(positions(b,k)-positions(a,k));
    int index=points.size(); points.push_back(point); crossing[key]=index; return index;
  };
  for(int cell=0;cell<cells.nrow();++cell) {
    std::array<int,4> indices; int zeros=0;
    for(int j=0;j<4;++j) { indices[j]=cells(cell,j)-1; if(signed_distance[indices[j]]==0) ++zeros; }
    auto triangle=[&](int a,int b,int c,bool solid) {
      if(a==b||a==c||b==c) return;
      faces.push_back({a,b,c}); source.push_back(cell+1); volume.push_back(solid);
    };
    if(zeros==4) {
      int id[4]; for(int j=0;j<4;++j) id[j]=vertex(indices[j],indices[j]);
      triangle(id[0],id[1],id[2],true); triangle(id[0],id[3],id[1],true);
      triangle(id[0],id[2],id[3],true); triangle(id[1],id[3],id[2],true);
    } else if(zeros==3) {
      int id[3],j=0;
      for(int a:indices) if(signed_distance[a]==0) id[j++]=vertex(a,a);
      triangle(id[0],id[1],id[2],false);
    } else {
      ggplot3d_geometry::tetrahedron_level(indices,[&](int a){return signed_distance[a]>0;},vertex,
        [&](int a,int b,int c,int,int){triangle(a,b,c,false);});
    }
  }
  std::map<Face,int> volume_count;
  for(size_t i=0;i<faces.size();++i) if(volume[i]) {
    Face key=faces[i]; std::sort(key.begin(),key.end()); ++volume_count[key];
  }
  std::map<Face,bool> seen;
  std::vector<size_t> keep;
  for(size_t i=0;i<faces.size();++i) {
    Face key=faces[i]; std::sort(key.begin(),key.end());
    if(volume_count[key]>1||seen[key]) continue;
    seen[key]=true; keep.push_back(i);
  }
  NumericMatrix output(points.size(),3); IntegerMatrix triangles(keep.size(),3); IntegerVector owners(keep.size());
  for(size_t i=0;i<points.size();++i) for(int j=0;j<3;++j) output(i,j)=points[i][j];
  for(size_t i=0;i<keep.size();++i) {
    for(int j=0;j<3;++j) triangles(i,j)=faces[keep[i]][j]+1;
    owners[i]=source[keep[i]];
  }
  return List::create(_["vertices"]=output,_["triangles"]=triangles,_["cell"]=owners);
}
