#include <Rcpp.h>
#include "geometry/printing.h"
#include <cstdint>
using namespace Rcpp;

// [[Rcpp::export]]
List print_mesh_cpp(NumericMatrix position,IntegerMatrix triangles,bool cross_components=false,Nullable<IntegerVector> identity=R_NilValue) {
  using namespace ggplot3d_geometry;
  IntegerVector ids=identity.isNotNull()?IntegerVector(identity):IntegerVector();
  printing::Mesh mesh(position,triangles,cross_components,ids);List out(mesh.groups.size());
  for(size_t g=0;g<mesh.groups.size();++g) {
    std::map<int,int> index;std::vector<int> vertex;
    for(int i:mesh.groups[g]) for(int j:mesh.tri[i]) if(!index.count(j)) { index[j]=vertex.size();vertex.push_back(j); }
    NumericMatrix p(vertex.size(),3),normal(vertex.size(),3);IntegerMatrix t(mesh.groups[g].size(),3);
    std::vector<V> normals(vertex.size(),V{0,0,0});
    for(int i:mesh.groups[g]) { auto f=mesh.tri[i];V n=cross(sub(mesh.p[f[1]],mesh.p[f[0]]),sub(mesh.p[f[2]],mesh.p[f[0]]));for(int j:f) normals[index[j]]=add(normals[index[j]],n); }
    for(V& n:normals) n=unit(n);
    IntegerVector source(mesh.groups[g].size());
    for(size_t i=0;i<vertex.size();++i) for(int k=0;k<3;++k) { p(i,k)=mesh.p[vertex[i]][k];normal(i,k)=normals[i][k]; }
    for(size_t i=0;i<mesh.groups[g].size();++i) { int j=mesh.groups[g][i];source[i]=mesh.source[j]+1;for(int k=0;k<3;++k) t(i,k)=index[mesh.tri[j][k]]+1; }
    std::vector<std::pair<int,int>> edges;std::vector<int> boundary_source;
    for(size_t k=0;k<mesh.boundary.size();++k) { auto e=mesh.boundary[k];if(mesh.component[mesh.boundary_face[k]]==static_cast<int>(g)) { edges.push_back({index[e.first]+1,index[e.second]+1});boundary_source.push_back(mesh.source[mesh.boundary_face[k]]+1); } }
    IntegerMatrix boundary(edges.size(),2);for(size_t i=0;i<edges.size();++i) { boundary(i,0)=edges[i].first;boundary(i,1)=edges[i].second; }
    out[g]=List::create(_["position"]=p,_["triangles"]=t,_["normal"]=normal,_["source"]=source,
      _["boundary"]=boundary,_["boundary_source"]=wrap(boundary_source),_["closed"]=static_cast<bool>(mesh.closed[g]),_["volume"]=mesh.volume[g]);
  }
  NumericMatrix vertices(mesh.p.size(),3);IntegerMatrix faces(mesh.tri.size(),3);IntegerVector source(mesh.source.size());
  for(size_t i=0;i<mesh.p.size();++i) for(int k=0;k<3;++k) vertices(i,k)=mesh.p[i][k];
  for(size_t i=0;i<mesh.tri.size();++i) { source[i]=mesh.source[i]+1;for(int k=0;k<3;++k) faces(i,k)=mesh.tri[i][k]+1; }
  return List::create(_["parts"]=out,_["position"]=vertices,_["triangles"]=faces,_["source"]=source);
}

// ZIP archives with stored members, UTF-8 names and CRC-32 checksums.
// [[Rcpp::export]]
RawVector print_zip_cpp(CharacterVector names,List contents) {
  if(names.size()!=contents.size()||names.size()>65535) stop("invalid ZIP member count");
  std::vector<unsigned char> out,central;
  auto word=[](std::vector<unsigned char>& data,uint32_t value,int n) { for(int i=0;i<n;++i) data.push_back((value>>(8*i))&255); };
  auto crc=[](RawVector data) { uint32_t c=0xffffffff;for(unsigned char b:data) { c^=b;for(int k=0;k<8;++k) c=(c>>1)^((c&1)?0xedb88320:0); }return c^0xffffffff; };
  for(int i=0;i<names.size();++i) {
    std::string name=as<std::string>(names[i]);RawVector data=contents[i];
    if(name.size()>65535||data.size()>UINT32_MAX||out.size()>UINT32_MAX) stop("ZIP member exceeds 32-bit archive limits");
    uint32_t checksum=crc(data),offset=out.size(),size=data.size();
    word(out,0x04034b50,4);word(out,20,2);word(out,2048,2);word(out,0,2);word(out,0,2);word(out,33,2);
    word(out,checksum,4);word(out,size,4);word(out,size,4);word(out,name.size(),2);word(out,0,2);
    out.insert(out.end(),name.begin(),name.end());out.insert(out.end(),data.begin(),data.end());
    word(central,0x02014b50,4);word(central,20,2);word(central,20,2);word(central,2048,2);word(central,0,2);word(central,0,2);word(central,33,2);
    word(central,checksum,4);word(central,size,4);word(central,size,4);word(central,name.size(),2);word(central,0,2);word(central,0,2);
    word(central,0,2);word(central,0,2);word(central,0,4);word(central,offset,4);central.insert(central.end(),name.begin(),name.end());
  }
  uint32_t offset=out.size(),size=central.size();out.insert(out.end(),central.begin(),central.end());
  word(out,0x06054b50,4);word(out,0,2);word(out,0,2);word(out,names.size(),2);word(out,names.size(),2);word(out,size,4);word(out,offset,4);word(out,0,2);
  return RawVector(out.begin(),out.end());
}
