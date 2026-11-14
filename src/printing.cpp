#include <Rcpp.h>
#include "geometry/printing.h"
#include <cstdint>
#include <array>
#include <limits>
#include <unordered_set>
#include "manifold3d/manifold.h"
#include "manifold3d/cross_section.h"
using namespace Rcpp;

namespace {
using manifold::Manifold;
using manifold::CrossSection;
using manifold::OpType;

Manifold checked_solid(const Manifold& solid, const std::string& label) {
  checkUserInterrupt();
  const auto status = solid.Status();
  if (status != Manifold::Error::NoError) {
    static const char* errors[] = {
      "NoError", "NonFiniteVertex", "NotManifold", "VertexOutOfBounds",
      "PropertiesWrongLength", "MissingPositionProperties", "MergeVectorsDifferentLengths",
      "MergeIndexOutOfBounds", "TransformWrongLength", "RunIndexWrongLength",
      "FaceIDWrongLength", "InvalidConstruction", "ResultTooLarge", "InvalidTangents", "Cancelled"
    };
    stop("%s: Manifold reports %s", label, errors[static_cast<int>(status)]);
  }
  if (solid.IsEmpty()) stop("%s: solid is empty", label);
  return solid;
}

Manifold coloured_solid(const Manifold& solid, NumericVector rgba) {
  if (rgba.size() != 4 || is_true(any(!is_finite(rgba)))) stop("colour needs four finite channels");
  const std::array<double, 4> colour{{rgba[0], rgba[1], rgba[2], rgba[3]}};
  return solid.SetProperties(4, [colour](double* out, manifold::vec3, const double*) {
    std::copy(colour.begin(), colour.end(), out);
  });
}

Manifold mesh_solid(const List& part, const std::string& label) {
  NumericMatrix position = part["position"], colour = part["colour"];
  IntegerMatrix triangles = part["triangles"];
  if (position.ncol() != 3 || triangles.ncol() != 3 || colour.ncol() != 4 ||
      colour.nrow() != triangles.nrow()) stop("%s: invalid mesh dimensions", label);
  manifold::MeshGL64 mesh;
  mesh.numProp = 7;
  const size_t corners = static_cast<size_t>(triangles.nrow()) * 3;
  mesh.vertProperties.reserve(corners * 7);
  mesh.triVerts.reserve(corners);
  std::vector<int64_t> first(position.nrow(), -1);
  for (int i = 0; i < triangles.nrow(); ++i) {
    if (i % 1024 == 0) checkUserInterrupt();
    for (int k = 0; k < 3; ++k) {
      const int vertex = triangles(i, k);
      if (vertex == NA_INTEGER || vertex < 1 || vertex > position.nrow())
        stop("%s: triangle index outside its vertex table", label);
      const uint64_t corner = mesh.triVerts.size();
      mesh.triVerts.push_back(corner);
      for (int j = 0; j < 3; ++j) mesh.vertProperties.push_back(position(vertex - 1, j));
      for (int j = 0; j < 4; ++j) mesh.vertProperties.push_back(colour(i, j));
      if (first[vertex - 1] < 0) first[vertex - 1] = corner;
      else {
        mesh.mergeFromVert.push_back(corner);
        mesh.mergeToVert.push_back(first[vertex - 1]);
      }
    }
  }
  for (double x : mesh.vertProperties) if (!std::isfinite(x)) stop("%s: non-finite mesh property", label);
  return checked_solid(Manifold(mesh), label);
}

Manifold text_solid(const List& text, const std::string& label) {
  const int mode = as<int>(text["mode"]);
  const double depth = as<double>(text["depth"]), dilation = as<double>(text["dilation"]);
  if ((mode != 1 && mode != 2) || !std::isfinite(depth) || depth <= 0 ||
      !std::isfinite(dilation) || dilation < 0) stop("%s: invalid text dimensions", label);
  List contours = text["contours"];
  manifold::Polygons polygons;
  manifold::vec2 low(std::numeric_limits<double>::infinity()), high(-std::numeric_limits<double>::infinity());
  for (SEXP contour : contours) {
    DataFrame xy(contour);
    NumericVector x = xy[0], y = xy[1];
    if (x.size() != y.size() || x.size() < 3) stop("%s: invalid text contour", label);
    manifold::SimplePolygon polygon;
    for (R_xlen_t i = 0; i < x.size(); ++i) {
      if (!std::isfinite(x[i]) || !std::isfinite(y[i])) stop("%s: non-finite text contour", label);
      polygon.push_back({x[i], y[i]});
      low.x = std::min(low.x, x[i]); low.y = std::min(low.y, y[i]);
      high.x = std::max(high.x, x[i]); high.y = std::max(high.y, y[i]);
    }
    polygons.push_back(std::move(polygon));
  }
  if (polygons.empty()) stop("%s: text has no contours", label);
  auto section = CrossSection(polygons, CrossSection::FillRule::NonZero)
    .Offset(dilation, CrossSection::JoinType::Round, 2.0, 12);
  const double height = depth * (mode == 2 ? 2 : 1);
  Manifold glyph = Manifold::Extrude(section.ToPolygons(), height);
  low -= manifold::vec2(2 * dilation); high += manifold::vec2(2 * dilation);
  CrossSection border(manifold::SimplePolygon{{low.x, low.y}, {high.x, low.y},
                                             {high.x, high.y}, {low.x, high.y}});
  Manifold backing = coloured_solid(Manifold::Extrude(border.ToPolygons(), height)
    .Translate({0, 0, -height}), NumericVector::create(.85, .85, .85, 1));
  NumericVector rgba = text["colour"];
  Manifold solid = mode == 2 ? backing - coloured_solid(glyph.Translate({0, 0, -depth}), rgba)
                             : backing + coloured_solid(glyph, rgba);
  NumericMatrix transform = text["transform"];
  if (transform.nrow() != 3 || transform.ncol() != 4) stop("%s: invalid text transform", label);
  manifold::mat3x4 matrix;
  for (int j = 0; j < 4; ++j) for (int i = 0; i < 3; ++i) {
    if (!std::isfinite(transform(i, j))) stop("%s: non-finite text transform", label);
    matrix[j][i] = transform(i, j);
  }
  return checked_solid(solid.Transform(matrix), label);
}
}

// [[Rcpp::export]]
List print_union_cpp(List meshes, List texts) {
  struct Group { std::string name; std::vector<Manifold> adds, holes; };
  std::vector<Group> groups;
  std::map<std::string, size_t> indices;
  std::vector<std::string> labels;
  std::unordered_set<std::string> names;
  auto label_part = [&](const std::string& name) {
    if (names.insert(name).second) labels.push_back(name);
  };
  std::vector<Manifold> positives, engravers;
  for (SEXP value : meshes) {
    List part(value);
    const std::string name = as<std::string>(part["name"]);
    label_part(name);
    const int role = as<int>(part["role"]);
    if (role < 0 || role > 2) stop("%s: invalid solid role", name);
    auto solid = mesh_solid(part, name);
    if (role == 2) engravers.push_back(std::move(solid));
    else {
      auto entry = indices.emplace(name, groups.size());
      if (entry.second) groups.push_back({name, {}, {}});
      auto& group = groups[entry.first->second];
      (role == 1 ? group.holes : group.adds).push_back(std::move(solid));
    }
  }
  for (size_t i = 0; i < groups.size(); ++i) {
    auto& group = groups[i];
    auto solid = checked_solid(Manifold::BatchBoolean(group.adds, OpType::Add), group.name);
    if (!group.holes.empty()) {
      auto cavity = checked_solid(Manifold::BatchBoolean(group.holes, OpType::Add), group.name);
      auto outside = cavity - solid;
      if (outside.Status() != Manifold::Error::NoError || !outside.IsEmpty())
        stop("%s: negative-winding cavity extends outside its solid", group.name);
      solid = checked_solid(solid - cavity, group.name);
    }
    positives.push_back(std::move(solid));
  }
  for (SEXP value : texts) {
    List text(value);
    const std::string name = as<std::string>(text["name"]);
    label_part(name);
    positives.push_back(text_solid(text, name));
  }
  std::string label;
  for (const auto& name : labels) { if (!label.empty()) label += ", "; label += name; }
  auto result = checked_solid(Manifold::BatchBoolean(positives, OpType::Add), label);
  if (!engravers.empty()) result = checked_solid(result - Manifold::BatchBoolean(engravers, OpType::Add), label);
  const auto output = result.Translate(-result.BoundingBox().min).GetMeshGL64();
  if (output.numProp != 7 || output.NumVert() > INT_MAX || output.NumTri() > INT_MAX)
    stop("%s: invalid union dimensions", label);
  NumericMatrix position(output.NumVert(), 3), colour(output.NumVert(), 4);
  IntegerMatrix triangles(output.NumTri(), 3);
  for (int i = 0; i < position.nrow(); ++i) {
    for (int j = 0; j < 3; ++j) position(i, j) = output.vertProperties[i * 7ULL + j];
    for (int j = 0; j < 4; ++j) colour(i, j) = output.vertProperties[i * 7ULL + j + 3];
  }
  for (int i = 0; i < triangles.nrow(); ++i) for (int j = 0; j < 3; ++j)
    triangles(i, j) = output.triVerts[i * 3ULL + j] + 1;
  return List::create(_["position"] = position, _["colour"] = colour, _["triangles"] = triangles);
}

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
