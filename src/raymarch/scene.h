#pragma once
#include "kernel.h"
#include "../shade/read.h"
#include "../raster/texture.h"

namespace ggplot3d { namespace raymarch {
inline Vec3 basis_sample(const Vec3& axis,double x,double y,double z) {
  Vec3 tangent=unit(engine::cross(std::abs(axis[2])<.9 ? Vec3{0,0,1}:Vec3{1,0,0},axis));
  Vec3 bitangent=engine::cross(axis,tangent),out;
  for(int k=0;k<3;++k)out[k]=x*tangent[k]+y*bitangent[k]+z*axis[k];
  return out;
}
struct SurfaceHit {bool found=false;int illustrative=0,triangle=-1;double t=0,alpha=1;Vec3 normal{0,0,0},colour{0,0,0};std::array<double,2> uv{};};
struct Triangle {
  std::array<Vec3,3> p,n,c;std::array<double,3> alpha;int illustrative=0,texture=0;double fill_alpha=1;
  std::array<std::array<double,2>,3> uv{};
  SurfaceHit intersect(const Ray& ray) const {
    SurfaceHit out;Vec3 e1=engine::subtract(p[1],p[0]),e2=engine::subtract(p[2],p[0]);
    Vec3 h=engine::cross(ray.direction,e2);double det=engine::dot(e1,h);
    if(std::abs(det)<=1e-14*norm(e1)*norm(e2))return out;
    Vec3 s=engine::subtract(ray.origin,p[0]);double u=engine::dot(s,h)/det;if(u<0 || u>1)return out;
    Vec3 q=engine::cross(s,e1);double v=engine::dot(ray.direction,q)/det;if(v<0 || u+v>1)return out;
    double t=engine::dot(e2,q)/det;if(t<ray.near || t>ray.far)return out;
    out.found=true;out.t=t;out.illustrative=illustrative;double weights[3]={1-u-v,u,v};out.alpha=0;
    for(int i=0;i<3;++i) {out.alpha+=weights[i]*alpha[i];for(int k=0;k<3;++k){out.colour[k]+=weights[i]*c[i][k];out.normal[k]+=weights[i]*n[i][k];}}
    for(int i=0;i<3;++i) for(int k=0;k<2;++k) out.uv[k]+=weights[i]*uv[i][k];
    out.normal=unit(out.normal);return out;
  }
};
struct Implicit {
  Tape tape;Vec3 low,span,scale,colour;double tolerance;int steps;
  Implicit(const Rcpp::List& input,const Vec3& model):tape(Rcpp::as<Rcpp::List>(input["tape"])),scale(model) {
    Rcpp::List ranges=input["ranges"];Rcpp::NumericVector rgb=input["rgb"];
    for(int k=0;k<3;++k){Rcpp::NumericVector r=ranges[k];low[k]=r[0];span[k]=r[1]-r[0];colour[k]=rgb[k];}
    tolerance=Rcpp::as<double>(input["tolerance"]);steps=Rcpp::as<int>(input["max_steps"]);
  }
  SurfaceHit intersect(const Ray& ray,std::vector<Jet>& scratch) const {
    Ray local;Vec3 high;
    for(int k=0;k<3;++k){local.origin[k]=low[k]+ray.origin[k]*span[k]/scale[k];local.direction[k]=ray.direction[k]*span[k]/scale[k];high[k]=low[k]+span[k];}
    double length=norm(local.direction);local.direction=unit(local.direction);local.near=ray.near*length;local.far=ray.far*length;
    SurfaceHit out;if(!clip_box(local,low,high))return out;
    Hit h=trace(tape,local,tolerance,steps,scratch);if(!h.found)return out;
    out.found=true;out.t=h.t/length;out.colour=colour;
    for(int k=0;k<3;++k)out.normal[k]=h.normal[k]*span[k]/scale[k];
    out.normal=unit(out.normal);return out;
  }
};
struct Bounds {
  Vec3 lo{INFINITY,INFINITY,INFINITY},hi{-INFINITY,-INFINITY,-INFINITY};
  void add(const Vec3& p){for(int k=0;k<3;++k){lo[k]=std::min(lo[k],p[k]);hi[k]=std::max(hi[k],p[k]);}}
  void add(const Bounds& b){add(b.lo);add(b.hi);}
  double area()const{Vec3 d=engine::subtract(hi,lo);return 2*(d[0]*d[1]+d[1]*d[2]+d[2]*d[0]);}
  bool intersects(const Ray& r)const{Ray clipped=r;return clip_box(clipped,lo,hi);}
};
struct Object {Bounds bounds;int kind,index;double centre(int k)const{return (bounds.lo[k]+bounds.hi[k])/2;}};
struct BvhNode {Bounds bounds;int begin=0,end=0,left=-1,right=-1;};
struct TraceScene {
  std::vector<Triangle> triangles;std::vector<Implicit> implicit;std::vector<Object> objects;std::vector<BvhNode> nodes;
  Vec3 model_scale;bool clip;
  std::vector<shade::Material> materials;
  shade::Lights lights;
  std::vector<raster::Texture> textures;
  TraceScene(const Rcpp::List& mesh,const Rcpp::List& fields,const Vec3& scale,bool clip_data):model_scale(scale),clip(clip_data) {
    Rcpp::NumericMatrix p=mesh["position"],n=mesh["normal"],c=mesh["colour"];Rcpp::IntegerMatrix index=mesh["triangles"];
    Rcpp::IntegerVector illustrative(index.ncol());
    Rcpp::IntegerVector texture(index.ncol());
    Rcpp::NumericVector fill_alpha(index.ncol(),1.0);
    Rcpp::NumericMatrix uv(p.nrow(),2);
    if(mesh.containsElementNamed("textures")) {
      Rcpp::List images=mesh["textures"];Rcpp::LogicalVector interpolate=mesh["interpolate"];
      require(interpolate.size()==images.size(),"Invalid ambient texture interpolation");
      for(R_xlen_t i=0;i<images.size();++i) textures.push_back(raster::read_texture(images[i],interpolate[i]==TRUE));
      texture=Rcpp::as<Rcpp::IntegerVector>(mesh["texture"]);
      fill_alpha=Rcpp::as<Rcpp::NumericVector>(mesh["fill_alpha"]);
      uv=Rcpp::as<Rcpp::NumericMatrix>(mesh["uv"]);
      require(texture.size()==index.ncol() && fill_alpha.size()==index.ncol() && uv.nrow()==p.nrow() && uv.ncol()==2,
        "Invalid ambient texture coordinates");
    }
    if(mesh.containsElementNamed("illustrative")) illustrative=Rcpp::as<Rcpp::IntegerVector>(mesh["illustrative"]);
    if(mesh.containsElementNamed("materials")) materials=shade::read_materials(Rcpp::as<Rcpp::List>(mesh["materials"]));
    if(mesh.containsElementNamed("lights")) lights=shade::read_lights(Rcpp::as<Rcpp::List>(mesh["lights"]));
    require(illustrative.size()==index.ncol(),"Invalid illustrative material indices");
    require(p.ncol()==3 && n.ncol()==3 && c.ncol()==4 && p.nrow()==n.nrow() && p.nrow()==c.nrow() && index.nrow()==3,"Invalid path-trace mesh");
    for(int i=0;i<index.ncol();++i){Triangle t;Object o;o.kind=0;o.index=i;
      t.illustrative=illustrative[i];
      t.texture=texture[i];t.fill_alpha=fill_alpha[i];
      require(t.texture>=0 && t.texture<=int(textures.size()),"Invalid ambient texture index");
      require(t.illustrative>=0 && (t.illustrative==0 || t.illustrative<int(materials.size())),"Invalid illustrative material");
      for(int j=0;j<3;++j){int row=index(j,i)-1;require(row>=0 && row<p.nrow(),"Invalid triangle index");
        for(int k=0;k<3;++k){t.p[j][k]=p(row,k);t.n[j][k]=n(row,k);t.c[j][k]=c(row,k);}t.alpha[j]=c(row,3);
        for(int k=0;k<2;++k)t.uv[j][k]=uv(row,k);o.bounds.add(t.p[j]);}
      triangles.push_back(t);objects.push_back(o);
    }
    for(SEXP field:fields){Object o;o.kind=1;o.index=implicit.size();o.bounds.lo={0,0,0};o.bounds.hi=scale;implicit.emplace_back(Rcpp::as<Rcpp::List>(field),scale);objects.push_back(o);}
    if(!objects.empty())build(0,objects.size());
  }
  int build(int begin,int end) {
    BvhNode node;node.begin=begin;node.end=end;for(int i=begin;i<end;++i)node.bounds.add(objects[i].bounds);
    int id=nodes.size();nodes.push_back(node);if(end-begin<=4)return id;
    double best=INFINITY;int axis=-1,split=-1;Bounds centroids;for(int i=begin;i<end;++i)centroids.add(Vec3{objects[i].centre(0),objects[i].centre(1),objects[i].centre(2)});
    constexpr int bins=12;
    for(int k=0;k<3;++k) {
      double range=centroids.hi[k]-centroids.lo[k];if(range==0)continue;
      std::array<Bounds,bins> bounds;std::array<int,bins> count{};
      for(int i=begin;i<end;++i){int bin=std::min(bins-1,int(bins*(objects[i].centre(k)-centroids.lo[k])/range));bounds[bin].add(objects[i].bounds);++count[bin];}
      for(int s=0;s<bins-1;++s){Bounds a,b;int na=0,nb=0;for(int j=0;j<bins;++j)if(count[j]){if(j<=s){a.add(bounds[j]);na+=count[j];}else{b.add(bounds[j]);nb+=count[j];}}
        double cost=na && nb ? na*a.area()+nb*b.area():INFINITY;if(cost<best){best=cost;axis=k;split=s;}}
    }
    int middle=(begin+end)/2;
    if(axis>=0){double threshold=centroids.lo[axis]+(centroids.hi[axis]-centroids.lo[axis])*(split+1)/bins;
      middle=std::stable_partition(objects.begin()+begin,objects.begin()+end,[&](const Object& o){return o.centre(axis)<threshold;})-objects.begin();}
    if(middle==begin || middle==end)middle=(begin+end)/2;
    int left=build(begin,middle),right=build(middle,end);nodes[id].left=left;nodes[id].right=right;return id;
  }
  SurfaceHit intersect(Ray ray,std::vector<Jet>& scratch) const {
    SurfaceHit hit;if(nodes.empty() || (clip && !clip_box(ray,{0,0,0},model_scale)))return hit;
    std::vector<int> stack{0};
    while(!stack.empty()) {int id=stack.back();stack.pop_back();const auto& node=nodes[id];if(!node.bounds.intersects(ray))continue;
      if(node.left>=0){stack.push_back(node.right);stack.push_back(node.left);continue;}
      for(int i=node.begin;i<node.end;++i){const auto& o=objects[i];SurfaceHit h=o.kind==0 ? triangles[o.index].intersect(ray):implicit[o.index].intersect(ray,scratch);
        if(h.found && o.kind==0 && triangles[o.index].texture>0) {
          const auto& t=triangles[o.index];double a=textures[t.texture-1].sample(h.uv[0],h.uv[1],1)[3];
          h.alpha*=a+(1-a)*t.fill_alpha;
        }
        if(h.found && (!hit.found || h.t<hit.t)){h.triangle=o.kind==0 ? o.index:-1;hit=h;ray.far=h.t;}}
    }
    return hit;
  }
  double visibility(Ray ray,std::vector<Jet>& scratch,double epsilon) const {
    double transmission=1;
    while(transmission>1e-8){SurfaceHit h=intersect(ray,scratch);if(!h.found)return transmission;transmission*=1-h.alpha;
      ray.origin=point(ray.origin,ray.direction,h.t+epsilon);ray.far-=h.t+epsilon;ray.near=0;if(ray.far<=0)break;}
    return transmission;
  }
};
} }
