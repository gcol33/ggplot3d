#include "raymarch/kernel.h"
#include "raymarch/volume.h"
using namespace ggplot3d::raymarch;

// [[Rcpp::export(rng = false)]]
Rcpp::CharacterVector raymarch_ops_cpp() {
  Rcpp::CharacterVector out;for(const auto& op:operations())out.push_back(op.name);return out;
}

// [[Rcpp::export(rng = false)]]
Rcpp::NumericMatrix raymarch_eval_cpp(Rcpp::List input,Rcpp::NumericMatrix points) {
  require(points.ncol()==3,"Points need three columns");Tape tape(input);std::vector<Jet> scratch;
  Rcpp::NumericMatrix out(points.nrow(),4);
  for(int i=0;i<points.nrow();++i) {Jet j=tape.evaluate({points(i,0),points(i,1),points(i,2)},scratch);
    out(i,0)=j.v;for(int k=0;k<3;++k)out(i,k+1)=j.d[k];}return out;
}

// [[Rcpp::export(rng = false)]]
Rcpp::List raymarch_trace_cpp(Rcpp::List input,Rcpp::NumericMatrix origins,Rcpp::NumericMatrix directions,
                             Rcpp::NumericVector bounds,double tolerance,int max_steps,int threads) {
  require(origins.ncol()==3 && directions.ncol()==3 && origins.nrow()==directions.nrow(),"Invalid ray matrices");
  require(bounds.size()==6 && tolerance>0 && std::isfinite(tolerance) && max_steps>0,"Invalid tracing controls");
  Tape tape(input);std::size_t n=origins.nrow();std::vector<Hit> hits(n);
  std::vector<Ray> rays(n);
  for(std::size_t i=0;i<n;++i) rays[i]={{origins(i,0),origins(i,1),origins(i,2)},unit({directions(i,0),directions(i,1),directions(i,2)}),0,std::numeric_limits<double>::infinity()};
  Vec3 lo{bounds[0],bounds[2],bounds[4]},hi{bounds[1],bounds[3],bounds[5]};
  ggplot3d::engine::parallel_blocks((n+255)/256,threads==0 ? std::max(1u,std::thread::hardware_concurrency()):threads,[&](std::size_t block){
    std::vector<Jet> scratch;for(std::size_t i=block*256;i<std::min(n,(block+1)*256);++i)
      if(clip_box(rays[i],lo,hi))hits[i]=trace(tape,rays[i],tolerance,max_steps,scratch);
  });
  Rcpp::NumericVector depth(n,NA_REAL);Rcpp::NumericMatrix normals(n,3);Rcpp::IntegerVector steps(n);
  for(std::size_t i=0;i<n;++i) {steps[i]=hits[i].steps;if(hits[i].found){depth[i]=hits[i].t;for(int k=0;k<3;++k)normals(i,k)=hits[i].normal[k];}}
  return Rcpp::List::create(Rcpp::Named("distance")=depth,Rcpp::Named("normal")=normals,Rcpp::Named("steps")=steps);
}

// [[Rcpp::export(rng = false)]]
Rcpp::List raymarch_rays_cpp(Rcpp::List camera,Rcpp::List viewport,int width,int height,Rcpp::NumericVector extent) {
  require(width>0 && height>0 && extent.size()==4,"Invalid image dimensions");
  ggplot3d::Viewport v{Rcpp::as<double>(viewport["width_mm"]),Rcpp::as<double>(viewport["height_mm"]),Rcpp::as<double>(viewport["dpi"])};
  ggplot3d::engine::Projection p(ggplot3d::engine::read_camera(camera),v);
  const auto& c=p.resolved;int n=width*height;Rcpp::NumericMatrix origins(n,3),directions(n,3);
  for(int x=0;x<width;++x) for(int y=0;y<height;++y) {
    int i=y+height*x;double u=(2*(extent[0]+(x+0.5)*(extent[2]-extent[0])/width)-1)/p.sx;
    double w=(2*(extent[3]-(y+0.5)*(extent[3]-extent[1])/height)-1)/p.sy;
    for(int k=0;k<3;++k) {
      double d=c.ray[k]+(p.camera.perspective ? u*c.right[k]+w*c.up[k]:0);
      double o=c.eye[k]+(p.camera.perspective ? 0:u*c.right[k]+w*c.up[k]);
      origins(i,k)=(o+c.near*d)/p.scale[k];directions(i,k)=d/p.scale[k];
    }
  }
  return Rcpp::List::create(Rcpp::Named("origin")=origins,Rcpp::Named("direction")=directions,
    Rcpp::Named("near")=c.near,Rcpp::Named("far")=c.far);
}

// [[Rcpp::export(rng = false)]]
Rcpp::List raymarch_volume_cpp(Rcpp::List inputs,Rcpp::NumericMatrix origins,Rcpp::NumericMatrix directions,
                              Rcpp::NumericVector limit,int threads) {
  require(origins.ncol()==3 && directions.ncol()==3 && origins.nrow()==directions.nrow() && limit.size()==origins.nrow(),"Invalid volume rays");
  std::vector<Volume> volumes;for(SEXP input:inputs)volumes.emplace_back(Rcpp::as<Rcpp::List>(input));
  require(!volumes.empty(),"No volumes supplied");
  bool absorption=volumes[0].mode==0;for(const auto& v:volumes)require((v.mode==0)==absorption,"Separate surface and absorption passes");
  std::size_t n=origins.nrow();std::vector<Ray> rays(n);std::vector<Medium> media(n);std::vector<VolumeHit> hits(n);std::vector<Vec3> rgb(n);
  for(std::size_t i=0;i<n;++i)rays[i]={{origins(i,0),origins(i,1),origins(i,2)},unit({directions(i,0),directions(i,1),directions(i,2)}),0,limit[i]};
  ggplot3d::engine::parallel_blocks((n+127)/128,threads==0 ? std::max(1u,std::thread::hardware_concurrency()):threads,[&](std::size_t block){
    for(std::size_t i=block*128;i<std::min(n,(block+1)*128);++i) {
      if(absorption)media[i]=absorb(volumes,rays[i]);
      else for(const auto& v:volumes) {
        auto h=volume_surface(v,rays[i]);if(h.hit.found && (!hits[i].hit.found || h.hit.t<hits[i].hit.t)) {hits[i]=h;rgb[i]=v.transfer(h.value).first;}
      }
    }
  });
  Rcpp::NumericVector distance(n,NA_REAL),alpha(n),value(n,NA_REAL);Rcpp::NumericMatrix colour(n,3),normal(n,3);
  for(std::size_t i=0;i<n;++i) {
    if(absorption) {alpha[i]=media[i].alpha;if(std::isfinite(media[i].entry))distance[i]=media[i].entry;for(int k=0;k<3;++k)colour(i,k)=media[i].colour[k];}
    else if(hits[i].hit.found){distance[i]=hits[i].hit.t;alpha[i]=1;value[i]=hits[i].value;for(int k=0;k<3;++k){colour(i,k)=rgb[i][k];normal(i,k)=hits[i].hit.normal[k];}}
  }
  return Rcpp::List::create(Rcpp::Named("distance")=distance,Rcpp::Named("normal")=normal,Rcpp::Named("colour")=colour,Rcpp::Named("alpha")=alpha,Rcpp::Named("value")=value);
}
