#include "raymarch/kernel.h"
#include "raymarch/volume.h"
#include "raymarch/path.h"
#include "raymarch/thurston-scene.h"
using namespace ggplot3d::raymarch;

// [[Rcpp::export(rng = false)]]
Rcpp::List raymarch_geodesic_cpp(std::string type, Rcpp::NumericVector origin, Rcpp::NumericVector direction,
                                 Rcpp::NumericVector times, bool transport = true) {
  require(origin.size() == 3 && direction.size() == 3, "Geodesics need three-coordinate points and tangents");
  Geometry g = geometry(type);
  Geodesic ray(g, {origin[0], origin[1], origin[2]}, {direction[0], direction[1], direction[2]});
  Rcpp::NumericMatrix position(times.size(), 3), velocity(times.size(), 3),
      coordinate_velocity(times.size(), 3), Q(times.size(), 9);
  Rcpp::NumericVector area(times.size());
  for (int i = 0; i < times.size(); ++i) {
    Flow f = ray.at(times[i], transport);
    Vec3 v = mv(frame(g, f.p), f.u);
    for (int j = 0; j < 3; ++j) {
      position(i, j) = f.p[j];
      velocity(i, j) = f.u[j];
      coordinate_velocity(i, j) = v[j];
      for (int k = 0; k < 3; ++k)
        Q(i, j + 3 * k) = f.transport[j][k];
    }
    if (transport)
      area[i] = area_density(ray, times[i]);
  }
  return Rcpp::List::create(Rcpp::Named("position") = position, Rcpp::Named("velocity") = velocity,
                            Rcpp::Named("coordinate_velocity") = coordinate_velocity,
                            Rcpp::Named("transport") = Q, Rcpp::Named("area") = area);
}

// [[Rcpp::export(rng = false)]]
Rcpp::List raymarch_curved_cpp(Rcpp::List inputs, Rcpp::NumericMatrix directions, Rcpp::List settings,
                               int threads) {
  require(directions.ncol() == 3, "Intrinsic rays need three tangent components");
  Geometry g = geometry(Rcpp::as<std::string>(settings["geometry"]));
  Rcpp::NumericVector o = settings["observer"];
  Vec3 origin{o[0], o[1], o[2]};
  double far = Rcpp::as<double>(settings["max_distance"]),
         tolerance = Rcpp::as<double>(settings["tolerance"]);
  int steps = Rcpp::as<int>(settings["max_steps"]);
  std::vector<GeometricField> fields;
  for (SEXP input : inputs)
    fields.emplace_back(Rcpp::as<Rcpp::List>(input));
  std::vector<Portal> portals;
  Rcpp::List domain = settings["quotient"];
  for (SEXP face : domain)
    portals.emplace_back(Rcpp::as<Rcpp::List>(face));
  for (const auto &portal : portals)
    require(portal.value(g, origin) < -tolerance, "The observer must be inside the fundamental domain");
  std::size_t n = directions.nrow();
  std::vector<Vec3> rays(n);
  std::vector<CurvedHit> hits(n);
  for (std::size_t i = 0; i < n; ++i)
    rays[i] = unit({directions(i, 0), directions(i, 1), directions(i, 2)});
  ggplot3d::engine::parallel_blocks(
      (n + 31) / 32, threads == 0 ? std::max(1u, std::thread::hardware_concurrency()) : threads,
      [&](std::size_t block) {
        for (std::size_t i = block * 32; i < std::min(n, (block + 1) * 32); ++i)
          hits[i] = curved_trace(g, origin, rays[i], fields, portals, far, tolerance, steps);
      });
  Rcpp::NumericVector distance(n, NA_REAL), area(n);
  Rcpp::IntegerVector count(n), crossings(n), object(n, NA_INTEGER);
  Rcpp::NumericMatrix normal(n, 3), position(n, 3), colour(n, 3), incoming(n, 3);
  for (std::size_t i = 0; i < n; ++i) {
    const auto &h = hits[i];
    count[i] = h.hit.steps;
    crossings[i] = h.portals;
    if (h.hit.found) {
      distance[i] = h.hit.t;
      area[i] = h.area;
      object[i] = h.object + 1;
      for (int j = 0; j < 3; ++j) {
        normal(i, j) = h.hit.normal[j];
        position(i, j) = h.position[j];
        colour(i, j) = h.colour[j];
        incoming(i, j) = h.incoming[j];
      }
    }
  }
  return Rcpp::List::create(
      Rcpp::Named("distance") = distance, Rcpp::Named("area") = area, Rcpp::Named("normal") = normal,
      Rcpp::Named("position") = position, Rcpp::Named("colour") = colour, Rcpp::Named("incoming") = incoming,
      Rcpp::Named("steps") = count, Rcpp::Named("crossings") = crossings, Rcpp::Named("object") = object);
}

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
    int i=y+height*x;
    Ray ray=camera_ray(p,extent[0]+(x+0.5)*(extent[2]-extent[0])/width,extent[3]-(y+0.5)*(extent[3]-extent[1])/height);
    Vec3 origin=point(ray.origin,ray.direction,ray.near);
    double forward=ggplot3d::engine::dot(ray.direction,c.forward);
    for(int k=0;k<3;++k) {
      origins(i,k)=origin[k]/p.scale[k];directions(i,k)=ray.direction[k]/(forward*p.scale[k]);
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

struct PathContext {
  ggplot3d::engine::Projection projection;
  TraceScene scene;
  PathContext(Rcpp::List mesh,Rcpp::List fields,Rcpp::List camera,Rcpp::List viewport):
    projection(ggplot3d::engine::read_camera(camera),ggplot3d::Viewport{Rcpp::as<double>(viewport["width_mm"]),Rcpp::as<double>(viewport["height_mm"]),Rcpp::as<double>(viewport["dpi"])}),
    scene(mesh,fields,projection.scale,projection.camera.clip_data){}
};

// [[Rcpp::export(rng = false)]]
SEXP raymarch_path_prepare_cpp(Rcpp::List mesh,Rcpp::List fields,Rcpp::List camera,Rcpp::List viewport) {
  return Rcpp::XPtr<PathContext>(new PathContext(mesh,fields,camera,viewport),true);
}

// [[Rcpp::export(rng = false)]]
Rcpp::NumericMatrix raymarch_path_cpp(SEXP pointer,int width,int height,Rcpp::NumericVector extent,
  Rcpp::List settings,Rcpp::NumericVector illumination,int start,int samples,Rcpp::NumericMatrix previous,int threads) {
  Rcpp::XPtr<PathContext> ctx(pointer);require(ctx.get()!=nullptr,"Invalid path-trace scene");
  require(width>0 && height>0 && double(width)*height<=std::numeric_limits<int>::max() && extent.size()==4,"Invalid path-trace dimensions");
  std::size_t n=std::size_t(width)*height;
  require(previous.nrow()==int(n) && previous.ncol()==4 && start>=0 && samples>0 && double(start)+samples<=std::numeric_limits<int>::max(),"Invalid progressive accumulation");
  require(illumination.size()==5,"Invalid path-trace lighting");
  int bounces=Rcpp::as<int>(settings["bounces"]);double aperture=Rcpp::as<double>(settings["aperture"]),focus=Rcpp::as<double>(settings["focus"]);
  double epsilon=Rcpp::as<double>(settings["epsilon"]),angle=Rcpp::as<double>(settings["light_radius"])*ggplot3d::engine::pi/180;
  require(bounces>0 && aperture>=0 && focus>0 && epsilon>0 && angle>=0 && angle<ggplot3d::engine::pi/2,"Invalid path-trace controls");
  std::uint64_t seed=Rcpp::as<double>(settings["seed"]);
  PathLight light{unit({illumination[0],illumination[1],illumination[2]}),illumination[3],illumination[4],std::cos(angle)};
  std::vector<std::array<double,4>> sums(n);for(std::size_t i=0;i<n;++i)for(int k=0;k<4;++k)sums[i][k]=previous(i,k);
  ggplot3d::engine::parallel_blocks((n+63)/64,threads==0 ? std::max(1u,std::thread::hardware_concurrency()):threads,[&](std::size_t block){
    for(std::size_t i=block*64;i<std::min(n,(block+1)*64);++i) {
      int x=i/height,y=i%height;
      for(int s=start;s<start+samples;++s){Sampler rng(seed,i,s);
        double px=extent[0]+(x+rng.next())*(extent[2]-extent[0])/width,py=extent[3]-(y+rng.next())*(extent[3]-extent[1])/height;
        Ray ray=lens_ray(ctx->projection,px,py,aperture,focus,rng);
        auto value=path_sample(ctx->scene,ray,light,bounces,rng,epsilon);for(int k=0;k<4;++k)sums[i][k]+=value[k];
      }
    }
  });
  Rcpp::NumericMatrix out(n,4);for(std::size_t i=0;i<n;++i)for(int k=0;k<4;++k)out(i,k)=sums[i][k];return out;
}
