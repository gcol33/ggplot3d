#pragma once
#include "../raymarch/scene.h"
#include <map>

// Hemisphere accessibility (Pharr and Green, GPU Gems, chapter 17), with
// uniform solid-angle samples and alpha transmission through the mesh BVH.
namespace ggplot3d { namespace ambient {
using engine::Vec3;
struct Context {
  raymarch::TraceScene scene;
  int samples;
  double radius, amount, epsilon;
  std::map<std::array<double, 6>, double> cache;
  Context(const Rcpp::List& mesh, int n, double r, double a, double e)
    : scene(mesh, Rcpp::List(), {1,1,1}, false), samples(n), radius(r), amount(a), epsilon(e) {
    engine::require(n > 0 && std::isfinite(r) && r > 0 && std::isfinite(a) && a >= 0 && a <= 1 &&
      std::isfinite(e) && e > 0, "Invalid ambient occlusion controls");
  }
  double visibility(Vec3 p, Vec3 n) {
    if (scene.nodes.empty() || !std::isfinite(p[0]+p[1]+p[2]+n[0]+n[1]+n[2]) || engine::dot(n,n) == 0) return 1;
    n = raymarch::unit(n);
    const std::array<double, 6> key{p[0],p[1],p[2],n[0],n[1],n[2]};
    const auto found = cache.find(key);
    if (found != cache.end()) return found->second;
    Vec3 origin = raymarch::point(p,n,epsilon);
    std::vector<raymarch::Jet> scratch;
    const Vec3 inward{-n[0],-n[1],-n[2]};
    const auto surface=scene.intersect({origin,inward,0,4*epsilon},scratch);
    if(surface.found && surface.triangle>=0) {
      const auto& triangle=scene.triangles[surface.triangle];
      Vec3 centre;
      for(int k=0;k<3;++k) centre[k]=(triangle.p[0][k]+triangle.p[1][k]+triangle.p[2][k])/3;
      origin=raymarch::point(origin,raymarch::unit(engine::subtract(centre,p)),epsilon);
    }
    const double golden = (std::sqrt(5.0)-1)/2;
    double sum = 0;
    for (int i=0;i<samples;++i) {
      const double z=(i+0.5)/samples, phi=2*engine::pi*golden*i, r=std::sqrt(1-z*z);
      const Vec3 direction=raymarch::basis_sample(n,r*std::cos(phi),r*std::sin(phi),z);
      sum += scene.visibility({origin,direction,0,radius},scratch,epsilon);
    }
    const double value = 1-amount*(1-sum/samples);
    cache.emplace(key,value);
    return value;
  }
};
} }
