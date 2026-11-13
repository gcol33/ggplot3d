#pragma once
#include "coverage.h"
#include <map>
#include <queue>
#include <tuple>

namespace ggplot3d { namespace engine {
using Point2 = std::array<double,2>;
using Ring2 = std::vector<Point2>;

static inline long double orient2(const Point2& a, const Point2& b, const Point2& c) {
  return (static_cast<long double>(b[0])-a[0])*(static_cast<long double>(c[1])-a[1]) -
    (static_cast<long double>(b[1])-a[1])*(static_cast<long double>(c[0])-a[0]);
}
static inline Point2 mix2(const Point2& a, const Point2& b, long double t) {
  return {static_cast<double>(a[0]+t*(b[0]-a[0])),
          static_cast<double>(a[1]+t*(b[1]-a[1]))};
}
static inline long double ring_area(const Ring2& ring) {
  long double area=0;
  for(std::size_t i=1;i+1<ring.size();++i) area+=orient2(ring[0],ring[i],ring[i+1]);
  return area;
}
static inline Ring2 convex_overlap(Ring2 subject,const Ring2& clip) {
  for(std::size_t i=0;i<clip.size() && !subject.empty();++i) {
    const auto& a=clip[i];const auto& b=clip[(i+1)%clip.size()];
    Ring2 next; next.reserve(subject.size()+1);
    Point2 previous=subject.back();long double dp=orient2(a,b,previous);
    for(const auto& current:subject) {
      long double dc=orient2(a,b,current);
      if((dp<0 && dc>0) || (dp>0 && dc<0)) next.push_back(mix2(previous,current,dp/(dp-dc)));
      if(dc>=0) next.push_back(current);
      previous=current;dp=dc;
    }
    subject=std::move(next);
  }
  return subject;
}

struct ProjectedPiece {
  Ring2 ring;
  Bounds2 bounds{R_PosInf,R_PosInf,R_NegInf,R_NegInf};
  // Depth, or reciprocal depth in perspective, is affine on a projected plane.
  std::array<long double,3> field{};
  std::array<long double,3> ray_normal{};
  double first_depth=0,last_depth=0;
  bool area=false;
};

static inline ProjectedPiece project_piece(const Piece& piece,const Projection& p) {
  ProjectedPiece out;
  for(const Vertex& v:piece.polygon) {
    auto c=p.camera_point(v.xyz);
    double w=p.camera.perspective ? c[2] : 1;
    Point2 q{c[0]/w,c[1]/w};out.ring.push_back(q);
    for(int k=0;k<2;++k) {out.bounds[k]=std::min(out.bounds[k],q[k]);out.bounds[k+2]=std::max(out.bounds[k+2],q[k]);}
  }
  out.first_depth=p.camera_point(piece.polygon.front().xyz)[2];
  out.last_depth=p.camera_point(piece.polygon.back().xyz)[2];
  if(!piece.members.empty()) return out;
  if(out.ring.size()>=3) {
    long double area=ring_area(out.ring);
    out.area=area!=0;
    auto plane=supporting_plane(piece,p);
    long double nx=dot(plane.normal,p.resolved.right), ny=dot(plane.normal,p.resolved.up);
    long double nz=dot(plane.normal,p.resolved.ray);
    out.ray_normal={nx,ny,nz};
    long double d=dot(plane.normal,subtract(plane.origin,p.resolved.eye));
    if(p.camera.perspective && d!=0) out.field={nx/d,ny/d,nz/d};
    else if(!p.camera.perspective && nz!=0) out.field={-nx/nz,-ny/nz,d/nz};
    else out.area=false;
    if(area<0) std::reverse(out.ring.begin(),out.ring.end());
  }
  return out;
}
static inline double projected_depth(const ProjectedPiece& piece,const Point2& q,bool perspective) {
  long double value;
  if(piece.ring.size()<3) {
    double t=0;
    if(piece.ring.size()==2) {
      auto a=piece.ring.front(),b=piece.ring.back();
      double dx=b[0]-a[0],dy=b[1]-a[1];
      double norm=dx*dx+dy*dy;
      if(norm>0) t=((q[0]-a[0])*dx+(q[1]-a[1])*dy)/norm;
    }
    value=perspective ? (1-t)/piece.first_depth+t/piece.last_depth :
      (1-t)*piece.first_depth+t*piece.last_depth;
  } else value=piece.field[0]*q[0]+piece.field[1]*q[1]+piece.field[2];
  return static_cast<double>(perspective ? 1/value : value);
}
static inline Ring2 segment_overlap(const Ring2& segment,const Ring2& polygon) {
  Point2 a=segment.front(),b=segment.back();
  long double first=0,last=1;
  for(std::size_t i=0;i<polygon.size();++i) {
    long double da=orient2(polygon[i],polygon[(i+1)%polygon.size()],a);
    long double db=orient2(polygon[i],polygon[(i+1)%polygon.size()],b);
    if(da<0 && db<0) return {};
    if(da<0) first=std::max(first,da/(da-db));
    if(db<0) last=std::min(last,da/(da-db));
  }
  if(first>last) return {};
  return {mix2(a,b,first),mix2(a,b,last)};
}
static inline Ring2 projected_overlap(const ProjectedPiece& a,const ProjectedPiece& b,
                                     const Projection& p) {
  if(a.ring.size()>=3 && !a.area) return {};
  if(b.ring.size()>=3 && !b.area) return {};
  if(a.area && b.area) {
    Ring2 overlap=convex_overlap(a.ring,b.ring);
    double perimeter=0;
    for(std::size_t i=0;i<overlap.size();++i) {
      auto x=overlap[i],y=overlap[(i+1)%overlap.size()];
      perimeter+=std::hypot(x[0]-y[0],x[1]-y[1]);
    }
    double scale=p.camera.perspective ? std::max({a.first_depth,a.last_depth,b.first_depth,b.last_depth}) : 1;
    if(overlap.size()<3 || ring_area(overlap)<=p.resolved.tolerance*perimeter/scale) return {};
    return overlap;
  }
  if(b.area) return segment_overlap(a.ring,b.ring);
  if(a.area) return segment_overlap(b.ring,a.ring);
  Point2 x=a.ring.front(),y=a.ring.back(),u=b.ring.front(),v=b.ring.back();
  long double d=orient2({0,0},{y[0]-x[0],y[1]-x[1]},{v[0]-u[0],v[1]-u[1]});
  if(d!=0) {
    long double t=orient2(x,u,v)/d, s=-orient2(x,y,u)/d;
    if(t>=0 && t<=1 && s>=0 && s<=1) return {mix2(x,y,t)};
    return {};
  }
  if(orient2(x,y,u)!=0 || orient2(u,v,x)!=0) return {};
  Ring2 result;
  for(auto q:{x,y,u,v}) if(q[0]>=std::max(a.bounds[0],b.bounds[0]) && q[0]<=std::min(a.bounds[2],b.bounds[2]) &&
      q[1]>=std::max(a.bounds[1],b.bounds[1]) && q[1]<=std::min(a.bounds[3],b.bounds[3])) result.push_back(q);
  return result;
}

static inline bool inside_convex(const Ring2& ring,const Point2& q) {
  for(std::size_t i=0;i<ring.size();++i)
    if(orient2(ring[i],ring[(i+1)%ring.size()],q)<0) return false;
  return true;
}

static inline Ring2 glyph_overlap(const Ring2& overlap,const Piece& glyph,
                                  const ProjectedPiece& other,const ScenePacket& packet,
                                  const Projection& p) {
  if(glyph.kind!=Kind::marker || overlap.size()<3) return overlap;
  const auto& f=packet.footprints;Index id=packet.markers.footprint_id[glyph.row];
  auto anchor=p.npc(p.camera_point(glyph.anchor[0].xyz));
  double angle=std::remainder(f.angle[id],360.0)*pi/180,co=std::cos(angle),si=std::sin(angle);
  auto local=[&](const Point2& q) -> Point2 {
    double x=(.5+.5*p.sx*q[0]-anchor[0])*p.viewport.width_mm;
    double y=(.5+.5*p.sy*q[1]-anchor[1])*p.viewport.height_mm;
    return {co*x+si*y,-si*x+co*y};
  };
  auto screen=[&](const Point2& q) -> Point2 {
    return {2*(anchor[0]+(co*q[0]-si*q[1])/p.viewport.width_mm-.5)/p.sx,
            2*(anchor[1]+(si*q[0]+co*q[1])/p.viewport.height_mm-.5)/p.sy};
  };
  Ring2 cell;for(auto q:overlap) cell.push_back(local(q));
  Ring2 samples;
  auto add=[&](Point2 q) {if(inside_convex(cell,q)) samples.push_back(screen(q));};
  if(f.kind[id]==static_cast<Index>(FootprintKind::ellipse)) {
    double rx=f.radius_x_mm[id],ry=f.radius_y_mm[id];
    if(rx==0 || ry==0) return {};
    for(auto q:cell) if(q[0]*q[0]/(rx*rx)+q[1]*q[1]/(ry*ry)<=1) samples.push_back(screen(q));
    for(std::size_t i=0;i<cell.size();++i) {
      auto a=cell[i],b=cell[(i+1)%cell.size()];
      long double x=a[0]/rx,y=a[1]/ry,dx=(b[0]-a[0])/rx,dy=(b[1]-a[1])/ry;
      long double aa=dx*dx+dy*dy,bb=x*dx+y*dy,cc=x*x+y*y-1;
      long double discriminant=bb*bb-aa*cc;
      if(aa>0 && discriminant>=0) for(int sign:{-1,1}) {
        long double t=(-bb+sign*std::sqrt(discriminant))/aa;
        if(t>=0 && t<=1) samples.push_back(screen(mix2(a,b,t)));
      }
    }
    long double gx=other.field[0]*2*co/(p.viewport.width_mm*p.sx)+other.field[1]*2*si/(p.viewport.height_mm*p.sy);
    long double gy=-other.field[0]*2*si/(p.viewport.width_mm*p.sx)+other.field[1]*2*co/(p.viewport.height_mm*p.sy);
    long double norm=std::hypot(gx*rx,gy*ry);
    if(norm>0) for(int sign:{-1,1}) add({static_cast<double>(sign*gx*rx*rx/norm),static_cast<double>(sign*gy*ry*ry/norm)});
    else {add({rx,0});add({-rx,0});add({0,ry});add({0,-ry});}
  } else {
    std::vector<Ring2> rings;
    for(Index j=f.offset[id];j<f.offset[id]+f.count[id];++j) {
      if(j==f.offset[id] || packet.footprint_vertices.ring[j]!=packet.footprint_vertices.ring[j-1]) rings.emplace_back();
      rings.back().push_back({packet.footprint_vertices.x_mm[j],packet.footprint_vertices.y_mm[j]});
    }
    auto inside=[&](const Point2& q) {
      bool hit=false;
      for(const auto& ring:rings) for(std::size_t j=0;j<ring.size();++j) {
        auto a=ring[j],b=ring[(j+1)%ring.size()];
        if((a[1]>q[1])!=(b[1]>q[1]) && q[0]<(b[0]-a[0])*(q[1]-a[1])/(b[1]-a[1])+a[0]) hit=!hit;
      }
      return hit;
    };
    for(auto q:cell) if(inside(q)) samples.push_back(screen(q));
    for(const auto& ring:rings) for(std::size_t j=0;j<ring.size();++j) {
      Ring2 segment{ring[j],ring[(j+1)%ring.size()]};
      for(auto q:segment_overlap(segment,cell)) samples.push_back(screen(q));
    }
  }
  return samples;
}

static inline int marker_coverage(const Piece& piece,const ScenePacket& packet,const Projection& p) {
  ProjectedPiece carrier=project_piece(piece,p),direction;
  if(glyph_overlap(carrier.ring,piece,direction,packet,p).empty()) return 0;
  Projection unbounded=p;unbounded.camera.clip=false;
  bool clipped=false;
  Piece full=piece;
  full.polygon=marker_bounds(packet,piece.row,piece.anchor[0],unbounded,clipped);
  auto footprint=project_piece(full,p);
  for(std::size_t i=0;i<carrier.ring.size();++i) {
    const auto& a=carrier.ring[i];const auto& b=carrier.ring[(i+1)%carrier.ring.size()];
    direction.field={a[1]-b[1],b[0]-a[0],0};
    double length=std::hypot(b[0]-a[0],b[1]-a[1]);
    double tolerance=p.resolved.tolerance*length/(p.camera.perspective ? carrier.first_depth:1);
    for(const auto& q:glyph_overlap(footprint.ring,full,direction,packet,p))
      if(orient2(a,b,q)<-tolerance) return 2;
  }
  return 1;
}

static inline int occlusion_relation(const Piece& a,const Piece& b,const ProjectedPiece& x,
                                    const ProjectedPiece& y,const ScenePacket& packet,const Projection& p) {
  if(!bounds_overlap(x.bounds,y.bounds)) return 0;
  if(!a.members.empty() || !b.members.empty()) {
    for(const Piece* object:{&a,&b}) {
      auto separate=[&](const Piece& face) {
        if(face.kind!=Kind::face) return 0;
        auto plane=supporting_plane(face,p);
        int sa=classify(a,plane,p),sb=classify(b,plane,p);
        if(!((sa==1 && sb==2)||(sa==2 && sb==1))) return 0;
        double eye=p.camera.perspective ? dot(plane.normal,subtract(p.resolved.eye,plane.origin)):
          -dot(plane.normal,p.resolved.ray);
        return (eye>=0 ? sa==2:sa==1) ? 1:2;
      };
      if(object->members.empty()) {int relation=separate(*object);if(relation) return relation;}
      else for(const auto& face:object->members) {int relation=separate(face);if(relation) return relation;}
    }
    return 3;
  }
  if(a.kind==Kind::face && b.kind==Kind::face && x.area && y.area) {
    int sign=0;
    for(double u:{std::max(x.bounds[0],y.bounds[0]),std::min(x.bounds[2],y.bounds[2])})
      for(double v:{std::max(x.bounds[1],y.bounds[1]),std::min(x.bounds[3],y.bounds[3])}) {
        double d=projected_depth(x,{u,v},p.camera.perspective)-projected_depth(y,{u,v},p.camera.perspective);
        sign|=d>p.resolved.tolerance ? 1 : (d<-p.resolved.tolerance ? 2:3);
      }
    if(sign==1 || sign==2) {
      for(const auto* ring:{&x.ring,&y.ring}) {
        const auto& other=ring==&x.ring ? y.ring:x.ring;
        for(std::size_t k=0;k<ring->size();++k) {
          bool outside=true;
          for(const auto& q:other) if(orient2((*ring)[k],(*ring)[(k+1)%ring->size()],q)>0) {outside=false;break;}
          if(outside) return 0;
        }
      }
      return sign;
    }
  }
  Ring2 overlap=projected_overlap(x,y,p);
  if(overlap.empty()) return 0;
  // A boundary stroke inherits the supporting face's coplanar paint order.
  const Piece* line=a.kind==Kind::polyline ? &a : (b.kind==Kind::polyline ? &b : nullptr);
  const Piece* face=a.kind==Kind::face ? &a : (b.kind==Kind::face ? &b : nullptr);
  if(line && face && !line->hidden) {
    const Polygon& path=line->anchor.empty() ? line->polygon : line->anchor;
    auto plane=supporting_plane(*face,p);
    bool coplanar=true;
    for(const auto& vertex:path)
      coplanar=coplanar && std::abs(plane_distance(plane,vertex,p))<=p.resolved.tolerance;
    if(coplanar) return tie_less(a,b) ? 1:2;
  }
  if(a.kind==Kind::marker && b.kind!=Kind::marker) overlap=glyph_overlap(overlap,a,y,packet,p);
  else if(b.kind==Kind::marker && a.kind!=Kind::marker) overlap=glyph_overlap(overlap,b,x,packet,p);
  if(overlap.empty()) return 0;
  int relation=0;
  for(const auto& q:overlap) {
    double difference=projected_depth(x,q,p.camera.perspective)-projected_depth(y,q,p.camera.perspective);
    double normal_scale=1;
    for(const auto* projected:{&x,&y}) if(projected->area) {
      const auto& normal=projected->ray_normal;
      double incidence=std::abs(normal[2]+(p.camera.perspective ? normal[0]*q[0]+normal[1]*q[1] : 0));
      normal_scale=std::min(normal_scale,incidence);
    }
    difference*=normal_scale;
    if(difference>p.resolved.tolerance) relation|=1;
    if(difference<-p.resolved.tolerance) relation|=2;
  }
  return relation ? relation : (tie_less(a,b) ? 1:2);
}

static inline bool single_cut_component(std::vector<Piece>& pieces,const ScenePacket& packet,
                                       const Projection& p,PartitionStats& stats,ResourceBudget& budget) {
  if(pieces.size()<3 || pieces.size()>8) return false;
  for(std::size_t i=0;i<pieces.size();++i) {
    SplitPlane plane=supporting_plane(pieces[i],p);
    for(std::size_t j=0;j<pieces.size();++j) if(i!=j && classify(pieces[j],plane,p)==3) {
      auto halves=split_piece(pieces[j],plane,p);
      std::vector<Piece> trial=pieces;
      trial[j]=std::move(halves.first);trial.push_back(std::move(halves.second));
      std::vector<ProjectedPiece> projected;
      for(const auto& piece:trial) projected.push_back(project_piece(piece,p));
      std::vector<std::vector<std::size_t>> edges(trial.size());
      std::vector<std::size_t> indegree(trial.size(),0);
      for(std::size_t a=0;a<trial.size();++a) for(std::size_t b=a+1;b<trial.size();++b) {
        int relation=occlusion_relation(trial[a],trial[b],projected[a],projected[b],packet,p);
        if(relation&1) {edges[a].push_back(b);++indegree[b];}
        if(relation&2) {edges[b].push_back(a);++indegree[a];}
      }
      std::vector<std::size_t> order;
      for(std::size_t a=0;a<trial.size();++a) if(indegree[a]==0) order.push_back(a);
      for(std::size_t k=0;k<order.size();++k) for(auto b:edges[order[k]]) if(--indegree[b]==0) order.push_back(b);
      if(order.size()!=trial.size()) continue;
      budget_split(pieces[j],budget);++stats.splits;
      pieces.clear();
      for(auto a:order) pieces.push_back(std::move(trial[a]));
      return true;
    }
  }
  return false;
}

static inline std::vector<Piece> order_scene(std::vector<Piece> pieces,const ScenePacket& packet,const Projection& p,
                                            PartitionStats& stats,ResourceBudget& budget,bool group_solids=true,
                                            Index* seam_groups=nullptr) {
  Index local_groups=0;
  Index& groups=seam_groups ? *seam_groups : local_groups;
  if(group_solids) {
    std::map<Index,std::vector<Piece>> solids;
    std::vector<Piece> ungrouped;
    for(auto& piece:pieces) {
      if(piece.solid>=0 && piece.kind==Kind::face) solids[piece.solid].push_back(std::move(piece));
      else ungrouped.push_back(std::move(piece));
    }
    pieces=std::move(ungrouped);
    for(auto& entry:solids) {
      auto& members=entry.second;
      if(members.size()==1) {pieces.push_back(std::move(members.front()));continue;}
      Piece group=members.front();group.polygon.clear();
      for(const auto& member:members) group.polygon.insert(group.polygon.end(),member.polygon.begin(),member.polygon.end());
      group.members=std::move(members);pieces.push_back(std::move(group));
    }
  }
  const std::size_t n=pieces.size();
  std::vector<ProjectedPiece> projected;projected.reserve(n);
  for(const auto& piece:pieces) projected.push_back(project_piece(piece,p));
  BoundsIndex<ProjectedPiece> index(projected);
  std::vector<std::pair<Index,Index>> edges;
  auto edge=[&](std::size_t a,std::size_t b) {
    budget.add(32);edges.emplace_back(a,b);++stats.edges;
  };
  for(std::size_t i=0;i<n;++i) {
    if(i%1024==0) Rcpp::checkUserInterrupt();
    index.query(projected[i].bounds,i+1,[&](std::size_t j) {
      ++stats.pairs;
      int relation=occlusion_relation(pieces[i],pieces[j],projected[i],projected[j],packet,p);
      if(relation==3) ++budget.intersections;
      if(relation&1) edge(i,j);
      if(relation&2) edge(j,i);
    });
  }
  std::vector<std::size_t> offsets(n+1,0);
  for(const auto& e:edges) ++offsets[e.first+1];
  std::partial_sum(offsets.begin(),offsets.end(),offsets.begin());
  auto cursor=offsets;
  std::vector<Index> targets(edges.size());
  for(const auto& e:edges) targets[cursor[e.first]++]=e.second;
  std::vector<std::size_t> in_offsets(n+1,0);
  for(const auto& e:edges) ++in_offsets[e.second+1];
  std::partial_sum(in_offsets.begin(),in_offsets.end(),in_offsets.begin());
  auto in_cursor=in_offsets;
  std::vector<Index> sources(edges.size());
  for(const auto& e:edges) sources[in_cursor[e.second]++]=e.first;
  edges.clear();edges.shrink_to_fit();
  projected.clear();projected.shrink_to_fit();
  // Tarjan's explicit DFS stack emits components in reverse topological order.
  std::vector<Index> discovery(n,-1),low(n),component(n,-1),active;
  struct Frame {Index vertex;std::size_t next;};
  std::vector<Frame> dfs;
  std::vector<std::vector<Index>> components;
  Index time=0;
  auto enter=[&](Index v) {discovery[v]=low[v]=time++;active.push_back(v);dfs.push_back({v,offsets[v]});};
  for(std::size_t root=0;root<n;++root) if(discovery[root]<0) {
    enter(root);
    while(!dfs.empty()) {
      auto& frame=dfs.back();Index v=frame.vertex;
      if(frame.next<offsets[v+1]) {
        Index w=targets[frame.next++];
        if(discovery[w]<0) enter(w);
        else if(component[w]<0) low[v]=std::min(low[v],discovery[w]);
      } else {
        if(low[v]==discovery[v]) {
          std::vector<Index> group;
          Index w;
          do {w=active.back();active.pop_back();component[w]=components.size();group.push_back(w);} while(w!=v);
          stats.largest_scc=std::max(stats.largest_scc,group.size());
          stats.cyclic+=group.size()>1;
          ++stats.scc_sizes[group.size()];
          components.push_back(std::move(group));
        }
        dfs.pop_back();
        if(!dfs.empty()) low[dfs.back().vertex]=std::min(low[dfs.back().vertex],low[v]);
      }
    }
  }
  stats.components=components.size();
  std::vector<std::size_t> indegree(components.size(),0);
  std::vector<double> priority_depth(n,0);
  for(std::size_t v=0;v<n;++v) for(const auto& vertex:pieces[v].polygon)
    priority_depth[v]+=p.camera_point(vertex.xyz)[2]/pieces[v].polygon.size();
  auto free_less=[&](Index a,Index b) {
    return priority_depth[a]==priority_depth[b] ? tie_less(pieces[a],pieces[b]) : priority_depth[a]>priority_depth[b];
  };
  std::vector<Index> representative;
  for(const auto& group:components) representative.push_back(*std::min_element(group.begin(),group.end(),
    free_less));
  for(std::size_t v=0;v<n;++v) for(std::size_t e=offsets[v];e<offsets[v+1];++e)
    if(component[v]!=component[targets[e]]) ++indegree[component[targets[e]]];
  // Any topological order is an exact painter's order, since only pieces that
  // overlap on screen are constrained. Among ready components the next one
  // continues the material (kind, primitive table, layer, opacity) of the
  // last emitted piece whenever possible, so each layer is painted in long
  // runs that a backend can fill as one path; otherwise far precedes near.
  using Material=std::tuple<int,const PrimitiveTable*,Index,double>;
  std::map<Material,Index> material_index;
  std::vector<Index> material(components.size());
  for(std::size_t c=0;c<components.size();++c) {
    const Piece& r=pieces[representative[c]];
    Material key{static_cast<int>(r.kind),r.source,r.source->layer_id[r.row],r.source->opacity[r.row]};
    material[c]=material_index.emplace(key,static_cast<Index>(material_index.size())).first->second;
  }
  auto later=[&](Index a,Index b) {return free_less(representative[b],representative[a]);};
  using ReadyQueue=std::priority_queue<Index,std::vector<Index>,decltype(later)>;
  ReadyQueue ready(later);
  std::vector<ReadyQueue> ready_by_material(material_index.size(),ReadyQueue(later));
  std::vector<bool> emitted(components.size(),false),is_ready(components.size(),false);
  auto make_ready=[&](Index c) {is_ready[c]=true;ready.push(c);ready_by_material[material[c]].push(c);};
  // Before any other piece of the material, a ready fragment of the same
  // polyline: first the one that starts where the last one ended, so a backend
  // draws the two as one stroke with a line join, then any other, so masked
  // pieces of one whole stroke share a single paint operation.
  using PathKey=std::pair<const PrimitiveTable*,Index>;
  std::map<PathKey,std::vector<std::pair<double,Index>>> path_fragments;
  for(std::size_t c=0;c<components.size();++c) {
    const Piece& r=pieces[representative[c]];
    if(components[c].size()==1 && r.members.empty() && r.kind==Kind::polyline)
      path_fragments[PathKey(r.source,r.row)].emplace_back(r.path_start,static_cast<Index>(c));
  }
  for(auto& entry:path_fragments) std::sort(entry.second.begin(),entry.second.end());
  bool has_path=false;PathKey last_path(nullptr,0);double last_end=0;
  auto continuation=[&]()->Index {
    if(!has_path) return -1;
    auto found=path_fragments.find(last_path);
    if(found==path_fragments.end()) return -1;
    const auto& list=found->second;
    double tolerance=1e-9*std::max(1.0,std::abs(last_end));
    auto it=std::lower_bound(list.begin(),list.end(),std::make_pair(last_end-tolerance,Index(-1)));
    for(;it!=list.end() && it->first<=last_end+tolerance;++it)
      if(is_ready[it->second] && !emitted[it->second]) return it->second;
    for(const auto& fragment:list) if(is_ready[fragment.second] && !emitted[fragment.second]) return fragment.second;
    return -1;
  };
  auto next_ready=[&](Index last)->Index {
    Index follow=continuation();
    if(follow>=0) return follow;
    if(last>=0) {
      auto& same=ready_by_material[material[last]];
      while(!same.empty() && emitted[same.top()]) same.pop();
      if(!same.empty()) {Index c=same.top();same.pop();return c;}
    }
    while(emitted[ready.top()]) ready.pop();
    Index c=ready.top();ready.pop();return c;
  };
  std::size_t pending=0;
  for(std::size_t c=0;c<components.size();++c) if(indegree[c]==0) {make_ready(c);++pending;}
  std::vector<Piece> ordered;ordered.reserve(n);
  Index last=-1;
  // Seam groups: consecutive pieces of one material of which no two overlap on
  // screen. Every overlapping pair carries an occlusion edge, so a piece opens
  // a new group when it is adjacent to a piece of the current one. A backend
  // may paint the union of a group under its faces without covering any face
  // drawn later in the group.
  std::vector<Index> piece_group(n,-1);
  Index current_group=-1,current_material=-1;
  while(pending) {
    Index c=next_ready(last);emitted[c]=true;--pending;last=c;auto& group=components[c];
    {
      const Piece& r=pieces[representative[c]];
      has_path=group.size()==1 && r.members.empty() && r.kind==Kind::polyline;
      if(has_path) {last_path=PathKey(r.source,r.row);last_end=r.path_end;}
    }
    if(group.size()==1 && pieces[group.front()].members.empty()) {
      Index v=group.front();
      bool open=current_group<0 || material[c]!=current_material;
      for(std::size_t e=offsets[v];!open && e<offsets[v+1];++e) open=piece_group[targets[e]]==current_group;
      for(std::size_t e=in_offsets[v];!open && e<in_offsets[v+1];++e) open=piece_group[sources[e]]==current_group;
      if(open) current_group=++groups;
      current_material=material[c];piece_group[v]=current_group;
      pieces[v].seam_group=current_group;
      ordered.push_back(std::move(pieces[v]));
    } else {
      std::sort(group.begin(),group.end());
      std::vector<Piece> local;local.reserve(group.size());
      bool compound=false;
      for(Index i:group) {
        if(pieces[i].members.empty()) local.push_back(std::move(pieces[i]));
        else {compound=true;for(auto& member:pieces[i].members) local.push_back(std::move(member));}
      }
      if(compound) local=order_scene(std::move(local),packet,p,stats,budget,false,&groups);
      else {
        if(!single_cut_component(local,packet,p,stats,budget)) local=partition_scene(std::move(local),p,stats,budget);
        for(auto& piece:local) piece.seam_group=++groups;
      }
      for(auto& piece:local) ordered.push_back(std::move(piece));
      current_group=-1;current_material=-1;
    }
    for(Index v:group) for(std::size_t e=offsets[v];e<offsets[v+1];++e) {
      Index other=component[targets[e]];
      if(other!=c && --indegree[other]==0) {make_ready(other);++pending;}
    }
  }
  return ordered;
}
} }
