#ifndef GGPLOT3D_GEOMETRY_SPARSE_H
#define GGPLOT3D_GEOMETRY_SPARSE_H
#include "vector.h"
#include <complex>
namespace ggplot3d_geometry {
namespace sparse {

using Complex=std::complex<double>;
static inline double conj_(double x) { return x; }
static inline Complex conj_(Complex z) { return std::conj(z); }
static inline double real_(double x) { return x; }
static inline double real_(Complex z) { return z.real(); }
static inline double abs2_(double x) { return x*x; }
static inline double abs2_(Complex z) { return std::norm(z); }

// Hermitian positive definite matrix given by its diagonal and one entry
// (r, c, v) per off-diagonal pair, meaning A(r, c) = v and A(c, r) =
// conj(v); repeated entries are summed. Each row carries a point in space
// for the ordering.
template<class T> struct Matrix {
  int n=0; std::vector<double> diag; std::vector<int> r,c; std::vector<T> v; std::vector<V> at;
  explicit Matrix(int size=0): n(size),diag(size,0),at(size,V{0,0,0}) {}
  void add(int i,int j,T x) { r.push_back(i); c.push_back(j); v.push_back(x); }
};

// Fill-reducing elimination order by geometric nested dissection (George
// 1973; Miller, Teng, Thurston and Vavasis 1998): the rows of a set are
// split at the median of their longest coordinate extent, the rows on the
// smaller side of the cut that touch the other side form a separator, the
// two halves are ordered recursively and the separator is eliminated last.
// Sets of at most 32 rows keep their order.
struct Dissection {
  const std::vector<int>& ptr; const std::vector<int>& adj; const std::vector<V>& at;
  std::vector<int> label,order; int next=1;
  Dissection(const std::vector<int>& p,const std::vector<int>& a,const std::vector<V>& x): ptr(p),adj(a),at(x),label(x.size(),0) {}
  void run(std::vector<int> set) {
    if(set.size()<=32) { order.insert(order.end(),set.begin(),set.end()); return; }
    V lo=at[set[0]],hi=lo;
    for(int i:set) for(int a=0;a<3;++a) { lo[a]=std::min(lo[a],at[i][a]); hi[a]=std::max(hi[a],at[i][a]); }
    int axis=0; for(int a=1;a<3;++a) if(hi[a]-lo[a]>hi[axis]-lo[axis]) axis=a;
    size_t half=set.size()/2;
    std::nth_element(set.begin(),set.begin()+half,set.end(),[&](int a,int b) { return at[a][axis]<at[b][axis]; });
    int left=next++,right=next++;
    for(size_t k=0;k<set.size();++k) label[set[k]]=k<half?left:right;
    std::vector<int> part[2],cut[2];
    for(size_t k=0;k<set.size();++k) {
      int i=set[k],side=k<half?0:1,other=side?left:right; bool touches=false;
      for(int q=ptr[i];q<ptr[i+1]&&!touches;++q) touches=label[adj[q]]==other;
      (touches?cut[side]:part[side]).push_back(i);
    }
    int s=cut[0].size()<=cut[1].size()?0:1;
    part[1-s].insert(part[1-s].end(),cut[1-s].begin(),cut[1-s].end());
    run(std::move(part[0])); run(std::move(part[1]));
    order.insert(order.end(),cut[s].begin(),cut[s].end());
  }
};

// Sparse Cholesky factor P A P^T = L L^H of a Hermitian positive definite
// matrix. Symbolic analysis follows Davis (2006, ch. 4): the elimination
// tree by Liu's algorithm with path compression, column counts from the row
// subtrees, then an up-looking numeric factorisation that computes row k of
// L by a sparse triangular solve over the reach of column k in the tree.
template<class T> struct Cholesky {
  int n=0; std::vector<int> perm,inverse,Lp,Li; std::vector<T> Lx;

  Cholesky(const Matrix<T>& A,double budget_bytes) {
    n=A.n;
    std::vector<std::vector<int>> nb(n);
    for(size_t e=0;e<A.r.size();++e) if(A.r[e]!=A.c[e]) { nb[A.r[e]].push_back(A.c[e]); nb[A.c[e]].push_back(A.r[e]); }
    std::vector<int> ptr(n+1,0),adj;
    for(int i=0;i<n;++i) { std::sort(nb[i].begin(),nb[i].end()); nb[i].erase(std::unique(nb[i].begin(),nb[i].end()),nb[i].end()); ptr[i+1]=ptr[i]+nb[i].size(); adj.insert(adj.end(),nb[i].begin(),nb[i].end()); }
    nb.clear(); nb.shrink_to_fit();
    Dissection d(ptr,adj,A.at); std::vector<int> all(n); std::iota(all.begin(),all.end(),0); d.run(all);
    perm=d.order; inverse.assign(n,0); for(int k=0;k<n;++k) inverse[perm[k]]=k;

    // Upper triangle of the permuted matrix by columns, duplicates summed.
    std::vector<int> Cp(n+1,0); std::vector<std::pair<int,T>> entries(A.r.size());
    std::vector<int> column(A.r.size());
    for(size_t e=0;e<A.r.size();++e) {
      int a=inverse[A.r[e]],b=inverse[A.c[e]];
      if(a==b) Rcpp::stop("Off-diagonal entries must join two different rows.");
      column[e]=std::max(a,b); ++Cp[column[e]+1];
    }
    for(int k=0;k<n;++k) Cp[k+1]+=Cp[k];
    { std::vector<int> fill(Cp.begin(),Cp.end()-1);
      for(size_t e=0;e<A.r.size();++e) {
        int a=inverse[A.r[e]],b=inverse[A.c[e]];
        entries[fill[column[e]]++]=a<b?std::make_pair(a,A.v[e]):std::make_pair(b,conj_(A.v[e]));
      } }
    std::vector<int> Ci; std::vector<T> Cx; std::vector<int> Cq(n+1,0);
    for(int k=0;k<n;++k) {
      std::sort(entries.begin()+Cp[k],entries.begin()+Cp[k+1],[](const std::pair<int,T>& x,const std::pair<int,T>& y) { return x.first<y.first; });
      for(int q=Cp[k];q<Cp[k+1];++q) {
        if(Ci.size()>static_cast<size_t>(Cq[k])&&Ci.back()==entries[q].first) Cx.back()+=entries[q].second;
        else { Ci.push_back(entries[q].first); Cx.push_back(entries[q].second); }
      }
      Cq[k+1]=Ci.size();
    }
    entries.clear(); entries.shrink_to_fit();

    std::vector<int> parent(n,-1),ancestor(n,-1),flag(n,-1),count(n,1);
    for(int k=0;k<n;++k) for(int q=Cq[k];q<Cq[k+1];++q) {
      int i=Ci[q];
      while(i!=-1&&i<k) { int up=ancestor[i]; ancestor[i]=k; if(up==-1) { parent[i]=k; break; } i=up; }
    }
    for(int k=0;k<n;++k) { flag[k]=k; for(int q=Cq[k];q<Cq[k+1];++q) for(int i=Ci[q];flag[i]!=k;i=parent[i]) { flag[i]=k; ++count[i]; } }
    Lp.assign(n+1,0); for(int k=0;k<n;++k) Lp[k+1]=Lp[k]+count[k];
    double bytes=static_cast<double>(Lp[n])*(sizeof(T)+sizeof(int));
    if(bytes>budget_bytes) Rcpp::stop("The sparse Cholesky factor needs %.0f MB, above the budget of %.0f MB.",bytes/1048576,budget_bytes/1048576);
    Li.assign(Lp[n],0); Lx.assign(Lp[n],T(0));

    std::vector<int> top(Lp.begin(),Lp.end()-1),pattern; std::vector<T> x(n,T(0));
    std::fill(flag.begin(),flag.end(),-1);
    for(int k=0;k<n;++k) {
      pattern.clear(); flag[k]=k;
      for(int q=Cq[k];q<Cq[k+1];++q) {
        x[Ci[q]]=Cx[q];
        for(int i=Ci[q];flag[i]!=k;i=parent[i]) { flag[i]=k; pattern.push_back(i); }
      }
      std::sort(pattern.begin(),pattern.end());
      double d=A.diag[perm[k]];
      for(int j:pattern) {
        T z=x[j]/real_(Lx[Lp[j]]); x[j]=T(0);
        for(int q=Lp[j]+1;q<top[j];++q) x[Li[q]]-=Lx[q]*z;
        d-=abs2_(z);
        Li[top[j]]=k; Lx[top[j]++]=conj_(z);
      }
      if(!(d>0)) Rcpp::stop("The matrix is not positive definite.");
      Li[top[k]]=k; Lx[top[k]++]=T(std::sqrt(d));
      if(k%4096==0) Rcpp::checkUserInterrupt();
    }
  }
  size_t nonzeros() const { return Lx.size(); }
  // Solves A x = b.
  std::vector<T> solve(const std::vector<T>& b) const {
    std::vector<T> y(n);
    for(int k=0;k<n;++k) y[k]=b[perm[k]];
    for(int j=0;j<n;++j) { y[j]/=real_(Lx[Lp[j]]); for(int q=Lp[j]+1;q<Lp[j+1];++q) y[Li[q]]-=Lx[q]*y[j]; }
    for(int j=n-1;j>=0;--j) { T s=y[j]; for(int q=Lp[j]+1;q<Lp[j+1];++q) s-=conj_(Lx[q])*y[Li[q]]; y[j]=s/real_(Lx[Lp[j]]); }
    std::vector<T> out(n);
    for(int k=0;k<n;++k) out[perm[k]]=y[k];
    return out;
  }
};
}
}
#endif
