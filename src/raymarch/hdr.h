#pragma once
#include <Rcpp.h>
#include <fstream>
#include <sstream>
#include <array>

namespace ggplot3d { namespace raymarch {
inline Rcpp::NumericVector read_hdr(const std::string& path) {
  std::ifstream in(path,std::ios::binary);if(!in)Rcpp::stop("Cannot open HDR image");
  std::string line;std::getline(in,line);
  if(line.rfind("#?",0)!=0)Rcpp::stop("Invalid Radiance HDR signature");
  bool format=false;
  while(std::getline(in,line)) {if(!line.empty() && line.back()=='\r')line.pop_back();if(line.empty())break;
    if(line=="FORMAT=32-bit_rle_rgbe")format=true;}
  if(!format)Rcpp::stop("HDR image must use FORMAT=32-bit_rle_rgbe");
  std::getline(in,line);std::istringstream resolution(line);std::string first,second;int n1=0,n2=0;
  resolution>>first>>n1>>second>>n2;
  auto axis=[](const std::string& s){return s.size()==2 && (s[0]=='+' || s[0]=='-') && (s[1]=='X' || s[1]=='Y');};
  if(!axis(first) || !axis(second) || first[1]==second[1] || n1<=0 || n2<=0 || double(n1)*n2>1e8)
    Rcpp::stop("Invalid HDR resolution");
  int width=first[1]=='X' ? n1:n2,height=first[1]=='Y' ? n1:n2;
  Rcpp::NumericVector out(R_xlen_t(width)*height*3);out.attr("dim")=Rcpp::IntegerVector::create(height,width,3);
  auto byte=[&](){int b=in.get();if(b==EOF)Rcpp::stop("Truncated HDR image");return b;};
  std::vector<std::array<int,4>> scan(n2);
  for(int row=0;row<n1;++row) {
    std::array<int,4> p;for(int& v:p)v=byte();
    if(n2>=8 && n2<=32767 && p[0]==2 && p[1]==2 && !(p[2]&128)) {
      if((p[2]<<8|p[3])!=n2)Rcpp::stop("HDR scanline length mismatch");
      for(int k=0;k<4;++k)for(int i=0;i<n2;) {int code=byte();if(code==0)Rcpp::stop("Invalid HDR RLE run");
        int count=code>128 ? code-128:code;if(i+count>n2)Rcpp::stop("HDR RLE run exceeds scanline");
        if(code>128){int v=byte();while(count--)scan[i++][k]=v;}else while(count--)scan[i++][k]=byte();}
    } else {
      int i=0,shift=0;
      while(i<n2) {
        if(p[0]==1 && p[1]==1 && p[2]==1) {
          if(i==0 || shift>24)Rcpp::stop("Invalid HDR flat run");
          std::uint64_t count=std::uint64_t(p[3])<<shift;if(count>std::uint64_t(n2-i))Rcpp::stop("HDR flat run exceeds scanline");
          while(count--){scan[i]=scan[i-1];++i;}shift+=8;
        }else{scan[i++]=p;shift=0;}
        if(i<n2)for(int& v:p)v=byte();
      }
    }
    for(int col=0;col<n2;++col) {
      int x=first[1]=='X' ? row:col,y=first[1]=='Y' ? row:col;
      std::string xs=first[1]=='X' ? first:second,ys=first[1]=='Y' ? first:second;
      if(xs[0]=='-')x=width-1-x;
      if(ys[0]=='+')y=height-1-y;
      double factor=scan[col][3] ? std::ldexp(1.0,scan[col][3]-136):0;
      for(int k=0;k<3;++k)out[y+height*(x+width*k)]=scan[col][k]*factor;
    }
  }
  return out;
}
} }
