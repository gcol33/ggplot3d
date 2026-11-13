#include <Rcpp.h>
#include <array>
#include <cstdint>

// CRC-32 of the PNG specification (ISO/IEC 15948, annex D): reflected
// polynomial 0xEDB88320, initial value and final xor 0xFFFFFFFF.
// [[Rcpp::export(rng = false)]]
double png_crc32_cpp(Rcpp::RawVector bytes) {
  static const std::array<std::uint32_t, 256> table = [] {
    std::array<std::uint32_t, 256> t{};
    for (std::uint32_t n = 0; n < 256; ++n) {
      std::uint32_t c = n;
      for (int k = 0; k < 8; ++k) c = (c & 1u) ? 0xEDB88320u ^ (c >> 1) : c >> 1;
      t[n] = c;
    }
    return t;
  }();
  std::uint32_t crc = 0xFFFFFFFFu;
  for (R_xlen_t i = 0; i < bytes.size(); ++i) crc = table[(crc ^ bytes[i]) & 0xFFu] ^ (crc >> 8);
  return static_cast<double>(crc ^ 0xFFFFFFFFu);
}
