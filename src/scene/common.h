#pragma once

#include <Rcpp.h>
#include "contract.h"
#include <algorithm>
#include <cmath>
#include <limits>
#include <numeric>
#include <stdexcept>
#include <string>
#include <tuple>
#include <utility>

namespace ggplot3d { namespace engine {
using Vec3 = std::array<double, 3>;
constexpr double pi = 3.14159265358979323846;

// Engine failures are plain C++ exceptions so that they can be raised on worker
// threads; the Rcpp export wrapper turns them into R errors.
struct EngineError : std::runtime_error {
  explicit EngineError(const std::string& message) : std::runtime_error(message) {}
};

template <typename Message>
static inline void require(bool condition, const Message& message) {
  if (!condition) throw EngineError(message);
}
} }
