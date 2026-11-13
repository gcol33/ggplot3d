#pragma once

#include <Rcpp.h>
#include "contract.h"
#include <algorithm>
#include <cmath>
#include <limits>
#include <numeric>
#include <string>
#include <tuple>
#include <utility>

namespace ggplot3d { namespace engine {
using Vec3 = std::array<double, 3>;
constexpr double pi = 3.14159265358979323846;

static inline void require(bool condition, const std::string& message) {
  if (!condition) Rcpp::stop(message);
}
} }
