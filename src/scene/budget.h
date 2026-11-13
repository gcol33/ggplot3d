#pragma once
#include "common.h"

namespace ggplot3d { namespace engine {
struct ResourceBudget {
  double limit, estimated = 0;
  std::size_t inputs, intersections = 0, fragments = 0, threads = 1;
  explicit ResourceBudget(std::size_t n, double bytes)
    : limit(bytes), inputs(n) {}
  void add(double bytes) {
    estimated += bytes;
    if (estimated > limit)
      Rcpp::stop("Exact scene resource budget exceeded: %llu input primitives, %llu intersecting pairs, %llu estimated fragments, %.0f estimated bytes (limit %.0f). Use the raster backend or increase the internal engine_budget_bytes viewport attribute.",
        static_cast<unsigned long long>(inputs), static_cast<unsigned long long>(intersections),
        static_cast<unsigned long long>(fragments), estimated, limit);
  }
  void piece(std::size_t corners) {
    ++fragments;
    add(768.0 + 192.0*corners);
  }
};
} }
