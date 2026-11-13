#pragma once
#include "common.h"
#include <atomic>
#include <exception>
#include <system_error>
#include <thread>

namespace ggplot3d { namespace engine {
// Runs work(block) for block in [0, blocks) on up to `threads` threads; the
// calling thread takes part. Each block writes only to its own output slot, so
// the caller merges results in block order and the outcome does not depend on
// the thread count. The work must not touch the R API. The first failure in
// block order is rethrown on the calling thread after every thread has joined.
template<class Work>
static inline void parallel_blocks(std::size_t blocks,std::size_t threads,Work work) {
  if(blocks==0) return;
  threads=std::max<std::size_t>(1,std::min(threads,blocks));
  std::vector<std::exception_ptr> failures(blocks);
  std::atomic<std::size_t> next{0};
  auto run=[&]() {
    for(std::size_t block=next++;block<blocks;block=next++) {
      try {work(block);} catch(...) {failures[block]=std::current_exception();}
    }
  };
  std::vector<std::thread> pool;pool.reserve(threads-1);
  for(std::size_t t=1;t<threads;++t) {
    try {pool.emplace_back(run);} catch(const std::system_error&) {break;}
  }
  run();
  for(auto& thread:pool) thread.join();
  for(const auto& failure:failures) if(failure) std::rethrow_exception(failure);
}
} }
