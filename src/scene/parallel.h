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

// Calls body(i) for every i in [0, n). Each call writes only to slot i of its
// output, and the first failure in index order is rethrown.
template<class Body>
static inline void parallel_for(std::size_t n,std::size_t threads,Body body) {
  const std::size_t block_size=1024,blocks=(n+block_size-1)/block_size;
  parallel_blocks(blocks,threads,[&](std::size_t block) {
    for(std::size_t i=block*block_size,end=std::min(n,i+block_size);i<end;++i) body(i);
  });
}

// Splits [0, n) into blocks, computes a wave of blocks in parallel with
// compute(begin, end, result), then merges the wave's results on the calling
// thread in block order with merge(result). Merging in order makes the outcome
// independent of the thread count; merging after each wave lets the caller
// charge its budget before more work is done.
template<class Result,class Compute,class Merge>
static inline void parallel_ordered(std::size_t n,std::size_t block_size,std::size_t threads,
                                    Compute compute,Merge merge) {
  const std::size_t blocks=(n+block_size-1)/block_size;
  const std::size_t wave=std::max<std::size_t>(16,4*threads);
  std::vector<Result> results(std::min(blocks,wave));
  for(std::size_t first=0;first<blocks;first+=wave) {
    Rcpp::checkUserInterrupt();
    const std::size_t count=std::min(wave,blocks-first);
    parallel_blocks(count,threads,[&](std::size_t slot) {
      const std::size_t begin=(first+slot)*block_size;
      compute(begin,std::min(n,begin+block_size),results[slot]);
    });
    for(std::size_t slot=0;slot<count;++slot) merge(results[slot]);
  }
}
} }
