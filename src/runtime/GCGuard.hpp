#ifndef LISP_SRC_RUNTIME_GCGUARD_HPP_
#define LISP_SRC_RUNTIME_GCGUARD_HPP_

#include "Heap.hpp"

namespace runtime {

class GCGuard {
  friend class Heap;

private:
  Heap &heap;
  bool prevGCState;
  bool released;

  GCGuard(Heap &heap);

public:
  GCGuard(GCGuard &&gcGuard);
  void release();

  ~GCGuard();
};

} // namespace runtime

#endif
