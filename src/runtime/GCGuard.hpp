#ifndef LISP_SRC_RUNTIME_GCGUARD_HPP_
#define LISP_SRC_RUNTIME_GCGUARD_HPP_

namespace runtime {

class GCGuard {
  friend class Heap;

private:
  bool prevGCState;
  bool gcState;
  bool &gcStateRef;

  GCGuard(bool &enableGC, bool gcState);

public:
  GCGuard(GCGuard &&gcGuard);

  ~GCGuard();
};

} // namespace runtime

#endif
