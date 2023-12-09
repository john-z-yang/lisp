#include "GCGuard.hpp"

using namespace runtime;

GCGuard::GCGuard(Heap &heap)
    : heap(heap), prevGCState(heap.enableGC), released(false) {
  heap.enableGC = false;
}

GCGuard::GCGuard(GCGuard &&other)
    : heap(other.heap),
      prevGCState(other.prevGCState),
      released(other.released) {
  other.released = true;
}

void GCGuard::release() {
  if (released) {
    return;
  }
  heap.enableGC = prevGCState;
  released = true;
  heap.gc();
}

GCGuard::~GCGuard() { release(); }
