#include "GCGuard.hpp"
#include <iostream>

using namespace runtime;

GCGuard::GCGuard(bool &enableGC, bool gcState)
    : prevGCState(enableGC), gcStateRef(enableGC) {
  enableGC = gcState;
}

GCGuard::GCGuard(GCGuard &&other)
    : prevGCState(other.prevGCState), gcStateRef(other.gcStateRef) {}

GCGuard::~GCGuard() { gcStateRef = prevGCState; }
