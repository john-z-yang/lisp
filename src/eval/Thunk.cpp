#include "Thunk.hpp"
#include "eval.hpp"

Thunk::Thunk(std::function<std::unique_ptr<Thunk>()> &&f) : f(f) {}

std::unique_ptr<Thunk> Thunk::execute() { return f(); }