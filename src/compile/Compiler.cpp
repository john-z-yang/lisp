#include "Compiler.hpp"
#include "../runtime/VM.hpp"
#include "Compiler_codegen.cc"
#include "Compiler_parse.cc"
#include "Compiler_sema.cc"

using namespace sexpr;
using namespace compile;
using namespace runtime;

Compiler::Compiler(std::vector<std::string> source, VM &vm)
    : vm(vm), enclosing(nullptr), source(source), params(vm.alloc<Nil>()),
      body(parse()), stackOffset(1) {}
