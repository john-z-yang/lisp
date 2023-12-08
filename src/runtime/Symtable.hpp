#ifndef LISP_SRC_RUNTIME_SYMTABLE_HPP_
#define LISP_SRC_RUNTIME_SYMTABLE_HPP_

#include "../sexpr/Sym.hpp"
#include <unordered_map>

namespace runtime {

using SymTable = std::unordered_map<
    const sexpr::Sym *,
    const sexpr::SExpr *,
    sexpr::Sym::HashFunction,
    sexpr::Sym::EqualFunction>;

}

#endif
