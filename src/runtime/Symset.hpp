#ifndef LISP_SRC_RUNTIME_SYMSET_HPP_
#define LISP_SRC_RUNTIME_SYMSET_HPP_

#include "../sexpr/Sym.hpp"
#include <unordered_set>

namespace runtime {

using Symset = std::unordered_set<
    const sexpr::Sym *,
    sexpr::Sym::HashFunction,
    sexpr::Sym::EqualFunction>;

}

#endif
