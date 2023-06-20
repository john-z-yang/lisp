#ifndef LISP_SRC_REPL_REPL_HPP_
#define LISP_SRC_REPL_REPL_HPP_

#include <memory>
#include <string>

namespace repl {

int repl();

int repl(const std::string fileName);

} // namespace repl

#endif
