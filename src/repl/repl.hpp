#ifndef LISP_SRC_REPL_REPL_HPP_
#define LISP_SRC_REPL_REPL_HPP_

#include "../env/Env.hpp"
#include <memory>
#include <string>

int repl(std::shared_ptr<Env> env);

int repl(const std::string fileName, std::shared_ptr<Env> env);

#endif