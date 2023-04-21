#ifndef LISP_SRC_SEXPR_TYPEERROR_HPP_
#define LISP_SRC_SEXPR_TYPEERROR_HPP_

#include "SExpr.hpp"
#include <exception>
#include <memory>
#include <ostream>
#include <string>

class TypeError : public std::exception {
public:
  TypeError(const std::string &msg, const std::string expected,
            const std::shared_ptr<SExpr> actual);

  virtual const char *what() const noexcept override;

  const std::string expected;
  const std::shared_ptr<SExpr> actual;

private:
  std::string _msg;
};

std::ostream &operator<<(std::ostream &o, const TypeError &pe);

#endif
