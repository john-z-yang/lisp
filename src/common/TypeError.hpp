#ifndef LISP_SRC_EXCEPT_TYPEERROR_HPP_
#define LISP_SRC_EXCEPT_TYPEERROR_HPP_

#include "sexpr/SExpr.hpp"
#include <exception>
#include <memory>
#include <ostream>
#include <string>

class TypeError : public std::exception {
private:
  std::string _msg;

public:
  TypeError(const std::string &msg, const std::string expected,
            const std::shared_ptr<SExpr> actual);

  virtual const char *what() const noexcept override;

  const std::string expected;
  const std::shared_ptr<SExpr> actual;
};

std::ostream &operator<<(std::ostream &o, const TypeError &pe);

#endif
