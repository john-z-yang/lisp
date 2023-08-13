#ifndef LISP_SRC_ERROR_TYPEERROR_HPP_
#define LISP_SRC_ERROR_TYPEERROR_HPP_

#include "../sexpr/SExpr.hpp"
#include <exception>
#include <memory>
#include <ostream>
#include <string>

namespace error {

class TypeError : public std::exception {
private:
  std::string _msg;

public:
  TypeError(
      const std::string &msg,
      const std::string expected,
      const sexpr::SExpr &actual
  );

  virtual const char *what() const noexcept override;

  const std::string expected;
  const sexpr::SExpr &actual;
};

std::ostream &operator<<(std::ostream &o, const TypeError &pe);

} // namespace error

#endif
