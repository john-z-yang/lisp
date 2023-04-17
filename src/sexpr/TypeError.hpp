#ifndef LISP_SRC_SEXPR_TYPEERROR_HPP_
#define LISP_SRC_SEXPR_TYPEERROR_HPP_

#include <exception>
#include <ostream>
#include <string>

class TypeError : public std::exception {
public:
  TypeError(const std::string &msg);

  virtual const char *what() const noexcept override;

private:
  std::string _msg;
};

std::ostream &operator<<(std::ostream &o, const TypeError &pe);

#endif
