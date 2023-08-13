#ifndef LISP_SRC_ERROR_SYNTAXERROR_HPP_
#define LISP_SRC_ERROR_SYNTAXERROR_HPP_

#include <exception>
#include <ostream>
#include <string>

namespace error {

class SyntaxError : public std::exception {
  friend std::ostream &operator<<(std::ostream &o, const SyntaxError &se);

private:
  const std::string _msg;
  const std::string line;
  const unsigned int row;
  const unsigned int col;

public:
  SyntaxError(
      const std::string msg,
      const std::string line,
      const unsigned int row,
      const unsigned int col
  );

  virtual const char *what() const noexcept override;
};

std::ostream &operator<<(std::ostream &o, const SyntaxError &se);

} // namespace error

#endif
