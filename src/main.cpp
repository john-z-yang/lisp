#include "repl/repl.hpp"
#include <memory>
#include <string>

int main(int argc, char const *argv[]) {
  auto exitCode = EXIT_FAILURE;
  if (argc == 1) {
    exitCode = repl();
  } else if (argc == 2) {
    exitCode = repl(argv[1]);
  }
  return exitCode;
}