#include "repl.hpp"

int main(int argc, char const *argv[]) {
  if (argc == 1) {
    return repl();
  } else if (argc == 2) {
    return repl(argv[1]);
  }
  return 0;
}