#include "env/Env.hpp"
#include "repl/repl.hpp"
#include <memory>

int main(int argc, char const *argv[]) {
  auto env = std::make_shared<Env>();
  initEnv(env);
  auto exitCode = EXIT_FAILURE;
  if (argc == 1) {
    exitCode = repl(env);
  } else if (argc == 2) {
    exitCode = repl(argv[1], env);
  }
  env->clear();
  return exitCode;
}