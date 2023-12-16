CXX = g++
CXXFLAGS = -std=c++20 $(CXXFLAGS_ASAN) $(CXXFLAGS_WARN) $(CXXFLAGS_EXTRA)
CXXFLAGS_ASAN = -fsanitize=address,undefined -g
CXXFLAGS_WARN = -pedantic -Wall -Wextra -Wno-gnu-label-as-value
CXXFLAGS_EXTRA = -O2

PERCENT = %
define NEWLINE


endef

SRCDIR = src
OUTDIR = bin
TESTDIR = tests
LIBDIR = $(shell dirname $(realpath $(firstword $(MAKEFILE_LIST))))/lib

define ENV_NOT_SET_ERR_MSG
$(NEWLINE)$(NEWLINE)WARNING: LISP_LIB_ENV is undefined.$(NEWLINE)
run:
    export LISP_LIB_ENV=$(LIBDIR)$(NEWLINE)$(NEWLINE)
To set it to the /lib folder of this project.
endef

_DEPS = code/Code.hpp \
	compile/CodeGenerator.hpp compile/Parser.hpp \
	error/RuntimeError.hpp error/SyntaxError.hpp error/TypeError.hpp \
	fn/CPPFnImpls.hpp \
	repl/repl.hpp \
	runtime/Env.hpp runtime/Heap.hpp runtime/GCGuard.hpp  runtime/Upvalue.hpp runtime/VM.hpp \
	sexpr/Atom.hpp sexpr/Bool.hpp sexpr/Closure.hpp sexpr/Prototype.hpp sexpr/Num.hpp \
	sexpr/NatFn.hpp sexpr/Nil.hpp sexpr/SExpr.hpp sexpr/SExprs.hpp \
	sexpr/String.hpp sexpr/Sym.hpp sexpr/Undefined.hpp


DEPS = $(addprefix $(SRCDIR)/,$(_DEPS))
OBJS = $(patsubst %.hpp,$(OUTDIR)/%.o,$(subst /,_,$(_DEPS)))

TESTS = \
	$(TESTDIR)/apply \
	$(TESTDIR)/begin \
	$(TESTDIR)/combine \
	$(TESTDIR)/compare \
	$(TESTDIR)/cons \
	$(TESTDIR)/define \
	$(TESTDIR)/equality \
	$(TESTDIR)/hof \
	$(TESTDIR)/lexbind \
	$(TESTDIR)/list \
	$(TESTDIR)/logic \
	$(TESTDIR)/math \
	$(TESTDIR)/parse \
	$(TESTDIR)/pred \
	$(TESTDIR)/recur \
	$(TESTDIR)/set \
	$(TESTDIR)/sort \
	$(TESTDIR)/string \
	$(TESTDIR)/tailcall \
	$(TESTDIR)/upvalues \
	$(TESTDIR)/varargs \
	$(TESTDIR)/z-combinator

all: $(OUTDIR)/lisp check-env

$(OUTDIR)/lisp: $(OBJS) $(DEPS) $(OUTDIR)/main.o
	$(CXX) $(CXXFLAGS) $(OBJS) $(OUTDIR)/main.o -lreadline -o $(OUTDIR)/lisp

$(OUTDIR)/main.o: $(SRCDIR)/main.cpp $(DEPS)
	$(CXX) $(CXXFLAGS) -c $< -o $@

.SECONDEXPANSION:
$(OUTDIR)/%.o: $$(subst _,/,$$(patsubst $$(OUTDIR)/$$(PERCENT).o,src/$$(PERCENT).cpp,$$@)) \
	$$(subst _,/,$$(patsubst $$(OUTDIR)/$$(PERCENT).o,src/$$(PERCENT).hpp,$$@))
	$(CXX) $(CXXFLAGS) -c $< -o $@

$(TESTDIR)/%: $(TESTDIR)/%.scm $(TESTDIR)/%.expect $(OUTDIR)/lisp
	(export LISP_LIB_ENV=$(LIBDIR); $(OUTDIR)/lisp $@.scm >> $@.out 2>&1)
	diff $@.expect $@.out
	rm $@.out

.PHONY: test clean clean-test check-env

test: clean-tests $(TESTS) check-env

clean-tests:
	-rm tests/*.out

clean:
	-rm $(OUTDIR)/lisp $(OUTDIR)/*.o tests/*.out

check-env: $(OUTDIR)/lisp
ifndef LISP_LIB_ENV
	$(warning $(ENV_NOT_SET_ERR_MSG))
endif
