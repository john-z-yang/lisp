CXX = g++
CXXFLAGS_EXTRA =
CXXFLAGS = -std=c++2a -Wall $(CXXFLAGS_EXTRA)

PERCENT = %
define newline


endef

SRCDIR = src
OUTDIR = bin
TESTDIR = tests
LIBDIR = $(shell dirname $(realpath $(firstword $(MAKEFILE_LIST))))/lib

define ENV_NOT_SET_ERR_MSG
$(newline)$(newline)WARNING: LISP_LIB_ENV is undefined.$(newline)
run:
    export LISP_LIB_ENV=$(LIBDIR)$(newline)$(newline)
To set it to the /lib folder of this project.
endef

_DEPS = code/Code.hpp compile/Compiler.hpp compile/parse.hpp \
	compile/SyntaxError.hpp compile/Token.hpp repl/repl.hpp sexpr/Atom.hpp \
	sexpr/BoolAtom.hpp sexpr/ClosureAtom.hpp sexpr/FnAtom.hpp \
	sexpr/IntAtom.hpp sexpr/NatFnAtom.hpp sexpr/NilAtom.hpp sexpr/SExpr.hpp \
	sexpr/SExprs.hpp sexpr/StringAtom.hpp sexpr/SymAtom.hpp \
	sexpr/TypeError.hpp vm/Env.hpp vm/NatFnImpls.hpp vm/RuntimeException.hpp \
	vm/VM.hpp

DEPS = $(addprefix $(SRCDIR)/,$(_DEPS))
OBJS = $(patsubst %.hpp,$(OUTDIR)/%.o,$(subst /,_,$(_DEPS)))

TESTS = $(TESTDIR)/combine $(TESTDIR)/cons $(TESTDIR)/hof $(TESTDIR)/list \
    $(TESTDIR)/parse $(TESTDIR)/recur $(TESTDIR)/set $(TESTDIR)/string \
	$(TESTDIR)/tailcall $(TESTDIR)/varargs

$(OUTDIR)/lisp: $(OBJS) $(DEPS) $(OUTDIR)/main.o
	$(CXX) $(CXXFLAGS) $(OBJS) $(OUTDIR)/main.o -lreadline -o $(OUTDIR)/lisp

$(OUTDIR)/main.o: $(SRCDIR)/main.cpp $(DEPS)
	$(CXX) $(CXXFLAGS) -c $< -o $@

.SECONDEXPANSION:
$(OUTDIR)/%.o: $$(subst _,/,$$(patsubst $$(OUTDIR)/$$(PERCENT).o,src/$$(PERCENT).cpp,$$@)) \
	$$(subst _,/,$$(patsubst $$(OUTDIR)/$$(PERCENT).o,src/$$(PERCENT).hpp,$$@))
	$(CXX) $(CXXFLAGS) -c $< -o $@

$(TESTDIR)/%: $(TESTDIR)/%.lisp $(TESTDIR)/%.expect $(OUTDIR)/lisp
	(export LISP_LIB_ENV=$(LIBDIR); $(OUTDIR)/lisp $@.lisp >> $@.out 2>&1)
	diff $@.expect $@.out
	rm $@.out

.PHONY: test clean clean-test check-env

test: clean-tests $(TESTS) check-env

clean-tests:
	-rm tests/*.out
clean:
	-rm $(OUTDIR)/lisp $(OUTDIR)/*.o tests/*.out

check-env:
ifndef LISP_LIB_ENV
	$(warning $(ENV_NOT_SET_ERR_MSG))
endif
