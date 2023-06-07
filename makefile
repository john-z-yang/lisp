CXX = g++
CXXFLAGS_EXTRA =
CXXFLAGS = -std=c++20 -Wall $(CXXFLAGS_EXTRA)

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

_DEPS =  common/sexpr/Atom.hpp \
	common/sexpr/BoolAtom.hpp common/sexpr/ClosureAtom.hpp common/sexpr/FnAtom.hpp \
	common/sexpr/IntAtom.hpp common/sexpr/NatFnAtom.hpp common/sexpr/NilAtom.hpp common/sexpr/SExpr.hpp \
	common/sexpr/SExprs.hpp common/sexpr/StringAtom.hpp common/sexpr/SymAtom.hpp \
	common/Code.hpp common/TypeError.hpp compile/Compiler.hpp compile/parse.hpp \
	compile/SyntaxError.hpp compile/Token.hpp repl/repl.hpp \
	runtime/Env.hpp runtime/NatFnImpls.hpp runtime/Upvalue.hpp runtime/VM.hpp

DEPS = $(addprefix $(SRCDIR)/,$(_DEPS))
OBJS = $(patsubst %.hpp,$(OUTDIR)/%.o,$(subst /,_,$(_DEPS)))

TESTS = $(TESTDIR)/combine $(TESTDIR)/compare $(TESTDIR)/cons \
    $(TESTDIR)/equality $(TESTDIR)/hof $(TESTDIR)/list $(TESTDIR)/math \
	$(TESTDIR)/parse $(TESTDIR)/pred $(TESTDIR)/recur $(TESTDIR)/set \
	$(TESTDIR)/string $(TESTDIR)/tailcall $(TESTDIR)/upvalues \
	$(TESTDIR)/varargs

all: $(OUTDIR)/lisp check-env

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

check-env: $(OUTDIR)/lisp
ifndef LISP_LIB_ENV
	$(warning $(ENV_NOT_SET_ERR_MSG))
endif
