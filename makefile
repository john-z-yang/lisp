CXX = g++
CXXFLAGS_EXTRA =
CXXFLAGS = -std=c++2a -Wall $(CXXFLAGS_EXTRA)

PERCENT = %

SRCDIR = src
OUTDIR = bin
TESTDIR = tests

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
    $(TESTDIR)/load $(TESTDIR)/logic $(TESTDIR)/macro $(TESTDIR)/parse \
	$(TESTDIR)/quote $(TESTDIR)/pred $(TESTDIR)/recur $(TESTDIR)/set \
	$(TESTDIR)/string $(TESTDIR)/tailcall $(TESTDIR)/varargs

$(OUTDIR)/lisp: $(OBJS) $(DEPS) $(OUTDIR)/main.o
	$(CXX) $(CXXFLAGS) $(OBJS) $(OUTDIR)/main.o -lreadline -o $(OUTDIR)/lisp

$(OUTDIR)/main.o: $(SRCDIR)/main.cpp $(DEPS)
	$(CXX) $(CXXFLAGS) -c $< -o $@

.SECONDEXPANSION:
$(OUTDIR)/%.o: $$(subst _,/,$$(patsubst $$(OUTDIR)/$$(PERCENT).o,src/$$(PERCENT).cpp,$$@)) \
	$$(subst _,/,$$(patsubst $$(OUTDIR)/$$(PERCENT).o,src/$$(PERCENT).hpp,$$@))
	$(CXX) $(CXXFLAGS) -c $< -o $@

$(TESTDIR)/%: $(TESTDIR)/%.lisp $(TESTDIR)/%.expect $(OUTDIR)/lisp
	$(OUTDIR)/lisp $@.lisp >> $@.out 2>&1
	diff $@.expect $@.out
	rm $@.out

.PHONY: test clean

test: $(TESTS)

clean:
	-rm $(OUTDIR)/lisp $(OUTDIR)/*.o tests/*.out
