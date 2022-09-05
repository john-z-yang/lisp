CXX = g++
CXXFLAGS = -std=c++11 -Wall

PERCENT = %

SRCDIR = src
OUTDIR = bin
TESTDIR = tests

_DEPS = env/Env.hpp env/functions.hpp eval/eval.hpp eval/EvalException.hpp \
	parse/parse.hpp parse/ParseException.hpp sexpr/Atom.hpp sexpr/BoolAtom.hpp \
	sexpr/ClosureAtom.hpp sexpr/IntAtom.hpp sexpr/NilAtom.hpp sexpr/SExpr.hpp \
	sexpr/SExprs.hpp sexpr/SymAtom.hpp

DEPS = $(addprefix $(SRCDIR)/,$(_DEPS))
OBJS = $(patsubst %.hpp,$(OUTDIR)/%.o,$(subst /,_,$(_DEPS)))

TESTS = $(TESTDIR)/combine $(TESTDIR)/cons $(TESTDIR)/hof $(TESTDIR)/list \
    $(TESTDIR)/logic $(TESTDIR)/macro $(TESTDIR)/parse $(TESTDIR)/quote \
	$(TESTDIR)/pred $(TESTDIR)/recur $(TESTDIR)/set $(TESTDIR)/tailcall \
	$(TESTDIR)/varargs

$(OUTDIR)/lisp: $(OBJS) $(DEPS) $(OUTDIR)/main.o
	$(CXX) $(CXXFLAGS) $(OBJS) $(OUTDIR)/main.o -o $(OUTDIR)/lisp

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