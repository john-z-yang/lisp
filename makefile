CXX = g++
CXXFLAGS = -std=c++11

PERCENT = %

SRCDIR = src
OUTDIR = bin
TESTDIR = tests

_DEPS = env/Env.hpp env/functions.hpp eval/eval.hpp eval/EvalException.hpp \
	repl/ParseException.hpp repl/repl.hpp sexpr/Atom.hpp sexpr/BoolAtom.hpp \
	sexpr/ClosureAtom.hpp sexpr/IntAtom.hpp sexpr/NilAtom.hpp sexpr/SExpr.hpp \
	sexpr/SExprs.hpp sexpr/SymAtom.hpp
	
DEPS = $(addprefix $(SRCDIR)/,$(_DEPS))
OBJS = $(patsubst %.hpp,$(OUTDIR)/%.o,$(subst /,_,$(_DEPS)))

TESTS = $(TESTDIR)/combine $(TESTDIR)/cons $(TESTDIR)/hof $(TESTDIR)/parse \
	$(TESTDIR)/pred $(TESTDIR)/recur $(TESTDIR)/set

$(OUTDIR)/lisp: $(OBJS) $(DEPS) $(OUTDIR)/main.o
	$(CXX) $(CXXFLAGS) $(OBJS) $(OUTDIR)/main.o -o $(OUTDIR)/lisp

$(OUTDIR)/main.o: $(SRCDIR)/repl/main.cpp $(DEPS)
	$(CXX) $(CXXFLAGS) -c $< -o $@

.SECONDEXPANSION:
$(OUTDIR)/%.o: $$(subst _,/,$$(patsubst $$(OUTDIR)/$$(PERCENT).o,src/$$(PERCENT).cpp,$$@)) \
	$$(subst _,/,$$(patsubst $$(OUTDIR)/$$(PERCENT).o,src/$$(PERCENT).hpp,$$@))
	$(CXX) $(CXXFLAGS) -c $< -o $@

$(TESTDIR)/%: $(TESTDIR)/%.in $(TESTDIR)/%.expect $(OUTDIR)/lisp
	$(OUTDIR)/lisp < $@.in > $@.out
	diff $@.expect $@.out
	rm $@.out

.PHONY: test clean

test: $(TESTS)

clean:
	-rm $(OUTDIR)/lisp $(OUTDIR)/*.o tests/*.out