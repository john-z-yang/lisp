CXX = g++
CXXFLAGS = -std=c++11

out/lisp: out/main.o out/repl.o out/functions.o out/Env.o out/SymAtom.o out/SExprs.o out/SExpr.o out/NilAtom.o out/IntAtom.o out/ClosureAtom.o out/BoolAtom.o out/Atom.o
	$(CXX) $(CXXFLAGS) out/main.o out/repl.o out/functions.o out/Env.o out/SymAtom.o out/SExprs.o out/SExpr.o out/NilAtom.o out/IntAtom.o out/ClosureAtom.o out/BoolAtom.o out/Atom.o -o out/lisp

out/main.o: src/repl/main.cpp
	$(CXX) $(CXXFLAGS) -c src/repl/main.cpp -o out/main.o

out/repl.o: src/repl/repl.cpp
	$(CXX) $(CXXFLAGS) -c src/repl/repl.cpp -o out/repl.o

out/functions.o: src/env/functions.cpp
	$(CXX) $(CXXFLAGS) -c src/env/functions.cpp -o out/functions.o

out/Env.o: src/env/Env.cpp
	$(CXX) $(CXXFLAGS) -c src/env/Env.cpp -o out/Env.o

out/SymAtom.o: src/sexpr/SymAtom.cpp
	$(CXX) $(CXXFLAGS) -c src/sexpr/SymAtom.cpp -o out/SymAtom.o

out/SExprs.o: src/sexpr/SExprs.cpp
	$(CXX) $(CXXFLAGS) -c src/sexpr/SExprs.cpp -o out/SExprs.o

out/SExpr.o: src/sexpr/SExpr.cpp
	$(CXX) $(CXXFLAGS) -c src/sexpr/SExpr.cpp -o out/SExpr.o

out/NilAtom.o: src/sexpr/NilAtom.cpp
	$(CXX) $(CXXFLAGS) -c src/sexpr/NilAtom.cpp -o out/NilAtom.o

out/IntAtom.o: src/sexpr/IntAtom.cpp
	$(CXX) $(CXXFLAGS) -c src/sexpr/IntAtom.cpp -o out/IntAtom.o

out/ClosureAtom.o: src/sexpr/ClosureAtom.cpp
	$(CXX) $(CXXFLAGS) -c src/sexpr/ClosureAtom.cpp -o out/ClosureAtom.o

out/BoolAtom.o: src/sexpr/BoolAtom.cpp
	$(CXX) $(CXXFLAGS) -c src/sexpr/BoolAtom.cpp -o out/BoolAtom.o

out/Atom.o: src/sexpr/Atom.cpp
	$(CXX) $(CXXFLAGS) -c src/sexpr/Atom.cpp -o out/Atom.o

test: testHof testRecur testCons testCombine
	
testHof: out/lisp tests/hof.in tests/hof.expect
	out/lisp < tests/hof.in > tests/hof.out
	diff tests/hof.expect tests/hof.out

testCons: out/lisp tests/cons.in tests/cons.expect
	out/lisp < tests/cons.in > tests/cons.out
	diff tests/cons.expect tests/cons.out

testRecur: out/lisp tests/recur.in tests/recur.expect
	out/lisp < tests/recur.in > tests/recur.out
	diff tests/recur.expect tests/recur.out

testCombine: out/lisp tests/combine.in tests/combine.expect
	out/lisp < tests/combine.in > tests/combine.out
	diff tests/combine.expect tests/combine.out

clean:
	rm out/lisp out/*.o tests/*.out