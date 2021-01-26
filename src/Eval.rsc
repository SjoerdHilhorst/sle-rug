module Eval

import AST;
import Resolve;
import IO;

/*
 * Implement big-step semantics for QL
 */
 
// NB: Eval may assume the form is type- and name-correct.


// Semantic domain for expressions (values)
data Value
  = vint(int n)
  | vbool(bool b)
  | vstr(str s)
  ;

// The value environment
alias VEnv = map[str name, Value \value];

// Modeling user input
data Input
  = input(str question, Value \value);
  
// produce an environment which for each question has a default value
// (e.g. 0 for int, "" for str etc.)
VEnv initialEnv(AForm f) = ( n: typeDef(t) | /question(_, id(str n), AType t) <- f );

Value typeDef(integer()) = vint(0);
Value typeDef(boolean()) = vbool(false);


// Because of out-of-order use and declaration of questions
// we use the solve primitive in Rascal to find the fixpoint of venv.
VEnv eval(AForm f, Input inp, VEnv venv) {
	println(inp);
  return solve (venv) {
    venv = evalOnce(f, inp, venv);
  }
}

VEnv evalOnce(AForm f, Input inp, VEnv venv) {
  return (venv | eval(q, inp, it) | q <- f.questions); 
}

VEnv eval(AQuestion q, Input inp, VEnv venv) {
  // evaluate conditions for branching,
  // evaluate inp and computed questions to return updated VEnv
  switch(q){
  	case question(str q, AId id, AType \type):{
  		return venv + (id.name: inp.\value | inp.question == q);
  		}
  	case cquestion(str q, AId id, AType \type, AExpr expr):
	  	return venv + (id.name: eval(expr, venv));
	case cond(AExpr c, list[AQuestion] thenq, list[AQuestion] elseq): {
		println(eval(c, venv));
		if (vbool(true) := eval(c, venv)){
			return ( venv | eval(tq, inp, it) | tq <- thenq );
		} else {
			return ( venv | eval(eq, inp, it) | eq <- elseq );
		}
  	}
  }
  return venv; 
}

Value eval(AExpr e, VEnv venv) {
  switch (e) {
    case ref(id(str x)): return venv[x];
    case boolean(bool b): return vbool(b);
    case integer(int n): return vint(n);
    case not(AExpr ex): return vbool(!eval(ex, venv).b);
    case mul(AExpr l, AExpr r): return vint(eval(l, venv).n * eval(r, venv).n);
    case div(AExpr l, AExpr r): return vint(eval(l, venv).n / eval(r, venv).n);
    case add(AExpr l, AExpr r): return vint(eval(l, venv).n + eval(r, venv).n);
    case subtract(AExpr l, AExpr r): return vint(eval(l, venv).n - eval(r, venv).n);
    case greater(AExpr l, AExpr r): return vbool(eval(l, venv).n > eval(r, venv).n);
    case less(AExpr l, AExpr r): return vbool(eval(l, venv).n < eval(r, venv).n);
    case geq(AExpr l, AExpr r): return vbool(eval(l, venv).n >= eval(r, venv).n);
    case leq(AExpr l, AExpr r): return vbool(eval(l, venv).n <= eval(r, venv).n);
    case equals(AExpr l, AExpr r): return vbool(eval(l, venv).n == eval(r, venv).n);
    case notequals(AExpr l, AExpr r): return vbool(eval(l, venv).n != eval(r, venv).n);
    case and(AExpr l, AExpr r): return vbool(eval(l, venv).b && eval(r, venv).b);
    case or(AExpr l, AExpr r): return vbool(eval(l, venv).b || eval(r, venv).b);
        // etc.
    
    default: throw "Unsupported expression <e>";
  }
}