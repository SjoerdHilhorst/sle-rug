module CST2AST

import Syntax;
import AST;

import ParseTree;
import String;
import Boolean;
import IO;
/*
 * Implement a mapping from concrete syntax trees (CSTs) to abstract syntax trees (ASTs)
 *
 * - Use switch to do case distinction with concrete patterns (like in Hack your JS) 
 * - Map regular CST arguments (e.g., *, +, ?) to lists 
 *   (NB: you can iterate over * / + arguments using `<-` in comprehensions or for-loops).
 * - Map lexical nodes to Rascal primitive types (bool, int, str)
 * - See the ref example on how to obtain and propagate source locations.
 */

AForm cst2ast(start[Form] sf) {
  Form f = sf.top; // remove layout before and after form
  return form("<f.id>", [cst2ast(q) | q <- f.questions], src=f@\loc); 
}

AQuestion cst2ast(Question q) {
  switch (q) {
  	case (Question)`<Str name> <Id i> : <Type t>`:
  		return question("<name>", id("<i>", src=i@\loc), cst2ast(t), src=q@\loc);
  	case (Question)`<Str name> <Id i> : <Type t> = <Expr e>`:
  		return cquestion("<name>", id("<i>", src=i@\loc), cst2ast(t), cst2ast(e), src=q@\loc);
  	case (Question) `if ( <Expr e> ) { <Question* qs > }`: 
  		return cond(cst2ast(e), [cst2ast(q) | Question q <- qs], [], src=q@\loc);
  	case (Question) `if ( <Expr e> ) { <Question* qs1> } else { <Question* qs2>}`:
  		return cond(cst2ast(e), [cst2ast(q) | Question q <- qs1], [cst2ast(q) | Question q <- qs2], src=q@\loc);
  	
  	
  	default: throw "Unhandled question: <q>";
  		
  }
}

AExpr boolOrInt(AType t){
	switch(t){
		case boolean(): return boolean(false);
		case \integer(): return integer(0); 
		default: throw "Unhandled expression: <t>";
	}
}

AExpr cst2ast(Expr e) {
  switch (e) {
    case (Expr)`<Id x>`: return ref(id("<x>", src=x@\loc), src=x@\loc);
    case (Expr)`( <Expr e> )`: return brackets(cst2ast(e), src=e@\loc);
    case (Expr)`! <Expr e>`: return not(cst2ast(e), src=e@\loc);
    case (Expr)`<Expr l> * <Expr r>`: return mul(cst2ast(l), cst2ast(r), src=e@\loc);
    case (Expr)`<Expr l> / <Expr r>`: return div(cst2ast(l), cst2ast(r), src=e@\loc);
    case (Expr)`<Expr l> + <Expr r>`: return add(cst2ast(l), cst2ast(r), src=e@\loc);
    case (Expr)`<Expr l> - <Expr r>`: return subtract(cst2ast(l), cst2ast(r), src=e@\loc);
    case (Expr)`<Expr l> \> <Expr r>`: return greater(cst2ast(l), cst2ast(r), src=e@\loc);
    case (Expr)`<Expr l> \< <Expr r>`: return less(cst2ast(l), cst2ast(r), src=e@\loc);
    case (Expr)`<Expr expr1> \>= <Expr expr2>`: return geq(cst2ast(expr1), cst2ast(expr2), src=e@\loc);
    case (Expr)`<Expr l> \<= <Expr r>`: return leq(cst2ast(l), cst2ast(r), src=e@\loc);
    case (Expr)`<Expr l> == <Expr r>`: return equals(cst2ast(l), cst2ast(r), src=e@\loc);
    case (Expr)`<Expr l> != <Expr r>`: return notequals(cst2ast(l), cst2ast(r), src=e@\loc);
    case (Expr)`<Expr l> || <Expr r>`: return or(cst2ast(l), cst2ast(r), src=e@\loc);
    case (Expr)`<Expr l> && <Expr r>`: return and(cst2ast(l), cst2ast(r), src=e@\loc);
    case (Expr)`<Int i>`: return integer(toInt("<i>"), src=i@\loc);
    case (Expr)`<Bool b>`: return boolean(fromString("<b>"), src=b@\loc);
    
    default: throw "Unhandled expression: <e>";
  }
}

AType cst2ast(Type t) {
  switch(t) {
  	case (Type) `boolean`: return boolean();
  	case (Type) `integer`: return integer();
  }
  throw "Not yet implemented";
}
