module Check

import AST;
import Resolve;
import Message; // see standard library
import IO;
import Set;

data Type
  = tint()
  | tbool()
  | tstr()
  | tunknown()
  ;

// the type environment consisting of defined questions in the form 
alias TEnv = rel[loc def, str name, str label, Type \type];

// To avoid recursively traversing the form, use the `visit` construct
// or deep match (e.g., `for (/question(...) := f) {...}` ) 
TEnv collect(AForm f) {
  return {<q.src, id.name, question , AType2Type(t)>| /q:cquestion(str question, AId id, AType t, _) := f }
  + {<q.src, id.name, question , AType2Type(t)>| /q:question(str question, AId id, AType t) := f};
}

Type AType2Type(AType t){
  switch(t){
  	case boolean(): {
  		//println(" type: <t> tbool");
  	
  		return tbool();
  	}
  	case integer(): {
  	  		//println(" type: <t> tint");
  	 	return tint();
  	}
  	default: {return tunknown();}
  	}
}

set[Message] check(AForm f, TEnv tenv, UseDef useDef) {
	//println( (tenv));
	x=  ({} | it + check(q, tenv, useDef) | AQuestion q <- f.questions); 
  	//println("---------");
  	//println( (tenv));
  	  return x;
  	
  
}

// - produce an error if there are declared questions with the same name but different types.
// - duplicate labels should trigger a warning 
// - the declared type computed questions should match the type of the expression.
set[Message] check(AQuestion q, TEnv tenv, UseDef useDef) {
	set[Message] msgs = {};
	//println( size({qs | <src, "yes ",qs, Type t> <- tenv}));
	
	
	switch (q){
		case question(str label, i:id(str n), AType \type):
		 return {error("Duplicate question", i.src) | size({t | <_, n, qs, Type t> <- tenv}) > 1}
	      + {error("Duplicate question label", q.src) | size({l | <loc l, _, label, _> <- tenv}) > 1};
	    case cquestion(str label, i:id(str n), AType \type, AExpr expr):
	      return {error("Duplicate question", i.src) | size({t | <_, n, qs, Type t> <- tenv}) > 1}
	      + {error("Duplicate question label: <label>", q.src) | size({l | <loc l, _, label, _> <- tenv}) > 1}
	      + {error("Expression type does not match question type", q.src) | typeOf(expr, tenv, useDef) != AType2Type(\type)}
	      + check(expr, tenv, useDef);	      
	  	case cond(AExpr c, list[AQuestion] thenq, list[AQuestion] elseq):
	  	{ //println("<typeOf(c, tenv, useDef)>, <c>");
	      return {error("If condition expression is not a boolean type", c.src)| typeOf(c, tenv, useDef) != tbool()}
      		+ ( {} | it + check(tq, tenv, useDef) | AQuestion tq <- thenq )
      		+ ( {} | it + check(eq, tenv, useDef) | AQuestion eq <- elseq );
      	}
      			      
	    default: return {};
	}
}

// Check operand compatibility with operators.
// E.g. for an addition node add(lhs, rhs), 
//   the requirement is that typeOf(lhs) == typeOf(rhs) == tint()
set[Message] check(AExpr e, TEnv tenv, UseDef useDef) {
  set[Message] msgs = {};
  
  switch (e) {
  	case not(AExpr ex):{
  		if(typeOf(e, tenv,useDef) != tbool()){
  			msgs += error("Operand Uncompatible with Operators", e.src);
  		}
  		msgs += check(ex, tenv, useDef);
  	}
    	
    case div(AExpr expr1, AExpr expr2): {
  		if(typeOf(expr1, tenv,useDef) != typeOf(expr2, tenv,useDef) || typeOf(expr1, tenv,useDef) != tint()){
  			msgs += error("Operand Uncompatible with Operators", e.src);
  		}
  		msgs += check(expr1, tenv, useDef);
  		msgs += check(expr2, tenv, useDef);
    }
    case mul(AExpr expr1, AExpr expr2): {
  		if(typeOf(expr1, tenv,useDef) != typeOf(expr2, tenv,useDef) || typeOf(expr1, tenv,useDef) != tint()){
  			msgs += error("Operand Uncompatible with Operators", e.src);
  		}
  		msgs += check(expr1, tenv, useDef);
  		msgs += check(expr2, tenv, useDef);
    }
    case add(AExpr expr1, AExpr expr2): {
  		if(typeOf(expr1, tenv,useDef) != typeOf(expr2, tenv,useDef) || typeOf(expr1, tenv,useDef) != tint()){
  			msgs += error("Operand Uncompatible with Operators", e.src);
  		}
  		msgs += check(expr1, tenv, useDef);
  		msgs += check(expr2, tenv, useDef);
    }
    case subtract(AExpr expr1, AExpr expr2): {
  		if(typeOf(expr1, tenv,useDef) != typeOf(expr2, tenv,useDef) || typeOf(expr1, tenv,useDef) != tint()){
  			msgs += error("Operand Uncompatible with Operators", e.src);
  		}
  		msgs += check(expr1, tenv, useDef);
  		msgs += check(expr2, tenv, useDef);
    }
    case less(AExpr expr1, AExpr expr2): {
  		if(typeOf(expr1, tenv,useDef) != typeOf(expr2, tenv,useDef) || typeOf(expr1, tenv,useDef) != tbool()){
  			msgs += error("Operand Uncompatible with Operators", e.src);
  		}
  		msgs += check(expr1, tenv, useDef);
  		msgs += check(expr2, tenv, useDef);
    }
    case greater(AExpr expr1, AExpr expr2): {
  		if(typeOf(expr1, tenv,useDef) != typeOf(expr2, tenv,useDef) || typeOf(expr1, tenv,useDef) != tbool()){
  			msgs += error("Operand Uncompatible with Operators", e.src);
  		}
  		msgs += check(expr1, tenv, useDef);
  		msgs += check(expr2, tenv, useDef);
    }
    case leq(AExpr expr1, AExpr expr2): {
  		if(typeOf(expr1, tenv,useDef) != typeOf(expr2, tenv,useDef) || typeOf(expr1, tenv,useDef) != tbool()){
  			msgs += error("Operand Uncompatible with Operators", e.src);
  		}
  		msgs += check(expr1, tenv, useDef);
  		msgs += check(expr2, tenv, useDef);
    }
    case geq(AExpr expr1, AExpr expr2): {
  		if(typeOf(expr1, tenv,useDef) != typeOf(expr2, tenv,useDef) || typeOf(expr1, tenv,useDef) != tbool()){
  			msgs += error("Operand Uncompatible with Operators", e.src);
  		}
  		msgs += check(expr1, tenv, useDef);
  		msgs += check(expr2, tenv, useDef);
    }
    case equals(AExpr expr1, AExpr expr2): {
  		if(typeOf(expr1, tenv,useDef) != typeOf(expr2, tenv,useDef) || typeOf(expr1, tenv,useDef) != tbool()){
  			msgs += error("Operand Uncompatible with Operators", e.src);
  		}
  		msgs += check(expr1, tenv, useDef);
  		msgs += check(expr2, tenv, useDef);
    }
    case notequals(AExpr expr1, AExpr expr2): {
  		if(typeOf(expr1, tenv,useDef) != typeOf(expr2, tenv,useDef) || typeOf(expr1, tenv,useDef) != tbool()){
  			msgs += error("<e.src>" + "Operand Uncompatible with Operators");
  		}
  		msgs += check(expr1, tenv, useDef);
  		msgs += check(expr2, tenv, useDef);
    }
    case and(AExpr expr1, AExpr expr2): {
  		if(typeOf(expr1, tenv,useDef) != typeOf(expr2, tenv,useDef) || typeOf(expr1, tenv,useDef) != tbool()){
  			msgs += error("Operand Uncompatible with Operators", e.src);
  		}
  		msgs += check(expr1, tenv, useDef);
  		msgs += check(expr2, tenv, useDef);
    }
    case or(AExpr expr1, AExpr expr2): {
  		if(typeOf(expr1, tenv,useDef) != typeOf(expr2, tenv,useDef) || typeOf(expr1, tenv,useDef) != tbool()){
  			msgs += error("Operand Uncompatible with Operators", e.src);
  		}
  		msgs += check(expr1, tenv, useDef);
  		msgs += check(expr2, tenv, useDef);
    }
    case ref(AId _, src = loc u):
      msgs += { error("Undeclared question", u) | useDef[u] == {} };
  } 
  return msgs; 
}

Type typeOf(AExpr e, TEnv tenv, UseDef useDef) {
  switch (e) {
    case brackets(AExpr ex):
      return typeOf(ex, tenv, useDef);
    case not(AExpr ex): {
      if(typeOf(ex, tenv, useDef) == tbool()){
      	return tbool();
      }
      else{
      	return tunknown();
      }
    }
    case div(AExpr expr1, AExpr expr2): {
      if(typeOf(expr1, tenv, useDef) == tint() && typeOf(expr2, tenv, useDef) == tint()){
      	return tint();
      }
      else{
      	return tunknown();
      }
    }
    case mul(AExpr expr1, AExpr expr2): {
      if(typeOf(expr1, tenv, useDef) == tint() && typeOf(expr2, tenv, useDef) == tint()){
      	return tint();
      }
      else{
      	return tunknown();
      }
    }
    case add(AExpr expr1, AExpr expr2): {
      if(typeOf(expr1, tenv, useDef) == tint() && typeOf(expr2, tenv, useDef) == tint()){
      	return tint();
      }
      else{
      	return tunknown();
      }
    }
    case subtract(AExpr expr1, AExpr expr2): {
      if(typeOf(expr1, tenv, useDef) == tint() && typeOf(expr2, tenv, useDef) == tint()){
      	return tint();
      }
      else{
      	return tunknown();
      }
    }
    case less(AExpr expr1, AExpr expr2): {
      if(typeOf(expr1, tenv, useDef) == typeOf(expr2, tenv, useDef) && typeOf(expr1, tenv, useDef) != tunknown()){
      	return tbool();
      }
      else{
      	return tunknown();
      }
    }
    case greater(AExpr expr1, AExpr expr2): {
      if(typeOf(expr1, tenv, useDef) == typeOf(expr2, tenv, useDef) && typeOf(expr1, tenv, useDef) != tunknown()){
      	return tbool();
      }
      else{
      	return tunknown();
      }
    }
    case leq(AExpr expr1, AExpr expr2): {
      if(typeOf(expr1, tenv, useDef) == typeOf(expr2, tenv, useDef) && typeOf(expr1, tenv, useDef) != tunknown()){
      	return tbool();
      }
      else{
      	return tunknown();
      }
    }
    case geq(AExpr expr1, AExpr expr2): {
      if(typeOf(expr1, tenv, useDef) == typeOf(expr2, tenv, useDef) && typeOf(expr1, tenv, useDef) != tunknown()){
      	return tbool();
      }
      else{
      	return tunknown();
      }
    }
    case equals(AExpr expr1, AExpr expr2): {
      if(typeOf(expr1, tenv, useDef) == typeOf(expr2, tenv, useDef) && typeOf(expr1, tenv, useDef) != tunknown()){
      	return tbool();
      }
      else{
      	return tunknown();
      }
    }
    case notequals(AExpr expr1, AExpr expr2): {
      if(typeOf(expr1, tenv, useDef) == typeOf(expr2, tenv, useDef) && typeOf(expr1, tenv, useDef) != tunknown()){
      	return tbool();
      }
      else{
      	return tunknown();
      }
    }
    case and(AExpr expr1, AExpr expr2): {
      if(typeOf(expr1, tenv, useDef) == tbool() && typeOf(expr2, tenv, useDef) == tbool()){
      	return tbool();
      }
      else{
      	return tunknown();
      }
    }
    case or(AExpr expr1, AExpr expr2): {
      if(typeOf(expr1, tenv, useDef) == tbool() && typeOf(expr2, tenv, useDef) == tbool()){
      	return tbool();
      }
      else{
      	return tunknown();
      }
    }
    case ref(id(str x, src = loc u)): { 
	//change the function because it had some weird behaviour for me
    ud = <u, loc d> <- useDef;
    tv = <d, x, _, Type t> <- tenv;
      if (ud && tv) {
  		t = [t | <d, x, _, Type t> <- tenv];
    	return t[0];  	
      }      
     }
    case integer(int n):
      return tint();
    case boolean(bool \bool):
      return tbool();

  }
  return tunknown(); 
}

/* 
 * Pattern-based dispatch style:
 * 
 * Type typeOf(ref(str x, src = loc u), TEnv tenv, UseDef useDef) = t
 *   when <u, loc d> <- useDef, <d, x, _, Type t> <- tenv
 *
 * ... etc.
 * 
 * default Type typeOf(AExpr _, TEnv _, UseDef _) = tunknown();
 *
 */
 
 
 

