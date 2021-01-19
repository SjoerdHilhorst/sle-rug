module Transform

import Syntax;
import Resolve;
import AST;
import IO;

/* 
 * Transforming QL forms
 */
 
 
/* Normalization:
 *  wrt to the semantics of QL the following
 *     q0: "" int; 
 *     if (a) { 
 *        if (b) { 
 *          q1: "" int; 
 *        } 
 *        q2: "" int; 
 *      }
 *
 *  is equivalent to
 *     if (true) q0: "" int;
 *     if (true && a && b) q1: "" int;
 *     if (true && a) q2: "" int;
 *
 * Write a transformation that performs this flattening transformation.
 *
 */
 
AForm flatten(AForm f) {
  return form(f.name, ([] | it + flatten(q, boolean(true)) | q <- f.questions)); 
}

list[AQuestion] flatten(AQuestion q, AExpr con){
	switch(q){
		case question(_,_,_): return [cond(con, [q], [])];
		case cquestion(_,_,_,_): return [cond(con, [q], [])];
		case cond(AExpr c, list[AQuestion] thenq, list[AQuestion] elseq):
			return ([] | it + flatten(tq, and(con ,c)) | tq <- thenq)
			+ ([] | it + flatten(eq, and(not(c) ,con)) | eq <- elseq);
		default: return [];
	}
}

void print(AForm f){
	visit(f){
		case question(_,AId id,_): 	println("<id.name>");
		case cquestion(_,AId id,_,AExpr expr): println("<id.name> <expr>");
		case cond(AExpr c, list[AQuestion] thenq, list[AQuestion] elseq): println("if <c>, then" );
	}
}

/* Rename refactoring:
 *
 * Write a refactoring transformation that consistently renames all occurrences of the same name.
 * Use the results of name resolution to find the equivalence class of a name.
 *
 */
 
 start[Form] rename(start[Form] f, loc useOrDef, str newName, UseDef useDef) {
   return f; 
 } 
 
 
 

