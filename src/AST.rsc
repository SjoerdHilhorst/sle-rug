module AST

/*
 * Define Abstract Syntax for QL
 *
 * - complete the following data types
 * - make sure there is an almost one-to-one correspondence with the grammar
 */

data AForm(loc src = |tmp:///|)
  = form(str name, list[AQuestion] questions)
  ; 

data AQuestion(loc src = |tmp:///|)
 = cquestion(str q, AId id, AType \type, AExpr Expr)
 | question(str q, AId id, AType \type)
 | cond(AExpr c, list[AQuestion] thenq, list[AQuestion] elseq); 

data AExpr(loc src = |tmp:///|)
  = ref(AId id)
  | brackets(AExpr expr)
  | not(AExpr expr)
  | mul(AExpr left, AExpr right)
  | div(AExpr left, AExpr right)
  | add(AExpr left, AExpr right)
  | subtract(AExpr left, AExpr right)
  | less(AExpr left, AExpr right)
  | greater(AExpr left, AExpr right)
  | leq(AExpr left, AExpr right)
  | geq(AExpr left, AExpr right)
  | or(AExpr left, AExpr right)
  | and(AExpr left, AExpr right)
  | equals(AExpr left, AExpr right)
  | notequals(AExpr left, AExpr right)
  | integer(int n)
  | boolean(bool \bool)
  ;

data AId(loc src = |tmp:///|)
  = id(str name);

data AType(loc src = |tmp:///|)
  = \boolean()
  | \integer();
