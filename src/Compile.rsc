module Compile

import AST;
import Resolve;
import IO;
import Eval;
import lang::html5::DOM; // see standard library
import util::Math;

/*
 * Implement a compiler for QL to HTML and Javascript
 *
 * - assume the form is type- and name-correct
 * - separate the compiler in two parts form2html and form2js producing 2 files
 * - use string templates to generate Javascript
 * - use the HTML5Node type and the `str toString(HTML5Node x)` function to format to string
 * - use any client web framework (e.g. Vue, React, jQuery, whatever) you like for event handling
 * - map booleans to checkboxes, strings to textfields, ints to numeric text fields
 * - be sure to generate uneditable widgets for computed questions!
 * - if needed, use the name analysis to link uses to definitions
 */
HTML5Attr vmodel(value val) = html5attr("v-model", val);
HTML5Attr vif(value val) = html5attr("v-if", val);

void compile(AForm f) {
  writeFile(f.src[extension="js"].top, form2js(f));
  writeFile(f.src[extension="html"].top, toString(form2html(f)));
}

HTML5Node form2html(AForm f) {
  return html(
  	head(
  		title("<f.name>")	
  	),
  	body(
  		h1("<f.name>"),
  		div(
  			id("form"),
	  		form(
	  			([] | it + form2html(q) | AQuestion q <- f.questions)
	  			)
	  		)
  	),
  	 script(
  		src("https://cdn.jsdelivr.net/npm/vue/dist/vue.js")
  	),
  	script(
  		src(f.src[extension="js"].top.file)
  	)
  );
}

list[HTML5Node] form2html(AQuestion q){
	switch(q){
		case question(str q, AId aid, \integer()): 
			return [div(label(q), input(\type("number"), placeholder("Input integer"), vmodel(aid.name)))];
		case question(str q, AId aid, \boolean()): 
			return [div(label(q), select(\type("number"),option("false", \value(0)), option("true", \value(1)), vmodel(aid.name)))];
		case cquestion(str q, AId aid, AType t, AExpr expr): 
			return [p("<q>: ", p("{{<aid.name>()}}", id(aid.name)))];
		case cond(AExpr c, list[AQuestion] thenq, list[AQuestion] elseq):
			return [div(([vif(expr2js(c))] | it + form2html(q) | AQuestion q <- thenq))]+
			[div(([vif("!<expr2js(c)>")] | it + form2html(q) | AQuestion q <- elseq))];
	}
}

str initial(AQuestion q){
	switch(q){
		case question(str q, AId aid, \integer()): 
				return "<aid.name>: 0,\n";
		case question(str q, AId aid, \boolean()): 
				return "<aid.name>: false,\n";
		case cond(AExpr c, list[AQuestion] thenq, list[AQuestion] elseq):
			return ("" | it + initial(tq) | AQuestion tq <- thenq)
			 + ("" | it + initial(eq) | AQuestion eq <- elseq);					
		default: return "";
	}
}

str createFun(AQuestion q){
	switch(q){
		case cquestion(str q, AId aid, _, AExpr expr): 
			return "<aid.name>: function(){return <expr2js(expr)>},\n";
		case cond(AExpr c, list[AQuestion] thenq, list[AQuestion] elseq):{
			return ("" | it + createFun(tq) | AQuestion tq <- thenq)
			 + ("" | it + createFun(eq) | AQuestion eq <- elseq);	
			 }				
		default: return "";
	}
}

str form2js(AForm f) {
	str js = "app = new Vue({
				' el: \'#form\',
				' data: {";
				
	 for (AQuestion q <- f.questions){
  		js += initial(q);
  	}
  	js += "},\n methods: {";
  	js += ("" | it + createFun(q) | AQuestion q <- f.questions);
  	js += "}})";
	return js;
}

str expr2js(AExpr e) {
  switch (e) {
    case ref(id(str x)): return "parseFloat(this.<x>)";
    case boolean(true): return "true";
    case boolean(false): return "false";
    case brackets(AExpr e): return "(" + expr2js(e)+ ")";
    
    case integer(int n): return toString(n);
    case mul(AExpr l, AExpr r): return expr2js(l) + "*" +  expr2js(r);
    case div(AExpr l, AExpr r): return expr2js(l) + "/" +  expr2js(r);
    case add(AExpr l, AExpr r): return expr2js(l) + "+" +  expr2js(r);
    case subtract(AExpr l, AExpr r): return expr2js(l) + "-" +  expr2js(r);
    case greater(AExpr l, AExpr r): return expr2js(l) + "\>" +  expr2js(r);
    case less(AExpr l, AExpr r): return expr2js(l) + "\<" +  expr2js(r);
    case geq(AExpr l, AExpr r): return expr2js(l) + "\>=" +  expr2js(r);
    case leq(AExpr l, AExpr r): return expr2js(l) + "\<=" +  expr2js(r);
    case equals(AExpr l, AExpr r): return expr2js(l) + "==" +  expr2js(r);
    case notequals(AExpr l, AExpr r): return expr2js(l) + "!=" +  expr2js(r);
    case and(AExpr l, AExpr r): return expr2js(l) + "&&" +  expr2js(r);
    case or(AExpr l, AExpr r): return expr2js(l) + "||" +  expr2js(r);
    case not(Aexpr e): return "!" + expr2js(e);
    
        // etc.
    
    default: throw "Unsupported expression <e>";
  }
}
