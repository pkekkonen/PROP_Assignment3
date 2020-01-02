/***
A skeleton for Assignment 3 on PROP HT2019 at DSV/SU.
Peter Idestam-Almquist, 2019-12-09.
***/

/*** 
Load the tokenizer (tokenize/2) and the filewriter (write_to_file/3).
***/
:- [tokenizer].
:- [filewriter].


/***
The top level predicate run/2 of the solution.
To be called like this:
?- run('program2.txt','myparsetree2.txt').
***/
run(InputFile,OutputFile):-
	tokenize(InputFile,Program),
	parse(ParseTree,Program,[]),
	evaluate(ParseTree,[],VariablesOut), 
	write_to_file(OutputFile,ParseTree,VariablesOut).
	
/***
parse(-ParseTree)-->
	A grammar defining your programming language,
	and returning a parse tree.
***/

/* WRITE YOUR CODE FOR THE PARSER HERE */
block-->[symbol_code(123)], stmts, [symbol_code(125)].
stmts-->assign, stmts.
stmts-->[].
assign-->id, [symbol_code(61)], expr, symbol_code(59).
expr-->term, [symbol_code(43)], expr.
expr-->term, [symbol_code(45)], expr. 
expr-->term.
term-->factor, [symbol_code(42)], term.
term-->factor, [symbol_code(47)], term.
term-->factor.                    
factor-->int.
factor-->id.
factor-->[symbol_code(40)], expr, [symbol_code(41)]. 

id-->[X], {atom(X)}.
int-->[X], {integer(X)}.

	
/***
evaluate(+ParseTree,+VariablesIn,-VariablesOut):-
	Evaluates a parse-tree and returns the state of the program
	after evaluation as a list of variables and their values in 
	the form [var = value, ...].
***/

/* WRITE YOUR CODE FOR THE EVALUATOR HERE */
