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
?- run('/Users/paulinakekkonen/Documents/Github/PROP_Assignment3/program2.txt','/Users/paulinakekkonen/Documents/Github/PROP_Assignment3/myparsetree2.txt').

run(InputFile,OutputFile):-
	tokenize(InputFile,Program),
	parse(ParseTree, Program, []),
	evaluate(ParseTree,[],VariablesOut),
	write_to_file(OutputFile,ParseTree,VariablesOut).

/* WRITE YOUR CODE FOR THE PARSER HERE */

parse(ParseTree(bl))--> block(bl).

block(Block(symbol_code(123), st, symbol_code(125)))-->[symbol_code(123)], stmts(st), [symbol_code(125)].
stmts(statements(as, st))-->assign(as), stmts(st).
stmts(statements)-->[].
assign(assign(i, symbol_code(61), ex symbol_code(59)))-->id(i), [symbol_code(61)], expr(ex), [symbol_code(59)].
expr(expression(t, symbol_code(43), ex))-->term(t), [symbol_code(43)], expr(ex).
expr(expression(t, symbol_code(45) ex))-->term(t), [symbol_code(45)], expr(ex).
expr(expression(t))-->term(t).
term(term(f, symbol_code(42), t))-->factor(f), [symbol_code(42)], term(t).
term(term(f, symbol_code(47), t))-->factor(f), [symbol_code(47)], term(t).
term(term(f))-->factor(f).
factor(factor(i))-->int(i).
factor(factor(i))-->id(i).
factor(factor(symbol_code(40), ex, symbol_code(41)))-->[symbol_code(40)], expr(ex), [symbol_code(41)].

id(id(X))-->[X], {atom(X)}.
int(int(X))-->[X], {integer(X)}.


	
/***
evaluate(+ParseTree,+VariablesIn,-VariablesOut):-
	Evaluates a parse-tree and returns the state of the program
	after evaluation as a list of variables and their values in 
	the form [var = value, ...].
***/

/* WRITE YOUR CODE FOR THE EVALUATOR HERE */
