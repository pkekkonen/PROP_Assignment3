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
***/

run(InputFile,OutputFile):-
	tokenize(InputFile,Program),
	parse(ParseTree, Program, []),
	evaluate(ParseTree,[],VariablesOut),
	write_to_file(OutputFile,ParseTree,VariablesOut).

/* WRITE YOUR CODE FOR THE PARSER HERE */

parse(-ParseTree)--> block.

block(block(symbol_code(123), ST, symbol_code(125)))-->[symbol_code(123)], stmts(ST), [symbol_code(125)].
stmts(statements(AS, ST))-->assign(AS), stmts(ST).
stmts(statements)-->[].
assign(assign(ID, symbol_code(61), EX, symbol_code(59)))-->id(ID), [symbol_code(61)], expr(EX), [symbol_code(59)].
expr(expression(T, symbol_code(43), EX))-->term(T), [symbol_code(43)], expr(EX).
expr(expression(T, symbol_code(45) EX))-->term(T), [symbol_code(45)], expr(EX).
expr(expression(T))-->term(T).
term(term(F, symbol_code(42), T))-->factor(F), [symbol_code(42)], term(T).
term(term(F, symbol_code(47), T))-->factor(F), [symbol_code(47)], term(T).
term(term(F))-->factor(F).
factor(factor(I))-->int(I).
factor(factor(I))-->id(I).
factor(factor(symbol_code(40), EX, symbol_code(41)))-->[symbol_code(40)], expr(EX), [symbol_code(41)].

id(id(X))-->[X], {atom(X)}.
int(int(X))-->[X], {integer(X)}.


	
/***
evaluate(+ParseTree,+VariablesIn,-VariablesOut):-
	Evaluates a parse-tree and returns the state of the program
	after evaluation as a list of variables and their values in 
	the form [var = value, ...].
***/

/* WRITE YOUR CODE FOR THE EVALUATOR HERE */
