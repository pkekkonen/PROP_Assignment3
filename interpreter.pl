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



parse(block(left_curly, ST, right_curly))-->['{'], stmts(ST), ['}'].
stmts(statements(AS, ST))-->assign(AS), stmts(ST).
stmts(statements) --> [].
assign(assignment(ID, assign_op, EX, semicolon))-->id(ID), [=], expr(EX), [;].
expr(expression(T, add_op, EX))-->term(T), [+], expr(EX).
expr(expression(T, sub_op, EX))-->term(T), [-], expr(EX).
expr(expression(T))-->term(T).
term(term(F, mult_op, T))-->factor(F), [*], term(T).
term(term(F, div_op, T))-->factor(F), [/], term(T).
term(term(F))-->factor(F).
factor(factor(INT))-->integer(INT).
factor(factor(IDENT))-->id(IDENT).
factor(factor(left_paren, EX, right_paren))-->['('], expr(EX), [')'].

id(ident(X))-->[X], {atom(X)}.
integer(int(X))-->[X], {integer(X)}.


	
/***
evaluate(+ParseTree,+VariablesIn,-VariablesOut):-
	Evaluates a parse-tree and returns the state of the program
	after evaluation as a list of variables and their values in 
	the form [var = value, ...].
***/

evaluate(block(left_curly, ST, right_curly), VariablesIn, VariablesOut) :-
    evaluate(ST, VariablesIn, Result),
    VariablesOut = Result.

evaluate(statements(AS, ST), VariablesIn, VariablesOut) :-
    evaluate(AS,VariablesIn, R1), evaluate(ST, [R1 | VariablesIn], R2), VariablesOut = [R1|R2].

evaluate(statements, VariablesIn, VariablesOut).

evaluate(assignment(ID, assign_op, EX, semicolon), VariablesIn, VariablesOut) :-
    evaluate(ID, VariablesIn, R1), evaluate(EX, VariablesIn, R2), built_equality_structure(R1, R2, VariablesOut).

evaluate(expression(T, add_op, EX), VariablesIn, VariablesOut) :-
    evaluate(T, VariablesIn, R1), evaluate(EX, VariablesIn, R2), VariablesOut is R1 + R2.

evaluate(expression(T, sub_op, EX), VariablesIn, VariablesOut) :-
    evaluate(T, VariablesIn, R1), evaluate(EX, VariablesIn, R2), VariablesOut is R1 - R2.

evaluate(expression(T, sub_op, EX(Term, sub_op, Expr)), VariablesIn, VariablesOut) :-
evaluate(T, VariablesIn, R1), evaluate(EX(Term, sub_op, Expr), VariablesIn, R2), VariablesOut is R1 - R2.

evaluate(expression(T), VariablesIn, VariablesOut) :-
    evaluate(T, VariablesIn, VariablesOut).

evaluate(term(F, mult_op, T), VariablesIn, VariablesOut) :-
    evaluate(F, VariablesIn, R1), evaluate(T, VariablesIn, R2), VariablesOut is R1*R2.

evaluate(term(F, div_op, T), VariablesIn, VariablesOut) :-
    evaluate(F, VariablesIn, R1), evaluate(T, VariablesIn, R2), VariablesOut is R1/R2.

evaluate(term(F), VariablesIn, VariablesOut) :-
    evaluate(F, VariablesIn, VariablesOut).

evaluate(factor(INT), VariablesIn, VariablesOut) :-
    evaluate(INT, VariablesIn, VariablesOut).

evaluate(factor(IDENT), VariablesIn, VariablesOut) :-
     find_value(Res, VariablesOut, VariablesIn), evaluate(IDENT, VariablesIn, Res).

evaluate(factor(left_paren, EX, right_paren), VariablesIn, VariablesOut) :-
    evaluate(EX, VariablesIn, VariablesOut).

evaluate(ident(X), VariablesIn, VariablesOut) :-
    VariablesOut = X.


evaluate(int(X), VariablesIn, VariablesOut) :-
    VariablesOut = X.

built_equality_structure(Id,Value,Id = Value).


find_value(Id, Value, [First|_]) :- functor(Str, First, 0), get_id(Str, Id), get_value(Str, Value).
find_value(Id, Value, [_First| Variables]) :- find_value(Id, Value, Variables).


get_id(String, Result) :-
sub_atom(String, 0, Len, After, Result), sub_atom(String, Len, 1, AfterEqual, =), After is AfterEqual + 1.

get_value(String, Result) :-
sub_atom(String, Before, Len, 0, Result), sub_atom(String, BeforeEqual, 1, Len, =), Before is BeforeEqual + 1.


	
