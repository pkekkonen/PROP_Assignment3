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
    evaluatear(ID, VariablesIn, R1), evaluate(EX, VariablesIn, R2), built_equality_structure(R1, R2, VariablesOut).

evaluate(expression(T, add_op, EX), VariablesIn, VariablesOut) :-
    evaluate(T, VariablesIn, R1), evaluate(EX, VariablesIn, R2, R1, 'plus'), VariablesOut is R2.

evaluate(expression(T, add_op, EX), VariablesIn, VariablesOut, ValueSoFar, LastOp) :-
    LastOp = 'plus', evaluate(T, VariablesIn, R1), NewValuesSoFar is ValueSoFar + R1, evaluate(EX, VariablesIn, R2, NewValuesSoFar, 'plus'), VariablesOut is R2;
    LastOp = 'minus', evaluate(T, VariablesIn, R1), NewValuesSoFar is ValueSoFar - R1, evaluate(EX, VariablesIn, R2, NewValuesSoFar, 'plus'), VariablesOut is R2;
    LastOp = 'mult', evaluate(T, VariablesIn, R1), NewValuesSoFar is ValueSoFar * R1, evaluate(EX, VariablesIn, R2, NewValuesSoFar, 'plus'), VariablesOut is R2;
    LastOp = 'div', evaluate(T, VariablesIn, R1), NewValuesSoFar is ValueSoFar / R1, evaluate(EX, VariablesIn, R2, NewValuesSoFar, 'plus'), VariablesOut is R2.

evaluate(expression(T, sub_op, EX), VariablesIn, VariablesOut) :-
    evaluate(T, VariablesIn, R1), evaluate(EX, VariablesIn, R2, R1, 'minus'), VariablesOut is R2.

evaluate(expression(T, sub_op, EX), VariablesIn, VariablesOut, ValueSoFar, LastOp) :-
    LastOp = 'plus', evaluate(T, VariablesIn, R1), NewValuesSoFar is ValueSoFar + R1, evaluate(EX, VariablesIn, R2, NewValuesSoFar, 'minus'), VariablesOut is R2;
    LastOp = 'minus', evaluate(T, VariablesIn, R1), NewValuesSoFar is ValueSoFar - R1, evaluate(EX, VariablesIn, R2, NewValuesSoFar, 'minus'), VariablesOut is R2;
    LastOp = 'mult', evaluate(T, VariablesIn, R1), NewValuesSoFar is ValueSoFar * R1, evaluate(EX, VariablesIn, R2, NewValuesSoFar, 'minus'), VariablesOut is R2;
    LastOp = 'div', evaluate(T, VariablesIn, R1), NewValuesSoFar is ValueSoFar / R1, evaluate(EX, VariablesIn, R2, NewValuesSoFar, 'minus'), VariablesOut is R2.

evaluate(expression(T), VariablesIn, VariablesOut) :-
    evaluate(T, VariablesIn, VariablesOut).

evaluate(expression(T), VariablesIn, VariablesOut, ValueSoFar, LastOp) :-
    LastOp = 'plus', evaluate(T, VariablesIn, Res), VariablesOut is ValueSoFar+Res;
    LastOp = 'minus', evaluate(T, VariablesIn, Res), VariablesOut is ValueSoFar-Res;
    LastOp = 'mult', evaluate(T, VariablesIn, Res), VariablesOut is ValueSoFar*Res;
    LastOp = 'div', evaluate(T, VariablesIn, Res), VariablesOut is ValueSoFar/Res.

evaluate(term(F, mult_op, T), VariablesIn, VariablesOut) :-
    evaluate(F, VariablesIn, R1), evaluate(T, VariablesIn, R2, R1, 'mult'), VariablesOut is R2.

evaluate(term(F, mult_op, T), VariablesIn, VariablesOut, ValueSoFar, LastOp) :-
    LastOp = 'plus', evaluate(F, VariablesIn, R1), NewValuesSoFar is ValueSoFar + R1, evaluate(T, VariablesIn, R2, NewValuesSoFar, 'mult'), VariablesOut is R2;
    LastOp = 'minus', evaluate(F, VariablesIn, R1), NewValuesSoFar is ValueSoFar - R1, evaluate(T, VariablesIn, R2, NewValuesSoFar, 'mult'), VariablesOut is R2;
    LastOp = 'mult', evaluate(F, VariablesIn, R1), NewValuesSoFar is ValueSoFar * R1, evaluate(T, VariablesIn, R2, NewValuesSoFar, 'mult'), VariablesOut is R2;
    LastOp = 'div', evaluate(F, VariablesIn, R1), NewValuesSoFar is ValueSoFar / R1, evaluate(T, VariablesIn, R2, NewValuesSoFar, 'mult'), VariablesOut is R2.


evaluate(term(F, div_op, T), VariablesIn, VariablesOut) :-
    evaluate(F, VariablesIn, R1), evaluate(T, VariablesIn, R2, R1, 'div'), VariablesOut is R2.

evaluate(term(F, div_op, T), VariablesIn, VariablesOut, ValueSoFar, LastOp) :-
    LastOp = 'plus', evaluate(F, VariablesIn, R1), NewValuesSoFar is ValueSoFar + R1, evaluate(T, VariablesIn, R2, NewValuesSoFar, 'div'), VariablesOut is R2;
    LastOp = 'minus', evaluate(F, VariablesIn, R1), NewValuesSoFar is ValueSoFar - R1, evaluate(T, VariablesIn, R2, NewValuesSoFar, 'div'), VariablesOut is R2;
    LastOp = 'mult', evaluate(F, VariablesIn, R1), NewValuesSoFar is ValueSoFar * R1, evaluate(T, VariablesIn, R2, NewValuesSoFar, 'div'), VariablesOut is R2;
    LastOp = 'div', evaluate(F, VariablesIn, R1), NewValuesSoFar is ValueSoFar / R1, evaluate(T, VariablesIn, R2, NewValuesSoFar, 'div'), VariablesOut is R2.

evaluate(term(F), VariablesIn, VariablesOut) :-
    evaluate(F, VariablesIn, VariablesOut).

evaluate(term(F), VariablesIn, VariablesOut) :-
    LastOp = 'plus', evaluate(F, VariablesIn, Res), VariablesOut is ValueSoFar+Res;
    LastOp = 'minus', evaluate(F, VariablesIn, Res), VariablesOut is ValueSoFar-Res;
    LastOp = 'mult', evaluate(F, VariablesIn, Res), VariablesOut is ValueSoFar*Res;
    LastOp = 'div', evaluate(F, VariablesIn, Res), VariablesOut is ValueSoFar/Res.

evaluate(factor(INT), VariablesIn, VariablesOut) :-
    evaluate(INT, VariablesIn, VariablesOut).

evaluate(factor(IDENT), VariablesIn, VariablesOut) :-
    evaluate(IDENT, VariablesIn, Res), Res is VariablesOut.

evaluate(factor(left_paren, EX, right_paren), VariablesIn, VariablesOut) :-
    evaluate(EX, VariablesIn, VariablesOut).

evaluate(ident(X), VariablesIn, VariablesOut) :-
    my_member(X=VariablesOut, VariablesIn);
    not_member(X=_, VariablesIn), VariablesOut = 0.

evaluatear(ident(X), VariablesIn, VariablesOut) :-
VariablesOut = X.

evaluate(ident(X), VariablesIn, VariablesOut) :-
    VariablesOut = X.

evaluate(int(X), VariablesIn, VariablesOut) :-
    VariablesOut = X.

built_equality_structure(Id,Value,Id=Value).


my_member(X, [X|Xs]).
my_member(X, [_Y|Xs]):- member(X, Xs).









not_member(X, []).
not_member(X, [Y|Xs]) :- X \= Y, not_member(X, Xs).


	
