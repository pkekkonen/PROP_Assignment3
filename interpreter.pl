/***
Paulina Lagebjer Kekkonen (pala7490) and Ida Söderberg (idso0102)
***/

:- [tokenizer].
:- [filewriter].

run(InputFile,OutputFile):-
	tokenize(InputFile,Program),
	parse(ParseTree, Program, []),
	evaluate(ParseTree,[],VariablesOut),
	write_to_file(OutputFile,ParseTree,VariablesOut).

/* PARSER */

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

/* EVALUATOR */

evaluate(block(left_curly, ST, right_curly), VariablesIn, VariablesOut) :-
    evaluate(ST, VariablesIn, Result),
    VariablesOut = Result.

evaluate(statements(AS, ST), VariablesIn, VariablesOut) :-
    evaluate(AS,VariablesIn, R1), evaluate(ST, [R1 | VariablesIn], R2), append(R2, [R1], VariablesOut).

evaluate(statements, VariablesIn, VariablesOut).

evaluate(assignment(ID, assign_op, EX, semicolon), VariablesIn, VariablesOut) :-
    evaluate_assign_ident(ID, VariablesIn, R1), evaluate(EX, VariablesIn, R2), build_equality_structure(R1, R2, VariablesOut).

evaluate(expression(T, add_op, EX), VariablesIn, VariablesOut) :-
    evaluate(T, VariablesIn, R1), evaluate(EX, VariablesIn, R2, R1, '+'), VariablesOut is R2.

evaluate(expression(T, add_op, EX), VariablesIn, VariablesOut, ValueSoFar, LastOp) :-
    LastOp = '+', evaluate(T, VariablesIn, R1), NewValuesSoFar is ValueSoFar + R1, evaluate(EX, VariablesIn, R2, NewValuesSoFar, '+'), VariablesOut is R2;
    LastOp = '-', evaluate(T, VariablesIn, R1), NewValuesSoFar is ValueSoFar - R1, evaluate(EX, VariablesIn, R2, NewValuesSoFar, '+'), VariablesOut is R2;
    LastOp = '*', evaluate(T, VariablesIn, R1), NewValuesSoFar is ValueSoFar * R1, evaluate(EX, VariablesIn, R2, NewValuesSoFar, '+'), VariablesOut is R2;
    LastOp = '/', evaluate(T, VariablesIn, R1), NewValuesSoFar is ValueSoFar / R1, evaluate(EX, VariablesIn, R2, NewValuesSoFar, '+'), VariablesOut is R2.

evaluate(expression(T, sub_op, EX), VariablesIn, VariablesOut) :-
    evaluate(T, VariablesIn, R1), evaluate(EX, VariablesIn, R2, R1, '-'), VariablesOut is R2.

evaluate(expression(T, sub_op, EX), VariablesIn, VariablesOut, ValueSoFar, LastOp) :-
    LastOp = '+', evaluate(T, VariablesIn, R1), NewValuesSoFar is ValueSoFar + R1, evaluate(EX, VariablesIn, R2, NewValuesSoFar, '-'), VariablesOut is R2;
    LastOp = '-', evaluate(T, VariablesIn, R1), NewValuesSoFar is ValueSoFar - R1, evaluate(EX, VariablesIn, R2, NewValuesSoFar, '-'), VariablesOut is R2;
    LastOp = '*', evaluate(T, VariablesIn, R1), NewValuesSoFar is ValueSoFar * R1, evaluate(EX, VariablesIn, R2, NewValuesSoFar, '-'), VariablesOut is R2;
    LastOp = '/', evaluate(T, VariablesIn, R1), NewValuesSoFar is ValueSoFar / R1, evaluate(EX, VariablesIn, R2, NewValuesSoFar, '-'), VariablesOut is R2.

evaluate(expression(T), VariablesIn, VariablesOut) :-
    evaluate(T, VariablesIn, VariablesOut).

evaluate(expression(T), VariablesIn, VariablesOut, ValueSoFar, LastOp) :-
    LastOp = '+', evaluate(T, VariablesIn, Res), VariablesOut is ValueSoFar+Res;
    LastOp = '-', evaluate(T, VariablesIn, Res), VariablesOut is ValueSoFar-Res;
    LastOp = '*', evaluate(T, VariablesIn, Res), VariablesOut is ValueSoFar*Res;
    LastOp = '/', evaluate(T, VariablesIn, Res), VariablesOut is ValueSoFar/Res.

evaluate(term(F, mult_op, T), VariablesIn, VariablesOut) :-
    evaluate(F, VariablesIn, R1), evaluate(T, VariablesIn, R2, R1, '*'), VariablesOut is R2.

evaluate(term(F, mult_op, T), VariablesIn, VariablesOut, ValueSoFar, LastOp) :-
    LastOp = '+', evaluate(F, VariablesIn, R1), NewValuesSoFar is ValueSoFar + R1, evaluate(T, VariablesIn, R2, NewValuesSoFar, '*'), VariablesOut is R2;
    LastOp = '-', evaluate(F, VariablesIn, R1), NewValuesSoFar is ValueSoFar - R1, evaluate(T, VariablesIn, R2, NewValuesSoFar, '*'), VariablesOut is R2;
    LastOp = '*', evaluate(F, VariablesIn, R1), NewValuesSoFar is ValueSoFar * R1, evaluate(T, VariablesIn, R2, NewValuesSoFar, '*'), VariablesOut is R2;
    LastOp = '-', evaluate(F, VariablesIn, R1), NewValuesSoFar is ValueSoFar / R1, evaluate(T, VariablesIn, R2, NewValuesSoFar, '*'), VariablesOut is R2.


evaluate(term(F, div_op, T), VariablesIn, VariablesOut) :-
    evaluate(F, VariablesIn, R1), evaluate(T, VariablesIn, R2, R1, '/'), VariablesOut is R2.

evaluate(term(F, div_op, T), VariablesIn, VariablesOut, ValueSoFar, LastOp) :-
    LastOp = '+', evaluate(F, VariablesIn, R1), NewValuesSoFar is ValueSoFar + R1, evaluate(T, VariablesIn, R2, NewValuesSoFar, '/'), VariablesOut is R2;
    LastOp = '-', evaluate(F, VariablesIn, R1), NewValuesSoFar is ValueSoFar - R1, evaluate(T, VariablesIn, R2, NewValuesSoFar, '/'), VariablesOut is R2;
    LastOp = '*', evaluate(F, VariablesIn, R1), NewValuesSoFar is ValueSoFar * R1, evaluate(T, VariablesIn, R2, NewValuesSoFar, '/'), VariablesOut is R2;
    LastOp = '/', evaluate(F, VariablesIn, R1), NewValuesSoFar is ValueSoFar / R1, evaluate(T, VariablesIn, R2, NewValuesSoFar, '/'), VariablesOut is R2.

evaluate(term(F), VariablesIn, VariablesOut) :-
    evaluate(F, VariablesIn, VariablesOut).

evaluate(term(F), VariablesIn, VariablesOut, ValueSoFar, LastOp) :-
    LastOp = '+', evaluate(F, VariablesIn, Res), VariablesOut is ValueSoFar+Res;
    LastOp = '-', evaluate(F, VariablesIn, Res), VariablesOut is ValueSoFar-Res;
    LastOp = '*', evaluate(F, VariablesIn, Res), VariablesOut is ValueSoFar*Res;
    LastOp = '/', evaluate(F, VariablesIn, Res), VariablesOut is ValueSoFar/Res.

evaluate(factor(INT), VariablesIn, VariablesOut) :-
    evaluate(INT, VariablesIn, VariablesOut).

evaluate(factor(IDENT), VariablesIn, VariablesOut) :-
    evaluate(IDENT, VariablesIn, Res), Res is VariablesOut.

evaluate(factor(left_paren, EX, right_paren), VariablesIn, VariablesOut) :-
    evaluate(EX, VariablesIn, VariablesOut).

evaluate(ident(X), VariablesIn, VariablesOut) :-
    my_member(X=VariablesOut, VariablesIn);
    not_member(X=_, VariablesIn), VariablesOut = 0.

evaluate_assign_ident(ident(X), VariablesIn, VariablesOut) :-
VariablesOut = X.

evaluate(ident(X), VariablesIn, VariablesOut) :-
    VariablesOut = X.

evaluate(int(X), VariablesIn, VariablesOut) :-
    VariablesOut = X.

build_equality_structure(Id,Value,Id=Value).

my_member(X, [X|Xs]).
my_member(X, [_Y|Xs]):- member(X, Xs).

not_member(X, []).
not_member(X, [Y|Xs]) :- X \= Y, not_member(X, Xs).

append([],Xs,Xs).
append([X|Xs],Ys,[X|Zs]):- append(Xs,Ys,Zs).

	
