:- module(arith, [(is)/2, (=:=)/2, (=\=)/2, (<)/2, (=<)/2, (>)/2, (>=)/2]).

:- use_module(lists, [member/2, length/2]).

N is Expr :-
	'$expr'(Expr, N1), !,
	N1 = N.
N is Expr :-
	arith_error(N is Expr, [Expr]).

% ----------------
A =:= B :-
	'$expr'(A, AN),
	'$expr'(B, BN), !,
	'$eq'(AN,BN).
A =:= B :-
	arith_error((A =:= B), [A,B]).

A =\= B :-
	'$expr'(A, AN),
	'$expr'(B, BN), !,
	'$neq'(AN, BN).
A =\= B :-
	arith_error((A =\= B), [A,B]).

A < B :-
	'$expr'(A, AN),
	'$expr'(B, BN), !,
	'$less'(AN, BN).
A < B :-
	arith_error((A < B), [A,B]).

A =< B :-
	'$expr'(A, AN),
	'$expr'(B, BN), !,
	'$leq'(AN, BN).
A =< B :-
	arith_error((A =< B), [A,B]).

A > B :-
	'$expr'(A, AN),
	'$expr'(B, BN), !,
	'$less'(BN, AN).
A > B :-
	arith_error((A > B), [A,B]).

A >= B :-
	'$expr'(A, AN),
	'$expr'(B, BN), !,
	'$leq'(BN, AN).
A >= B :-
	arith_error((A >= B), [A,B]).

% ----------------
arith_error(Call, L) :-
	'$get_bb'('$error', Error),
	find_error(Error, L, Call).

find_error(zero_divisor, _, Call) :-
	throw(error(evaluation_error(zero_divisor), Call)).
find_error(float_overflow, _, Call) :-
	throw(error(evaluation_error(float_overflow), Call)).
find_error(undefined, _, Call) :-
	throw(error(evaluation_error(undefined), Call)).
find_error(int_overflow, _, Call) :-
	throw(error(evaluation_error(int_overflow), Call)).
find_error(type_error, L, Call) :-
	check_list(L, Call).
find_error(_, _, Call) :-
	throw(error(unknown_error, Call)).

check_list([X|L], Call) :-
	check_expr(X, Call),
	check_list(L, Call).

check_expr(X, Call) :-
	var(X), !,
	throw(error(instantiation_error, Call)).
check_expr(X, _) :-
	number(X), !.
check_expr(X, Call) :-
	X =.. [Name|Args],
	(   Args = []      -> eval_error(Name/0, Call)
        ;   Args = [A1]    -> check_1(Name, A1, Call)
        ;   Args = [A1,A2] -> check_2(Name, A1, A2, Call)
        ;   length(Args, Arity), eval_error(Name/Arity, Call)
        ).

check_1(Name, Arg, Call) :-
	check_name_1(Name, Type, Call),
	check_expr(Arg, Call),
	Val is Arg,
	check_type(Type, Val, Call).

check_2(Name, Arg1, Arg2, Call) :-
	check_name_2(Name, Type, Call),
	check_expr(Arg1, Call),
	check_expr(Arg2, Call),
	Val1 is Arg1,
	Val2 is Arg2,
	check_type(Type, Val1, Call),
	check_type(Type, Val2, Call).

check_type(if, _, _).
check_type(i, Val, _) :-
	integer(Val), !.
check_type(i, Val, Call) :-
	throw(error(type_error(integer, Val), Call)).
check_type(f, Val, _) :-
	float(Val), !.
check_type(f, Val, Call) :-
	throw(error(type_error(float, Val), Call)).

check_name_1(Name, Type, _) :-
	member(t(Name, Type),
	      [t(\,i),
	       t(float_integer_part,f),
	       t(float_fractional_part,f),
	       t(round,f),
	       t(truncate,f),
	       t(floor,f),
	       t(ceiling,f),
	       t(-,if),
	       t(abs,if),
	       t(sign,if),
	       t(sin,if),
	       t(cos,if),
	       t(atan,if),
	       t(sqrt,if),
	       t(log,if),
	       t(exp,if),
	       t(float,if)
	      ]), !.
check_name_1(Name, _, Call) :-
	eval_error(Name/1, Call).
	       
check_name_2(Name, Type, _) :-
	member(t(Name, Type),
	      [t(//,i),
	       t(rem,i),
	       t(mod,i),
	       t(/\,i),
	       t(\/,i),
	       t(<<,i),
	       t(>>,i),
	       t(+,if),
	       t(-,if),
	       t(*,if),
	       t(/,if),
	       t(**,if)
              ]), !.
check_name_2(Name, _, Call) :-
	eval_error(Name/2, Call).
	       
eval_error(PI, Call) :-
	throw(error(type_error(evaluable, PI), Call)).
