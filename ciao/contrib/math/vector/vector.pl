:- module(_,
	    [
		vector_norm_inf/2,
		vector_multiply/3,
		vector_constant_multiply/3,
		vector_constant_division/3,
		vector_sum/2,
		vector_square_sum/2,
		vector_addition/3,
		vector_addition_list/3,
		vector_substraction/3,
		vector_multiply_components/3,
		vector_divide_components/3,
		structure_addition/3,
		structure_addition_list/3,
		structure_substraction/3,
		structure_substraction_list/3,
		structure_max_abs_number/2,
		vector_is_zero/1,
		vector_sqrt/2,
		vector_project/4,
		vector_project_list/4,
		vector_division/3,
		vector_constant_multiply_addition/4
	    ],
	    [assertions, nativeprops, unittestprops]).

:- test vector_norm_inf(L, S) : (L = [1, -2.0, 1.5]) => near(S, 2, 0) #
	"Norm of a 3 component vector".

:- test vector_norm_inf(L, S) : (L = []) => near(S, 0, 0) #
	"Empty list have norm zero".

:- pred vector_norm_inf(List, AbsMax) : list(number) * term =>
	list(number) * number # "Unifies @var{AbsMax} with the
	infinite norm, which is the maximum of the absolute value of
	the numbers in the list @var{List}.".

vector_norm_inf(X, A) :-
	vector_norm_inf_(X, 0, A).

vector_norm_inf_([],     A,  A).
vector_norm_inf_([X|Xs], A0, A) :-
	max_abs_number(X, A0, A1),
	vector_norm_inf_(Xs, A1, A).

max_abs_number(X, A0, A1) :-
	(
	    X < 0 ->
	    X1 is -X
	;
	    X1 = X
	),
	(
	    A0 > X1 ->
	    A1 = A0
	;
	    A1 = X1
	).

:- test vector_addition(A, B, C) : (A = [1, 2, 3], B = [3, 2, 2]) => ( C =
	    [4, 4, 5] ) # "Test addition of vectors".

:- true pred vector_addition(X, Y, Z) : list(number) * list(number) *
	term => list(number) * list(number) * list(number) # "Unifies
   @var{Z} with the sum of the vectors @var{X} and @var{Y}.".

vector_addition([],     Y,      Y).
vector_addition([X|Xs], [Y|Ys], [Z|Zs]) :-
	Z is X + Y,
	vector_addition(Xs, Ys, Zs).

vector_addition_list([],           Vector,  Vector).
vector_addition_list([Data|Datas], Vector0, Vector) :-
	vector_addition(Vector0, Data, Vector1),
	vector_addition_list(Datas, Vector1, Vector).

vector_multiply_components([],     Y,      Y).
vector_multiply_components([X|Xs], [Y|Ys], [Z|Zs]) :-
	Z is X*Y,
	vector_multiply_components(Xs, Ys, Zs).

vector_divide_components([],     Y,      Y).
vector_divide_components([X|Xs], [Y|Ys], [Z|Zs]) :-
	Z is X/Y,
	vector_divide_components(Xs, Ys, Zs).

:- test structure_addition_list(A, B, Y) : (A = [[1, [2, 3]]], B = [])
	=> (Y == [1, [2, 3]]) # "Adding two structures".

:- test structure_addition_list(A, B, _Y) : (A = [[1, [2, 3]]], B = [a]
	) + fails # "Adding something that is not a structure".

:- test structure_addition_list(A, B, Y) : ( A = [[5, [2, 3]],
		[1, [3, 1]]], B = [] ) => (Y == [6, [5, 4]]) # "Adding null to
	a structure".

structure_addition_list([],               Addition,  Addition).
structure_addition_list([Element|Vector], Addition0, Addition) :-
	structure_addition(Element, Addition0, Addition1),
	structure_addition_list(Vector, Addition1, Addition).

structure_addition([],     A,      A) :- !.
structure_addition(A,      [],     A) :- !.
structure_addition([A|As], [B|Bs], [C|Cs]) :- !,
	structure_addition(A,  B,  C),
	structure_addition(As, Bs, Cs).
structure_addition(A, B, C) :-
	num(A),
	num(B),
	!,
	C is A + B.
structure_addition(A, B, C) :- !,
	A =.. [F|As],
	B =.. [F|Bs],
	structure_addition(As, Bs, Cs),
	C =.. [F|Cs].

structure_substraction_list([],               Addition,  Addition).
structure_substraction_list([Element|Vector], Addition0, Addition) :-
	structure_substraction(Element, Addition0, Addition1),
	structure_substraction_list(Vector, Addition1, Addition).

structure_substraction([],     A,      A) :- !.
structure_substraction(A,      [],     A) :- !.
structure_substraction([A|As], [B|Bs], [C|Cs]) :- !,
	structure_substraction(A,  B,  C),
	structure_substraction(As, Bs, Cs).
structure_substraction(A, B, C) :-
	num(A),
	num(B),
	!,
	C is A - B.
structure_substraction(A, B, C) :- !,
	A =.. [F|As],
	B =.. [F|Bs],
	structure_substraction(As, Bs, Cs),
	C =.. [F|Cs].

structure_max_abs_number(L, A) :-
	structure_max_abs_number_(L, 0, A).

structure_max_abs_number_([],     A,  A) :- !.
structure_max_abs_number_([X|Xs], A0, A) :- !,
	structure_max_abs_number_(X,  A0, A1),
	structure_max_abs_number_(Xs, A1, A).
structure_max_abs_number_(X, A0, A) :-
	num(X),
	!,
	max_abs_number(X, A0, A).
structure_max_abs_number_(X, A0, A) :-
	X =.. [_F|Xs],
	structure_max_abs_number_(Xs, A0, A).

vector_is_zero([]).
vector_is_zero([Element|Vector]) :-
	Element =:= 0,
	vector_is_zero(Vector).

:- test vector_sum(L, S) : (L = [1, 2, 3.0, 7]) => (S = 13.0) # "Sum
   of 5 elements".

:- test vector_sum(L, S) : (L = []) => (S = 0) # "Empty list
   sums zero.".

:- true pred vector_sum(List, Sum) : list(number) * term => list(number) *
	number # "Unifies @var{Sum} with the total sum of the numbers in
   the list @var{List}.".

vector_sum(X, S) :-
	vector_sum_(X, 0, S).

vector_sum_([],     S,  S).
vector_sum_([X|Xs], S0, S) :-
	S1 is S0 + X,
	vector_sum_(Xs, S1, S).

:- test vector_square_sum(L, S) : (L = [1, 2, 3.0, 7]) => (S = 63.0)
# "Square sum of 5 elements".

:- test vector_square_sum(L, S) : (L = []) => (S = 0) # "Empty
   list sums zero".

:- true pred vector_square_sum(List, Sum2) : list(number) * term =>
	list(number) * number # "Unifies @var{Sum2} with the total sum of
   the square of the numbers in the list @var{List}.".

vector_square_sum(Vector, SquareSum) :-
	vector_multiply(Vector, Vector, SquareSum).

:- true pred vector_multiply(Vector1, Vector2, Result) : list(number) *
	list(number) * term => list(number) * list(number) * number #
"Unifies @var{Result} with the scalar product between the vectors
   @var{Vector1} and @var{Vector2}.".

vector_multiply(Vector1, Vector2, S) :-
	vector_multiply_(Vector1, Vector2, 0, S).

vector_multiply_([],     [],     S,  S).
vector_multiply_([A|As], [B|Bs], S0, S) :-
	S1 is S0 + A * B,
	vector_multiply_(As, Bs, S1, S).

:- true pred vector_constant_multiply(Vector, Scalar, Result) : number *
	list(number) * term => number * list(number) * list(number) #
"Unifies @var{Result} with the scalar product between @var{Scalar}
   and @var{Vector}.".

vector_constant_multiply([],     _, []).
vector_constant_multiply([X|Xs], R, [Y|Ys]) :-
	Y is R * X,
	vector_constant_multiply(Xs, R, Ys).

:- true pred vector_constant_division(Scalar, Vector, Result) : number *
	list(number) * term => number * list(number) * list(number) #
"Unifies @var{Result} with the scalar product between
   1.0/@var{Scalar} and @var{Vector}.".

vector_constant_division(_, [],     []).
vector_constant_division(R, [X|Xs], [Y|Ys]) :-
	Y is X/R,
	vector_constant_division(R, Xs, Ys).

:- true pred vector_substraction(X, Y, Z) : list(number) * list(number) *
	term => list(number) * list(number) * list(number) # "Unifies
   @var{Z} with the rest of the vectors @var{X} and @var{Y}.".

vector_substraction([],     Y,      Y).
vector_substraction([X|Xs], [Y|Ys], [Z|Zs]) :-
	Z is X-Y,
	vector_substraction(Xs, Ys, Zs).


vector_division([],     [],     []).
vector_division([A|As], [B|Bs], [R|Rs]) :-
	R is A / B,
	vector_division(As, Bs, Rs).

vector_sqrt([],         []).
vector_sqrt([C|Vector], [E|Sqrt]) :-
	E is sqrt(C),
	vector_sqrt(Vector, Sqrt).

:- true pred vector_constant_multiply_addition(Vector, Scalar, Add,
	    Result) : list(number) * number * list(number) * term => number *
	list(number) * list(number) * list(number) # "Unifies @var{Result}
   with the product between @var{Scalar} and @var{Vector}, plus
   @var{Add}.  In other words, @var{Result} = @var{Scalar} *
   @var{Vector} + @var{Add}.".

vector_constant_multiply_addition([],     _,      [],     []).
vector_constant_multiply_addition([V|Vs], Scalar, [A|As], [R|Rs]) :-
	R is Scalar * V + A,
	vector_constant_multiply_addition(Vs, Scalar, As, Rs).

:- test vector_project(A, B, C, D) :
	(
	    A = [a, b, c, d, e],
	    B = [b, d, e],
	    C = [aaa, bbb, ccc, ddd, eee]
	) =>
	(D == [bbb, ddd, eee]).

vector_project([], _, _, []).
vector_project([Element|Base], [Element|Projection], [Data|Datas],
	    [Data|Outputs]) :-
	!,
	vector_project(Base, Projection, Datas, Outputs).
vector_project([_Element|Base], Projection, [_Data|Datas], Outputs) :-
	vector_project(Base, Projection, Datas, Outputs).

:- test vector_project_list(A, B, C, D) : (
	    (B, C, A) = ( [a, b, c, d, e], [b, d, e], [[aa, bb, cc, dd, ee],
		    [aaa, bbb, ccc, ddd, eee]] )) =>
	(
	    D == [[bb, dd, ee], [bbb, ddd, eee]]).

vector_project_list([],           _Base, _Projection, []).
vector_project_list([Data|Datas], Base,  Projection,  [Output|Outputs]) :-
	vector_project(Base, Projection, Data, Output),
	vector_project_list(Datas, Base, Projection, Outputs).
