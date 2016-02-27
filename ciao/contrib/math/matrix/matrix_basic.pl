:- module(matrix_basic, _, [assertions, unittestprops, nativeprops]).

:- use_module(library(lists)).
:- use_module(library(math(vector))).

:- doc(module, "This module contain the matrix predicates that
   does not depend of C implemented functions.").

matrix_cols([Row|_X], Cols) :-
	length(Row, Cols).

matrix_rows(X, Rows) :-
	length(X, Rows).

:- test matrix_transpose(X, Y) : (X = [[aaa, bbb, ccc], [ddd, eee, fff]])
	=> (Y == [[aaa, ddd], [bbb, eee], [ccc, fff]]) # "Transpose a non
	  numeric matrix.".

:- test matrix_transpose(X, Y) : (X = [[1.0, 2.0]])
	=> (Y == [[1.0], [2.0]]) # "Transpose a 2x1 matrix.".

:- true pred matrix_transpose(Matrix, Trans) : list(list(term)) * term
	=> list(list(term)) * list(list(term)) # "Unifies @var{Trans} with
   the transposed matrix of @var{Matrix}.".

matrix_transpose([],    []).
matrix_transpose([V|M], T ) :-
	matrix_add_column(V, T1, T),
	matrix_transpose(M, T1).

:- true pred matrix_add_column(Vector, Matrix, Result) : list(term) *
	list(list(term)) * term => list(term) * list(list(term)) *
	list(list(term)) # "Unifies @var{Result} with the matrix obtained
   adding @var{Vector} as the first column of @var{Matrix}.".

matrix_add_column([],     [], []    ).
matrix_add_column([V|Vs], M,  [R|Rs]) :-
	matrix_add_element(M, V, R, Ms),
	matrix_add_column(Vs, Ms, Rs).

matrix_add_element([M|Ms], V, [V|M], Ms).
matrix_add_element([],     V, [V],   []).

matrix_add_column_zero([],     [],         []    ).
matrix_add_column_zero([A|As], [[0|A]|Bs], [0|Cs]) :-
	matrix_add_column_zero(As, Bs, Cs).

:- test matrix_norm(M, N) : (M = []) => (N =:= 0)
# "Matrix 0x0 have norm 0".

:- test matrix_norm(M, N) : (M = [[1, -2.0], [1, 1]]) => (N =:= 3).

matrix_norm(M, N) :-
	matrix_norm_(M, 0, N).
matrix_norm_([],    N,  N).
matrix_norm_([C|M], N0, N) :-
	vector_norm_inf(C, X),
	N1 is X + N0,
	matrix_norm_(M, N1, N).

matrix_substraction([],     Y,      Y     ).
matrix_substraction([X|Xs], [Y|Ys], [Z|Zs]) :-
	vector_substraction(X, Y, Z),
	matrix_substraction(Xs, Ys, Zs).

:- true pred matrix_addition(A, B, C) : list(list(number)) *
	list(list(number)) * term => list(list(number)) *
	list(list(number)) * list(list(number)) # "Unifies @var{Z} with the
   sum of the matrices @var{X} and @var{Y}.".

matrix_addition([],     Y,      Y     ).
matrix_addition([X|Xs], [Y|Ys], [Z|Zs]) :-
	vector_addition(X, Y, Z),
	matrix_addition(Xs, Ys, Zs).

:- test matrix_constant_multiply(X, Y, Z) : (X = [[1.5]], Y = 2) => (Z
	    =:= 3) # "Multiply a 1x1 matrix by a number".

matrix_constant_multiply([],    _, []   ).
matrix_constant_multiply([A|M], K, [B|R]) :-
	vector_constant_multiply(A, K, B),
	matrix_constant_multiply(M, K, R).


:- test matrix_identity(N, I) : (N = 3) => (I == [[1, 0, 0], [0, 1,
		    0], [0, 0, 1]]) # "Matrix identity 3x3".

matrix_identity(0, []        ) :- !.
matrix_identity(N, [[1|I]|Is]) :-
	N > 0,
	N1 is N-1,
	matrix_identity(N1, Is1),
	matrix_add_column_zero(Is1, Is, I).

:- test matrix_multiply(A, B, C) : (
	    A = [[-0.60, -0.64, -0.48],
		[0.80, -0.48, -0.36],
		[0.00, -0.60, 0.80]],
	    B = [[1, 2],
		[0, 3],
		[0, 0]             ]) =>
	near(C, [[-0.6, -3.12], [0.8, 0.16], [0.0, -1.8]], 0.00000001) #
	"Product M3x3 and M1x3".

:- true pred matrix_multiply_transpose(Matrix, Transpose, Result) :
	list(list(term)) * list(list(term)) * term =>
	list(list(term)) * list(list(term)) * list(list(term)) #
"Unifies @var{Result} with the matricial product between the
   matrices @var{Matrix} and the transposed matrix of
   @var{Transpose}.".

matrix_multiply_transpose([],           _,         []              ).
matrix_multiply_transpose([Row|Matrix], Transpose, [Element|Result]) :-
	matrix_vector_multiply(Transpose, Row, Element),
	matrix_multiply_transpose(Matrix, Transpose, Result).

matrix_vector_multiply([],           _,      []              ).
matrix_vector_multiply([Row|Matrix], Vector, [Element|Result]) :-
	vector_multiply(Row, Vector, Element),
	matrix_vector_multiply(Matrix, Vector, Result).

:- true pred matrix_multiply(Matrix1, Matrix2, Result) : list(list(number)) *
	list(list(number)) * term => list(list(number)) *
	list(list(number)) * list(list(number)) # "Unifies @var{Result}
   with the matricial product between the matrices @var{Matrix1} and
   @var{Matrix2}.".

matrix_multiply(A, B, C) :-
	matrix_transpose(B, B2),
	matrix_multiply_transpose(A, B2, C).

:- test matrix_diagonal(M, D) : (M = [[a, 2, 3], [4, 5, 6], [7, 8, 9]])
	=> (D = [a, 5, 9]).

matrix_diagonal([],                    []              ).
matrix_diagonal([[Element|_]|Matrix0], [Element|Vector]) :-
	matrix_add_column(_, Matrix, Matrix0),
	matrix_diagonal(Matrix, Vector).

:- test triangular_product_combination(A, B) :
	(A = [ 3,             2,          1,      5   ]) =>
	(B == [[9, 6, 3, 15], [4, 2, 10], [1, 5], [25]]).

triangular_product_combination([],               []          ).
triangular_product_combination([Element|Vector], [Row|Matrix]) :-
	vector_constant_multiply([Element|Vector], Element, Row),
	triangular_product_combination(Vector, Matrix).

:- test triangular_matrix(A, B) : (A = [[1, 2, 4], [3, 4], [5]]) =>
	(B = [[1, 2, 4], [2, 3, 4], [4, 4, 5]]) # "Simple call".
:- test triangular_matrix(A, B) : (B = [[1, 2, 4], [2, 3, 4], [4, 4, 5]]) =>
	(A = [[1, 2, 4], [3, 4], [5]]) # "Reverse call".
:- test triangular_matrix(_A, B) : (B = [[1, 2, 4], [2, 3, 4], [4, 8, 5]]) +
	fails
# "Reverse call over non simetric matrix".

:- true pred triangular_matrix(A, B) : list(list(number)) *
	list(list(number)) => list(list(number)) * list(list(number)) #
"Converts the triangular matrix @var{A} to the rectangular matrix
   @var{B}.  Note that a triangular matrix is as follow:

   A = [[A11,A12,    ...,A1N],
            [A22,A23,...,A2N],
                         ....
                        [ANN]]

   And the respective rectangular matrix is

   B = [[A11,A12,    ...,A1N],
        [A12,A22,A23,...,A2N],
        [A13,A23,A33,...,A3N],
	                  ...
        [A1N,A2N,A3N,...,ANN]]

".

triangular_matrix([],          []         ).
triangular_matrix([[A|Ar]|As], [[A|Ar]|Bs]) :-
	matrix_add_column(Ar, Bs1, Bs),
	triangular_matrix(As, Bs1).
