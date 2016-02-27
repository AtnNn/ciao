:- module(_, _, [assertions, unittestprops]).

:- use_module(library(math(matrix))).
:- use_module(library(math(vector))).

:- doc(module, "Statistics functions implemented using matrixes.").

:- test covariance(R, Cols, MRSS, CoVariance) : (
	    R = [[2.82842712474619, 237.58787847868, 415.7787873376899],
		[0.0, 37.06750598570128, 45.02595887200543],
		[0.0, 0.0,               43.11221436735159]],
	    Cols = 3,
	    MRSS = 35.2690958543004) =>
	near(CoVariance,
	    [[223.8943237364822, -1.119762785525169, -0.8532354610243564],
		[-1.119762785525169, 0.05366729794342455,
		    -0.0230495934811054],
		[-0.8532354610243564, -0.0230495934811054,
		    0.01897551913902865]], 0.00000001)
# "Calculating known covariance matrix of 3x3.".

:- doc(covariance/3, "The covariance matrix").

covariance(Upper, Cols, MRSS, Covariance) :-
	matrix_identity(Cols, I),
	matrix_R_solve_list(I, Upper, A),
	matrix_transpose(A, AT),
	matrix_R_solve_list(AT, Upper, B),
	matrix_constant_multiply(B, MRSS, Covariance).

:- test confidence_factor(Upper, Value, CF) :
	(
	    Upper = [[1, 2, 3], [1, 1, 2], [3, 1, 4], [2, 1, 1]],
	    Value = [2, 5, 7]
	) =>
	near(CF, 44.375, 0.000000001)
# "Test of confidence factor with a 3x4 QR matrix.".

confidence_factor(Upper, Value, ConfidenceFactor) :-
	matrix_QR_Rsolve(Upper, Value, A),
	vector_multiply(A, A, ConfidenceFactor).

:- doc(variance/2, "The variance which is the diagonal of the
   covariance.").

variance(Covariance, Variance) :-
	matrix_diagonal(Covariance, Variance).
