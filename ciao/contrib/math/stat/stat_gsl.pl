:- module(stat_gsl, [general_regression/6], [assertions, unittestprops]).

:- use_module(library(math(matrix)), [matrix_lssolve/7]).


:- true pred general_regression(Data, Results, Pattern, Variables,
	    Coefficients, Residuals) : list * list * list * list * term * term
	=>
	list * list * list * list * list * list # "Generic linear regression.
@var{Data} contains the data, @var{Results} contains the observed
results, @var{Pattern} contains a list of arithmetic expressions that
represent the components of the linear model. @var{Variables} contains
the list of variables used in the expression Pattern.".

:- test general_regression(Data, Result, Pattern, Variables, Coefficients,
	    Residuals) :
	(
	    Data = [[2, 2], [1, 2], [1, 1], [2, 1]],
	    Result = [3, 5, 7, 9],
	    Pattern = [1, X1, X2, X1*X2],
	    Variables = [X1, X2]
	) =>
	(
	    near(Coefficients, [3,   6,   2,   -4.0], 0.00000001),
	    near(Residuals,    [0.0, 0.0, 0.0, 0.0 ], 0.00000001)
	).

general_regression(Datas, Results, Pattern, Variables, Coefficients,
	    Residuals) :-
	apply_data_variables(Datas, Pattern, Variables, AppliedDatas),
	matrix_lssolve(AppliedDatas, Results, _Rows, _Cols, _Upper,
	    Coefficients, Residuals).

:- data general_regression_elem/1.

apply_data_variables([],     _,       _,         []    ).
apply_data_variables([D|Ds], Pattern, Variables, [L|Ls]) :-
	(Variables = D,
	    evaluate_patterns(Pattern, L),
	    assertz_fact(general_regression_elem(L)),
	    fail
	)
    ;
	current_fact(general_regression_elem(L)),
	retract_fact(general_regression_elem(L)),
	apply_data_variables(Ds, Pattern, Variables, Ls).

evaluate_patterns([],     []    ).
evaluate_patterns([P|Ps], [L|Ls]) :-
	L is P,
	evaluate_patterns(Ps, Ls).
