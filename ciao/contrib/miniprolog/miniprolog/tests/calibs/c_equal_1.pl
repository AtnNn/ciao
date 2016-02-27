c_type(call).
gen_c_body(N, _Init) :-
	gen_c_compare_eq_1(c_equal_1, '==', '1', '2', N).
