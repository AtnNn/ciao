c_type(call).
gen_c_body(N, _Init) :-
	gen_c_compare_eq_1(c_smaller_eq_1, '=<', '2', '1', N).
