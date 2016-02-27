c_type(call).
gen_c_body(N, _Init) :-
	gen_c_compare_1(c_smaller_1, '<', '2', N).
