c_type(cend).
gen_c_body(N, Init) :-
	gen_c_arith('mod', N, Init).
