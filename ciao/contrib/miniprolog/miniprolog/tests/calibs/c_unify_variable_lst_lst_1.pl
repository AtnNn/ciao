c_type(call).
gen_c_body(N, _Init) :-
	gen_c_unify_variable_gnd_gnd_1(N, '[a]', '[b]').
