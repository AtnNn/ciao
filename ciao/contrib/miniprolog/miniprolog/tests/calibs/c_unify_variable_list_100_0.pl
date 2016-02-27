c_type(call).
gen_c_body(N, _Init) :-
	writeln('	numlist( 100, A ),'),
	writeln('	numlist( 100, B ),'),
	gen_c_unify_variable_gnd_gnd_0(N, 'A', 'B').
