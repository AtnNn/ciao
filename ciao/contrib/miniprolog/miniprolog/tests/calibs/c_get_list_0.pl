c_type(call).
gen_c_body(N, _Init) :-
	write('	_X = [ _V'),
	for(0, N, _I, write(', _V')),
	writeln(' ].').
