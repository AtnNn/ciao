c_type(call).
gen_c_body(N, _Init) :-
	write('	_X = f( '),
	for(0, N, _I, write('f( ')),
	write(1),
	for(0, N, _I, write(' )')),
	writeln(' ).').
