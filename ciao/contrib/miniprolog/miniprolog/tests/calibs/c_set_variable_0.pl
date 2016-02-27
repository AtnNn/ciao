c_type(call).
gen_c_body(N, _Init) :-
	write('	_X = f( _V'),
	for(0, N, I, write_list([', _V', I])),
	writeln(' ).').
