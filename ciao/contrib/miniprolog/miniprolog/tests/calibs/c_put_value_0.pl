c_type(call).
gen_c_body(N, _Init) :-
	write('	pv0( 1'),
	N1 is N + 1,
	for(0, N1, _I, write(', _V')),
	writeln(' ).'),
	write_list_nl(['c_put_value_0_', N, '.']),
	write('	pv0( 2'),
	for(0, N1, _I, write(', _V')),
	writeln(' ).').
