c_type(call).
gen_c_body(N, _Init) :-
	write('	paca( a'),
	for(0, N, _I, write(', a')),
	writeln(' ).'),
	write_list_nl(['c_put_a_constant_atom_0_', N, '.']),
	write('	paca( b'),
	for(0, N, _I, write(', b')),
	writeln(' ).').
