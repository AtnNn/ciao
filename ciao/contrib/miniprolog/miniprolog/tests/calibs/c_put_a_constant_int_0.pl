c_type(call).
gen_c_body(N, _Init) :-
	write('	paci( 0'),
	for(0, N, I, write_list([', ', I])),
	writeln(' ).'),
	write_list_nl(['c_put_a_constant_int_0_', N, '.']),
	write('	paci( 1'),
	for(0, N, I, write_list([', ', I])),
	writeln(' ).').
