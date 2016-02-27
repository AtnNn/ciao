c_type(cend).
gen_c_body(N, Init) :-
	writeln('	A = 0,'),
	write_list(['	get_value_0( A']),
	for(0, N, _I, write_list([', A'])),
	writeln(' ).'),
	write_list(['get_value_0( _A']),
	for(0, N, I, write_list([', _A', I])),
	write_list_nl([' ) :- ', Init, ', fail.']),
	write('get_value_0( _A'),
	for(0, N, _I, write(', _A')),
	writeln(' ).').
