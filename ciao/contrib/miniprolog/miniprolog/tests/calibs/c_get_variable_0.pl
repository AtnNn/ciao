c_type(cend).
gen_c_body(N, Init) :-
	writeln('	A = 0,'),
	writeln('	gvr( A'),
	for(0, N, _I, write_list([', A'])),
	writeln(' ).'),
	write_list(['gvr( _A']),
	for(0, N, I, write_list([', _A', I])),
	write_list_nl([' ) :- ', Init, ', fail.']),
	write_list(['gvr( _A']),
	for(0, N, I, write_list([', _A', I])),
	writeln(' ).').
