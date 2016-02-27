c_type(cend).
gen_c_body(N, Init) :-
	writeln('	A = 1,'),
	write_list(['	_Y = dummy( _X']),
	for(0, N, I, write_list([', _X', I])),
	writeln(' ),'),
	write_list_nl(['	', Init, ',']),
	for(0, N, I, write_list_nl(['	A', ' = _X', I, ','])),
	writeln('	true.').
