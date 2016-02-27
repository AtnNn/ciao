c_type(cend).
gen_c_body(N, Init) :-
	write_list_nl(['	', Init, ',']),
	for(0, N, _I, writeln('	!,')),
	writeln('	true.').
