c_type(call).
gen_c_body(N, _Init) :-
	write_list(['	cgl_', N, '.']),
	nl,
	write_list(['cgl_', N, ' :-']),
	nl,
	writeln('	fail,'),
	for(0, N, _I, writeln('	!,')),
	writeln('	true.'),
	write_list(['cgl_', N, '.']),
	nl.
