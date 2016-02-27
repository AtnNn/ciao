c_type(call).
gen_c_body(N, _Init) :-
	writeln('	A = a,'),
	for(0, N, _I, write_list(['cgc_', N, '( A ), '])),
	write_list_nl(['cgc_', N, '( _X', N, ' ).']),
	write_list_nl(['cgc_', N, '( _X ).']),
	write_list_nl(['cgc_', N, '( b ).']).
