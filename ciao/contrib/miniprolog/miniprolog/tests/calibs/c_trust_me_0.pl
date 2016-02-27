c_type(fail).
gen_c_body(N, _Init) :-
	writeln('	A = 1,'),
	for(0, N, _I, write_list(['cf3_', N, '( A ), '])),
	write_list_nl(['cf3_', N, '( _X', N, ' ).']),
	write_list_nl(['cf3_', N, '( _X ).']),
	write_list_nl(['cf3_', N, '( 2 ).']).
