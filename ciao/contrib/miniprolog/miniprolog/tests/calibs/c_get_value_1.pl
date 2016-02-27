c_type(call).
gen_c_body(N, _Init) :-
	write_list(['	get_value_1_', N, '( 0, 1 ).']),
	nl,
	N1 is N + 1,
	for(0, N1, _I, write_list_nl(['get_value_1_', N, '( A, A ).'])),
	write_list(['get_value_1_', N, '( _A, _B ).']),
	nl.
