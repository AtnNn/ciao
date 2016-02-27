c_type(call).
gen_c_body(N, _Init) :-
	write_list_nl(['	cf1_', N, '.']),
	N1 is N + 1,
	for(0, N1, _I, write_list_nl(['cf1_', N, ' :- fail.'])),
	write_list_nl(['cf1_', N, '.']).
