c_type(call).
gen_c_body(N, _Init) :-
	write_list(['	ccc_', N, ', ']),
	for(0, N, _I, write_list(['ccc_', N, ', '])),
	write_list(['ccc_', N, '.']),
	nl,
	write_list(['ccc_', N, '.']),
	nl,
	write_list(['ccc_', N, '.']),
	nl.
