c_type(call).
gen_c_body(N, _Init) :-
	write_list(['	ccp_', N, ', ']),
	for(0, N, _I, write_list(['ccp_', N, ', '])),
	write_list(['ccp_', N, '.']),
	nl,
	write_list(['ccp_', N, '.']),
	nl.
