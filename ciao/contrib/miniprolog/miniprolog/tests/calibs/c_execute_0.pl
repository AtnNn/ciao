c_type(call).

each_gen_c_execute(N, I) :-
	I1 is I + 1,
	nl,
	write_list(['cex_', N, '_', I, ' :- cex_', N, '_', I1, '.']).

gen_c_body(N, _Init) :-
	write_list(['	cex_', N, '_0.']),
	nl,
	for(0, N, I, each_gen_c_execute(N, I)),
	nl,
	write_list(['cex_', N, '_', N, '.']),
	nl.
