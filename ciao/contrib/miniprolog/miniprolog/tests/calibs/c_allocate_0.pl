c_type(call).

each_gen_c_allocate_0(N, I) :-
	I1 is I + 1,
	nl,
	write_list(['ca_', N, '_', I, ' :- ca_', N, '_', I1, ', fail.']).

gen_c_body(N, _Init) :-
	write_list(['	ca_', N, '_0.']),
	nl,
	write_list(['c_allocate_0_', N, '.']),
	nl,
	for(0, N, I, each_gen_c_allocate_0(N, I)),
	nl,
	write_list(['ca_', N, '_', N, ' :- ca_end1_', N, ', fail.']),
	nl,
	write_list(['ca_end1_', N, '.']),
	nl.
