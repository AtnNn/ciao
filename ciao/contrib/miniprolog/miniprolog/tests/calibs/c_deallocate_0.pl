c_type(cend).

each_gen_c_deallocate_0(N, I) :-
	I1 is I + 1,
	write_list_nl(['cde_', N, '_', I,
		' :- call( cde_', N, '_', I1, ' ).']).

gen_c_body(N, Init) :-
	write_list_nl(['	cde_', N, '_0.']),
	for(0, N, I, each_gen_c_deallocate_0(N, I)),
	write_list_nl(['cde_', N, '_', N, ' :- ', Init, '.']).
