write_list([]) :- !.
write_list([E|L]) :-
	write(E),
	write_list(L).

write_list_nl(A) :-
	write_list(A),
	nl.

gen_c_get_constant_1(A, A2, Ex, N) :-
	write_list(['	fgc1_', A, '_', N, '( ', A, Ex, ' ).']),
	nl,
	N1 is N + 1,
	for(0, N1, _I, write_list_nl(['fgc1_', A, '_', N, '( ', A2, Ex, ' ).'])
	),
	write_list(['fgc1_', A, '_', N, '( _A', Ex, ' ).']),
	nl.

% each_gen_c_compare_0( Op, A, B ) :-
% 	write_list( [ '	', A, ' ', Op, ' ', B, ',' ] ),
% 	nl.

gen_c_compare_0(Op, A, B, N) :-
	write_list(['	A = ', A, ', B = ', B, ',']),
	nl,
	for(0, N, _I, write_list_nl(['	A ', Op, ' B,'])),
	writeln('	true.').

% gen_c_compare_1( CalibName, Op, A, N ) :-
% 	write_list_nl( [ '	com_', CalibName, '_', N, '.' ] ),
% 	N1 is N + 1,
% 	for( 0, N1, _I, write_list_nl( [ 'com_', CalibName, '_', N,
% 		    ' :- ', A, ' ', Op, ' ', A, '.' ] ) ),
% 	write_list_nl( [ 'com_', CalibName, '_', N, '.' ] ).

% gen_c_compare_eq_1( CalibName, Op, A, B, N ) :-
% 	write_list_nl( [ '	compare_', CalibName,
% 		'_', N, '( ', A, ', ', B, ' ).' ] ),
% 	N1 is N + 1,
% 	for( 0, N1, _I, write_list_nl( [ 'compare_', CalibName, '_', N,
% 		    '( A, B ) :- A ', Op, ' B.' ] ) ),
% 	write_list_nl( [ 'compare_', CalibName, '_', N, '( A, B ).' ] ).

gen_c_compare_1(CalibName, Op, A, N) :-
	write_list_nl(['	com_', CalibName, '_', N, '( ', A, ' ).']),
	N1 is N + 1,
	for(0, N1, _I, write_list_nl(['com_', CalibName, '_', N,
		    '( A ) :- A ', Op, ' A.'])),
	write_list_nl(['com_', CalibName, '_', N, '( A ).']).

gen_c_compare_eq_1(CalibName, Op, A, B, N) :-
	write_list_nl(['	ceq_', CalibName, '_', N, '.']),
	N1 is N + 1,
	for(0, N1, _I, write_list_nl(['ceq_', CalibName, '_', N,
		    ' :- ', A, ' ', Op, ' ', B, '.'])),
	write_list_nl(['ceq_', CalibName, '_', N, '.']).

gen_c_set_constant_0(A, N) :-
	write_list(['	_X = func( ', A]),
	for(0, N, _I, write_list([', ', A])),
	writeln(' ).').

gen_c_put_constant_0(V, N) :-
	for(0, N, I, write_list_nl(['	_A', I, ' = ', V, ','])),
	writeln('	true.').

gen_c_unify_variable_gnd_gnd_0(N, A, B) :-
	write_list_nl(['	uv_', N, '( ', A, ', ', B, ' ).']),
	nl,
	write_list_nl(['uv_', N, '( X, Y ) :-']),
	nl,
	for(0, N, _I, writeln('	X = Y,')),
	writeln('	true.').

gen_c_unify_variable_gnd_gnd_1(N, A, B) :-
	write_list_nl(['	uv1_', N, '.']),
	N1 is N + 1,
	for(0, N1, _I,
	    write_list_nl(['uv1_', N, ' :- ', A, ' = ', B, '.'])),
	write_list_nl(['uv1_', N, '.']).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Calibrators that are disabled now:

% gen_c_body( c_fail_1, N ) :-
% 	for( 0, N, _I, write_list( [ 'cf2_', N, ', ' ] ) ),
% 	write_list_nl( [ 'cf2_', N, '.'         ] ),
% 	write_list_nl( [ 'cf2_', N, '.'         ] ),
% 	write_list_nl( [ 'cf2_', N, ' :- fail.' ] ).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

gen_c_arith(Op, N, Init) :-
	writeln('	A = 1,'),
	write_list(['	_Y = dummy( _X']),
	for(0, N, I, write_list([', _X', I])),
	writeln(' ),'),
	write_list_nl(['	', Init, ',']),
	for(0, N, I,
	    write_list_nl(['	_X', I, ' is A ', Op, ' A,'])),
	writeln('	true.').

gen_c_get_constant_0(A, N, Init) :-
	write_list(['	fgc_', A, '_', N, '( ', A]),
	for(0, N, _I, write_list([', ', A])),
	writeln(' ).'),
	write_list(['fgc_', A, '_', N, '( ', A]),
	for(0, N, I, write_list([', _A', I])),
	write_list_nl([' ) :- ', Init, ', fail.']),
	write_list(['fgc_', A, '_', N, '( ', A]),
	for(0, N, _I, write_list([', ', A])),
	writeln(' ).').

% gen_c_body_type( c_unify_variable_lst_lst_0, N, Init ) :-
% 	write_list_nl( [ '	numlist( ', N, ', L ),' ] ),
% 	write_list_nl( [ '	', Init, ',' ] ),
% 	writeln( '	list( L ).' ).
