not(G) :-
	call(G),
	!,
	fail.
not(_).

measure(Goal, Time) :-
	not(not(measure_call(Goal))),
	time_interval(_TU, Time).

measure_call(Goal) :-
	time_init,
	call(Goal),
	time_end.

best_measure(N, FileName, Goal) :-
	best_measure(N, Goal, _Time0, Time),
	termtofile(Time, FileName).

best_measure(0, _Goal, Time,  Time) :- !.
best_measure(N, Goal,  Time0, Time) :-
	N > 0,
	measure(Goal, Time1),
	min_time(Time0, Time1, Time2),
	N1 is N - 1,
	best_measure(N1, Goal, Time2, Time).

min_time(T0,  T1, T1) :- var(T0), !.
min_time(T0,  T1, T0) :- T0 < T1, !.
min_time(_T0, T1, T1).

del_file_nofail(F) :-
	delete_file(F),
	!.
del_file_nofail(_F).

spec_call(L, I, G) :-
	I = L,
	call(G).

for(N, N, _I, _G) :- !.
for(L, N, I,  G) :-
	L < N,
	not(not(spec_call(L, I, G))),
	L1 is L + 1,
	for(L1, N, I, G).

repeat(0, _I, _G) :- !.
repeat(N, I,  G) :-
	N > 0,
	not(not(spec_call(N, I, G))),
	N1 is N - 1,
	repeat(N1, I, G).

length(L, N) :- var(N), !, llength(L, 0, N).
length(L, N) :- dlength(L, 0, N).

llength([],    I,  I).
llength([_|L], I0, I) :- I1 is I0+1, llength(L, I1, I).

dlength([],    I,  I) :- !.
dlength([_|L], I0, I) :- I0 < I, I1 is I0+1, dlength(L, I1, I).

numlist(N, L) :-
	numlist(N, [], L).

numlist(0, L,  L) :- !.
numlist(N, L0, L) :-
	N1 is N - 1,
	numlist(N1, [N|L0], L).

list([_X|L]) :-
	list(L).
list([]).

type(prof).
type(time).
type(true).

% We have less equations than bytecodes, that is because some
% bytecodes have linear dependencies between them.  In this wam the
% kernel of the linear system is: trust_me+retry_me_else = "sum of all
% bytecodes that fails".  Which we can proof that is always true
% (trivial).

% calibrator( CalibName, ProfileMethod ).

%1:
calibrator(c_arith_add_0,           cend).
calibrator(c_arith_div_0,           cend).
calibrator(c_arith_mod_0,           cend).
calibrator(c_arith_mul_0,           cend).
calibrator(c_arith_sub_0,           cend).
calibrator(c_cut_0,                 cend). % Not Fail
calibrator(c_equal_0,               call).
calibrator(c_execute_0,             call).
calibrator(c_get_constant_atom_0,   cend).
calibrator(c_get_constant_int_0,    cend).
calibrator(c_get_level_0,           call). % Not Fail
calibrator(c_put_a_constant_atom_0, call). % Not Fail
calibrator(c_put_a_constant_int_0,  call). % Not Fail
calibrator(c_put_constant_atom_0,   call). % Not Fail
calibrator(c_put_constant_int_0,    call). % Not Fail
calibrator(c_put_value_0,           call). % Not Fail
calibrator(c_set_constant_atom_0,   call). % Not Fail
calibrator(c_set_constant_int_0,    call). % Not Fail
calibrator(c_set_variable_0,        call). % Not Fail
calibrator(c_smaller_0,             call).
calibrator(c_smaller_eq_0,          call).
calibrator(c_unequal_0,             call).
%unify:
calibrator(c_unify_variable_atm_atm_0,  call).
calibrator(c_unify_variable_atm_atm_1,  call).
calibrator(c_unify_variable_cn1_cn2_1,  call).
calibrator(c_unify_variable_int_int_0,  call).
calibrator(c_unify_variable_int_int_1,  call).
calibrator(c_unify_variable_var_gnd_0,  cend).
calibrator(c_unify_variable_gnd_var_0,  cend).
calibrator(c_unify_variable_lst_lst_0,  call).
calibrator(c_unify_variable_str_str_0,  call).
calibrator(c_unify_variable_str_str_1,  call).
calibrator(c_unify_variable_lst_lst_1,  call).
calibrator(c_unify_variable_list_100_0, call).

%2:
calibrator(c_get_list_0,     call). % Not Fail
calibrator(c_get_variable_0, cend). % Not Fail
calibrator(c_get_value_0,    cend).
%3:
calibrator(c_equal_1,             call).
calibrator(c_get_constant_atom_1, fail).
calibrator(c_get_constant_int_1,  call).
calibrator(c_get_value_1,         call).
calibrator(c_smaller_1,           call).
calibrator(c_smaller_eq_1,        call).
calibrator(c_unequal_1,           call).
calibrator(c_call_0,              call).
calibrator(c_try_me_else_0,       call). % Not Fail
calibrator(c_allocate_0,          call).
calibrator(c_deallocate_0,        cend).
calibrator(c_retry_me_else_0,     call).
calibrator(c_trust_me_0,          fail).
calibrator(c_fail_1,              call).
calibrator(c_unify_variable_1,    call).
calibrator(c_get_struct_0,        call).

repnum(Type, N, AN) :-
	repetitions(Type, N, _M),
	atom_number(AN, N).
repnum(_Type, 0, '0').

get_calibname_n_type(CalibName_N_Type, CalibName, N, Type) :-
	get_calibname_n_type(CalibName_N_Type, _CalibName_N,
	    CalibName, N, Type).

get_calibname_n_type(CalibName_N_Type, CalibName_N, CalibName, N, Type) :-
	type(Type),
	atom_concat(CalibName_N_, Type, CalibName_N_Type),
	atom_concat(CalibName_N,  '_',  CalibName_N_),
	repnum(Type, N, AN),
	atom_concat(CalibName_, AN,  CalibName_N),
	atom_concat(CalibName,  '_', CalibName_),
	!.
