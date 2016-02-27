gen_measure(CalibName_N_Type, FileName) :-
	get_calibname_n_type(CalibName_N_Type, CalibName_N,
	    CalibName, _AN, Type),
	calibrator(CalibName, M),
	repetitions(Type, _N, R),
	measures(R, Type, M, CalibName_N, FileName),
	!.

measures(R, Type, M, C, F) :-
	measures_(R, _T, Type, M, C, F),
	measures_done(Type, F),
	!.

measures_done(prof, F) :-
	!,
	atom_concat(F, '.min', Source),
	atom_concat(F, '.prf', Target),
	del_file_nofail(Target),
	rename_file(Source, Target).
measures_done(_Type, _F).

measures_(0, _T, _Type, _M, _C, _F) :- !.
measures_(N, T0, Type,  M,  C,  F) :-
	N > 0,
	measure(Type, M, C, F),
	time_interval(_TU, T1),
	save_lower(T0, T1, Type, F, T2),
	N1 is N - 1,
	!,
	measures_(N1, T2, Type, M, C, F).

save_lower(T0, T1, _Type, _F, T0) :-
	integer(T0),
	integer(T1),
	T0 =< T1,
	!.
save_lower(_T0, T1, Type, F, T1) :-
	save_measures(Type, F).

save_measures(prof, F) :-
	atom_concat(F, '.prf', Source),
	atom_concat(F, '.min', Target),
	del_file_nofail(Target),
	rename_file(Source, Target).
save_measures(time, F) :-
	save_time(F).
save_measures(true, _F).

measure(prof, M, C, F) :-
	profile_method(M, C, F).
measure(time, M, C, _F) :-
	time_method(M, C).
measure(true, M, C, _F) :-
	true_method(M, C).

save_time(F0) :-
	atom_concat(F0, '.oti', F),
	open_output(F, S),
	time_interval(_TU, TL),
	write(TL),
	close_output(S).

profile_method(call, C, F) :-
	profile(F, C),
	!.
profile_method(fail, C, F) :-
	profile_fail(C, F),
	!.
profile_method(cini, C, F) :-
	profile_cini(C, F),
	!.
profile_method(cend, C, F) :-
	profile_cend(C, F),
	!.

time_method(call, C) :-
	time(C),
	!.
time_method(fail, C) :-
	time_fail(C),
	!.
time_method(cini, C) :-
	time_cini(C),
	!.
time_method(cend, C) :-
	time_cend(C),
	!.

true_method(call, C) :-
	call(C),
	!.
true_method(fail, C) :-
	true_fail(C),
	!.
true_method(cini, C) :-
	true_cini(C),
	!.
true_method(cend, C) :-
	true_cend(C),
	!.


profile_fail(C, F) :-
	call(C),
	profile_init(F),
	fail.
profile_fail(_C, _F) :-
	profile_end,
	!.

profile_cini(C, F) :-
	profile_init(F),
	call(C),
	!.

profile_cend(C, F) :-
	call(C),
	profile_end,
	!.

time_fail(C) :-
	call(C),
	time_init,
	fail.
time_fail(_) :-
	time_end,
	!.

time_cini(C) :-
	time_init,
	call(C),
	!.

time_cend(C) :-
	call(C),
	time_end,
	!.

true_fail(C) :-
	call(C),
	fail.
true_fail(_) :-
	!.

true_cini(C) :-
	call(C),
	!.

true_cend(C) :-
	call(C),
	!.


start2(Type) :-
	start2(_C, Type),
	fail.
start2(_Type) :-
	!.

start2(C, Type) :-
	calibrator(C, M),
% 	write('*** procesing '),
% 	write( C ),
% 	writeln( ' ***' ),
	start2_current(C, Type, M).

start2_current(C, Type, M) :-
	repetitions(Type, N, R),
	atom_number(NA, N),

	atom_concat(C,    '_',  C_),
	atom_concat(C_,   '0',  C_0),
	atom_concat(C_0,  '_',  C_0_),
	atom_concat(C_0_, Type, C_0_Type),
	measures(R, Type, M, C_0, C_0_Type),

	atom_concat(C_,   NA,   C_N),
	atom_concat(C_N,  '_',  C_N_),
	atom_concat(C_N_, Type, C_N_Type),
	measures(R, Type, M, C_N, C_N_Type),
	!.
