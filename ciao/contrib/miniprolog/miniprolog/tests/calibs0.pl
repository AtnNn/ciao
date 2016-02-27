% Stage 0 of calibration.  Expand the calibrators.

init_pred(prof, CalibName, N, Init) :-
	atom_number(AN, N),
	atom_concat('profile_init( ', CalibName,   I0),
	atom_concat(I0,               '_',         I1),
	atom_concat(I1,               AN,          I2),
	atom_concat(I2,               '_prof_c )', Init).
init_pred(time, _CalibName, _N, 'time_init').
init_pred(true, _CalibName, _N, 'true').

gen_calib(CalibName, N, Type) :-
	write_list_nl([CalibName, '_', N, ' :- ']),
	do_gen_c_body(CalibName, N, Type),
	!.

do_gen_c_body(CalibName, N, Type) :-
	init_pred(Type, CalibName, N, Init),
	gen_c_body(N, Init).

gen_calibrator(FileName, CalibName_N_Type) :-
	get_calibname_n_type(CalibName_N_Type, CalibName, N, Type),
	open_output(FileName, CO),
	gen_calib(CalibName, N, Type),
	close_output(CO).
