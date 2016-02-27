:- module(setperms, [
		get_perms/2,
		set_perms/2,
		set_exec_perms/2,
		mkdir_perm/2,
		convert_permissions/2,
		convert_permissions/4,
		execute_permissions/2,
		execute_permissions/4
	    ], [assertions]).

:- use_module(library(system)).
:- use_module(library(messages)).

get_perms(File, perm(User, Group, Others)) :-
	fmode(File, P),
	convert_permissions(User, Group, Others, P).

set_perms(Files, Perm) :-
	set_exec_perms_(Files, 0, _, Perm).

set_exec_perms(Files, Perm) :-
	set_exec_perms_(Files, Exec, Exec, Perm).

mkdir_perm(Dir, Perms) :-
	var(Perms),
	!,
	mkdir_mode(Dir, 0o777).
mkdir_perm(Dir, Perms) :-
	Perms = perm(User, Group, Others),
	execute_permissions(User, Group, Others, Exec),
	convert_permissions(User, Group, Others, Perm) ->
	Mode is Perm \/ Exec,
	mkdir_mode(Dir, Mode).

mkdir_mode(Dir, Mode) :-
	catch(make_dirpath(Dir, Mode),
	    Error,
	    (
		show_message(error, "Error calling ~w",
		    [make_dirpath(Dir, Mode)]),
		throw(Error)
	    )
	).

set_exec_perms_(Files, PermExec, Exec, perm(User, Group, Others)) :-
	(
	    execute_permissions(User, Group, Others, Exec),
	    convert_permissions(User, Group, Others, Perm) ->
	    set_perms_(Files, Perm \/ PermExec, Exec)
	;
	    error_message(
		"invalid permission (should be perm(User,Group,Others))", [])
	).

%% Files can have paths
set_perms_([], _Perm, _Exec) :-
	!.
set_perms_([File|Files], Perm, Exec) :-
	!,
	set_perms_(File,  Perm, Exec),
	set_perms_(Files, Perm, Exec).

set_perms_(File, Perm, Exec) :-
	set_perm(File, Perm, Exec).

set_perm(File, Perm, Exec) :-
	(
	    file_exists(File) ->
	    (
		file_property(File, mode(OrigMode)),
		(
		    file_property(File, type(directory)) ->
		    Mode is Perm \/ Exec
		;
		    Mode is Perm \/ (Exec /\ OrigMode)
		),
		(
		    Mode == OrigMode -> % same mode, do nothing
		    true
		;
		    chmod(File, Mode)
		)
	    )
	;
	    error_message("In set_perms/2, file '~w' not found", [File])
	).

execute_permissions(perm(U, G, O), E) :-
	execute_permissions(U, G, O, E).

execute_permissions(U, G, O, E) :-
	exec_mask_perm(U, NU),
	exec_mask_perm(G, NG),
	exec_mask_perm(O, NO),
	E is NU << 6 + NG << 3 + NO.

convert_permissions(perm(U, G, O), P) :-
	convert_permissions(U, G, O, P).

convert_permissions(U, G, O, P) :-
	valid_mode(U, NU),
	valid_mode(G, NG),
	valid_mode(O, NO),
	P is NU << 6 + NG << 3 + NO.

exec_mask_perm(''  , 0).
exec_mask_perm(x   , 0).
exec_mask_perm(w   , 0).
exec_mask_perm(wx  , 0).
exec_mask_perm(r   , 0).
exec_mask_perm(rx  , 0).
exec_mask_perm(rw  , 0).
exec_mask_perm(rwx , 0).
exec_mask_perm('X' , 1).
exec_mask_perm(wX  , 1).
exec_mask_perm(rX  , 1).
exec_mask_perm(rwX , 1).

valid_mode(''  , 0).
valid_mode(x   , 1).
valid_mode(w   , 2).
valid_mode(wx  , 3).
valid_mode(r   , 4).
valid_mode(rx  , 5).
valid_mode(rw  , 6).
valid_mode(rwx , 7).
valid_mode('X' , 0).
valid_mode(wX  , 2).
valid_mode(rX  , 4).
valid_mode(rwX , 6).
