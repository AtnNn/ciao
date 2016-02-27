:- module(persdbcache,
	[ add_term_to_file/2, add_term_to_file_db/2,
	  delete_bak_if_no_ops/2, delete_file1/1,
	  get_pred_files/6, keyword/1, persistent/5 ],
	[ assertions ]).

:- use_module(library(terms), [atom_concat/2]).
:- use_module(library(lists)).
:- use_module(library(system)).
:- use_module(library(file_locks)).

:- data persistent/5. % F/A (modulo expanded) is persistent and uses files
                      % FILE_OPS, FILE and FILE_BAK

add_term_to_file_db(Term, Pred) :-
        functor(Pred, F, N),
        current_fact(persistent(F, N, File_ops, _, File_bak)), !,
        delete_bak_if_no_ops(File_ops, File_bak),
        add_term_to_file(Term, File_ops).
add_term_to_file_db(_Term, _Pred).

% This ensure that we not create an operations file in a transient state
delete_bak_if_no_ops(File_ops, File_bak) :-
        ( file_exists(File_ops) -> true
	; file_exists(File_bak) -> delete_file1(File_bak)
	; true).

delete_file1(File):-
	(file_exists(File)->
	 delete_file(File)
	;
	 true).

% add_term_to_file(Term,File) adds the term Term to a file File
add_term_to_file(Term,File) :-
        current_output(OldOutput),
        lock_file(File, FD, _),
        open(File,append,Stream),
        set_output(Stream),
        display_term(Term),
        close(Stream),
        unlock_file(FD, _),
        set_output(OldOutput).        

get_pred_files(Dir, Name, Arity, File, File_ops, File_bak):-
        add_final_slash(Dir, DIR),
        atom_codes(Name, NameString),
        append(Module,":"||PredName,NameString),
        atom_codes(Mod, Module),
        atom_concat(DIR, Mod, DirMod),
        create_dir(DirMod),
        number_codes(Arity, AS),
        append("/"||PredName, "_"||AS, FilePrefS),
        atom_codes(FilePref, FilePrefS),
        atom_concat(DirMod, FilePref, PathName),
        atom_concat(PathName, '.pl', File),
        atom_concat(PathName, '_ops.pl', File_ops),
        atom_concat(PathName, '_bak.pl', File_bak).

create_dir(Dir) :-
        file_exists(Dir), !.  % Assuming it's a directory
create_dir(Dir) :-
        make_dirpath(Dir, 0xfff).

add_final_slash(Dir, DIR) :-
        atom_concat(_,'/',Dir) -> DIR = Dir ; atom_concat(Dir, '/', DIR).

:- prop keyword(X) + regtype 
# "@var{X} is an atom corresponding to a directory identifier.".

keyword(X) :- atm(X).

