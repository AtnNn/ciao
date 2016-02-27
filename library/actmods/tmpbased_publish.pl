:- module(tmpbased_publish, [], []).

:- use_module(library('actmods/tmpbased_common')).
:- use_module(library(filenames)).
:- use_module(library(streams)).
:- use_module(library(lists), [append/3]).
:- use_module(library(system)).


:- multifile save_addr_actmod/1.

save_addr_actmod(Address) :-
        current_executable(ExePath),
        atom_codes(ExePath, EXEPATH),
        no_path_file_name(EXEPATH, EXEFILE),
        ( file_name_extension(EXEFILE, MOD, _), ! ; MOD = EXEFILE ),
        atom_codes(Mod, MOD),
        module_to_addressfile(Mod, AddrPath),
        ( file_exists(AddrPath) -> delete_file(AddrPath) ; true ),
        get_pid(Pid),
        umask(OldUmask,0o077),
        open(AddrPath, write, ST),
        current_output(OldOut),
        set_output(ST),
        display_term(Address),
        display_term(pid(Pid)),
        set_output(OldOut),
        close(ST),
        umask(_, OldUmask).
