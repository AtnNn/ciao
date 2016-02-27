:- module(tmpbased_common, [module_to_addressfile/2], []).

:- use_module(library(system)).

common_directory(TmpDir) :-
	get_tmp_dir(TmpDir). % Directory where address files are saved

module_to_addressfile(Mod, AddrPath) :-
        atom_concat(Mod,'_address',AddrFile),
        common_directory(Dir),
        atom_concat(Dir, AddrFile, AddrPath).
